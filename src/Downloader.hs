-- vim: set et st=4 sw=4:
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Downloader where
import BasicPrelude  -- basic-prelude
import Control.Exception.Safe -- safe-exceptions
import Control.Lens  -- lens
import Data.Aeson.Lens  -- lens-aeson
import Data.Text (Text, dropEnd, takeEnd, pack, splitOn, unpack)  -- text
import Network.HTTP.Simple  -- http-conduit
import Text.XML.HXT.Core  -- hxt
import Text.HandsomeSoup  -- HandsomeSoup
import Safe hiding (readMay) -- safe
import System.IO.Temp  -- temporary
import System.Environment (getEnv)
import System.Exit
import System.Process.Text
import qualified Data.ByteString as BS  -- bytestring
import qualified Data.Map as M
import qualified Lastfm  -- liblastfm
import qualified Lastfm.Request as Request  -- liblastfm
import qualified Lastfm.Track as Track  -- liblastfm

-- the error handling methodology I've implemented is MonadThrow (https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell)
-- specifically, throw exceptions in IO in lieu of doing IO Either or IO Maybe
-- although I don't define any exception types, since I don't intend to actually catch anything in this once-off utility

-- the lastfm library uses Either Exception to signal errors, which can be adapted to our approach
-- throw the Left, or just return the Right
throwifyL :: MonadThrow m => Either Lastfm.LastfmError a -> m a
throwifyL = either throw return

-- throw the given string if Nothing, else return the Just
throwifyM :: MonadThrow m => Text -> Maybe a -> m a
throwifyM s = maybe (throwText s) return

throwText :: MonadThrow m => Text -> m a
throwText = throwString.unpack

getAPIKey :: IO Text
getAPIKey = pack <$> getEnv "LAST_FM_API_KEY"

getLastFMURL :: Text -> Text -> IO Text
getLastFMURL track artist = do
    apiKey <- getAPIKey
    let request = Track.getInfo
                  <*> Request.artist artist
                  <*> Request.track track
                  <*> Lastfm.apiKey apiKey
                  <* Lastfm.json
    connection <- Lastfm.newConnection
    json <- join $ map throwifyL $ Lastfm.lastfm connection request
    throwifyM "Incorrect JSON" $ preview (key "track" . key "url" . _String) json

getVideoURL :: Text -> IO Text
getVideoURL lastfmURL = do
    response <- map (unpack.decodeUtf8.getResponseBody) $ parseRequest ("GET " ++ unpack lastfmURL) >>= httpBS
    let doc = readString [withParseHTML yes, withWarnings no] response
    relativeURLs <- runX $ doc >>> css "a" >>> hasAttrValue "class" (isInfixOf "image-overlay-playlink-link") >>> getAttrValue "href"
    videoURL <-  throwifyM "No video URLs found on last.fm page" $ headMay $ nub relativeURLs
    return $ "https://www.youtube.com/watch?v=" ++ (takeEnd 11 $ pack videoURL)

downloadAudio :: Text -> Text -> IO Text
downloadAudio url targetDirectory = do
    let outputTemplate = targetDirectory ++ "/%(title)s.%(ext)s"
    let args = ["-f", "bestaudio", "-o", outputTemplate, url]
    _ <- readProcess "youtube-dl" args ""
    map (dropEnd 1) $ readProcess "youtube-dl" ("--get-filename":args) ""

readProcess :: FilePath -> [Text] -> Text -> IO Text
readProcess file args stdin = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode file (map unpack args) stdin
    if exitCode == ExitSuccess
    then return stdout
    else throwText $ pack file ++ " " ++ unwords args ++ " failed with code " ++ tshow exitCode ++ " and stderr:\n" ++ stderr


fingerprintFile :: Text -> IO (Text, Int)
fingerprintFile trackFilename = do
    withSystemTempFile "musicdownloader.wav" $ \wavFilenameS _ -> do
        let wavFilename = pack wavFilenameS
        _ <- readProcess "ffmpeg" ["-loglevel", "warning", "-y", "-i", trackFilename, wavFilename] ""
        fpcalcOutput <- readProcess "fpcalc" [wavFilename] ""
        parseFpcalcOutput fpcalcOutput

parseFpcalcOutput :: MonadThrow m => Text -> m (Text, Int)
parseFpcalcOutput output = throwifyM "Failed to parse the fpcalc output" $ do
    let arrToTuple xs = do
            a <- xs `atMay` 0  -- !! that returns Maybe a
            b <- xs `atMay` 1
            return (a, b)
    result <- map M.fromList $ mapM (arrToTuple . splitOn "=") $ lines output
    fingerprint <- result M.!? "FINGERPRINT"
    duration <- result M.!? "DURATION" >>= readMay :: Maybe Int
    return (fingerprint, duration)

getAcoustIDAPIKey :: IO Text
getAcoustIDAPIKey = pack <$> getEnv "ACOUSTID_API_KEY"

mbidFromAcoustIDFingerprint :: Text -> Int -> IO Text
mbidFromAcoustIDFingerprint fingerprint duration = do
    apiKey <- getAcoustIDAPIKey
    --TODO compress requests? AcoustID prefers it, although we won't exactly be making a lot of calls
    let url = "https://api.acoustid.org/v2/lookup?client=" ++ apiKey ++ "&duration=" ++ tshow duration ++ "&fingerprint=" ++ fingerprint ++ "&meta=recordingids"
    response <- map getResponseBody $ parseRequest ("POST " ++ unpack url) >>= httpBS
    throwifyM "Couldn't parse AcoustID response for MBID" $ preview (key "results" . nth 0 . key "recordings" . nth 0 . key "id". _String) $ decodeUtf8 response

addRequestQueryString :: (ByteString, Maybe ByteString) -> Request -> Request
addRequestQueryString qi r = setRequestQueryString (qi:(getRequestQueryString r)) r

musicBrainzRequest :: Request
musicBrainzRequest = setRequestHost "musicbrainz.org" $
                     setRequestPort 443 $
                     setRequestSecure True $
                     setRequestMethod "GET" $
                     addRequestQueryString ("fmt", Just "json") $
                     addRequestHeader "User-Agent" "Musicdownloader/0.1" $
                     defaultRequest


titleArtistFromMBID :: Text -> IO (Text, [Text])
titleArtistFromMBID mbid = do
    let request = setRequestPath ("/ws/2/recording/" ++ encodeUtf8 mbid) $
                  addRequestQueryString (encodeUtf8 "inc", Just $ encodeUtf8 "artists") $
                  musicBrainzRequest
    response <- map (decodeUtf8 . getResponseBody) $ httpBS request
    title <- throwifyM "Couldn't find the title in the MusicBrainz response" $ preview (key "title" . _String) response
    let artists = catMaybes $ toListOf (key "artist-credit" . _Array . traverse . to (^? (key "artist" . key "name"._String))) response
    return (title, artists)

albumsFromMBID :: Text -> IO [(Text, Text)]
albumsFromMBID recordingMBID = do
    let request = setRequestPath "/ws/2/release" $
                  addRequestQueryString (encodeUtf8 "recording", Just $ encodeUtf8 recordingMBID) $
                  musicBrainzRequest
    response <- map (decodeUtf8 . getResponseBody) $ httpBS request
    let processRelease release = do
            title <- release ^? key "title" . _String
            releaseMBID <- release ^? key "id" . _String
            date <- release ^? key "date" . _String
            hasArt <- release ^? key "cover-art-archive" . key "artwork" . _Bool
            artIsFrontArt <- release ^? key "cover-art-archive" . key "front" . _Bool
            return (title, releaseMBID, date, hasArt && artIsFrontArt)
    let releases = catMaybes $ toListOf (key "releases" . _Array . traverse . to processRelease) response
    return $ map (\(name, releaseId, _, _) -> (name, releaseId)) $ sortOn (\(_, _, date, _) -> date) $ filter (\(_, _, _, hasArt) -> hasArt) releases

getCoverArt :: Text -> IO ByteString
getCoverArt releaseId = map getResponseBody $ httpBS $ setRequestHost "coverartarchive.org" $
                                                       setRequestMethod "GET" $
                                                       setRequestPath ("/release/" ++ encodeUtf8 releaseId ++ "/front") $
                                                       defaultRequest

-- try a MonadCatch, and if it fails, try the next one, else return the result
-- this is, in a sense, the opposite of >>, because we short-circuit on success, and keep going on failure
untilSuccess :: MonadCatch m => m a -> m a -> m a
untilSuccess a b = catchAny a (const b)

firstAvailableCoverArt :: [(Text, Text)] -> IO (Text, ByteString)
firstAvailableCoverArt nameMBIDs = do
    let processPair (name, mbid) = do
            artBS <- getCoverArt mbid
            return (name, artBS)
    -- foldr, because untilSuccess will eventually not evaluate the second argument, allowing us to short-circuit
    foldr untilSuccess (throwString "No albums with cover art found") $ map processPair nameMBIDs

convertAndTag :: Text -> Text -> Text -> Text -> ByteString -> IO Text
convertAndTag trackFilename title artist album coverArt = do
    let resultFilename = artist ++ " - " ++ title ++ ".mp3"
    withSystemTempFile "musicdownloader" $ \tempArtFilename tempArtHandle -> do
            BS.hPut tempArtHandle coverArt
            _ <- readProcess "ffmpeg" ["-i", trackFilename,
                                       "-i", pack tempArtFilename,
                                       "-map", "0:0",
                                       "-map", "1:0",
                                       "-b:0", "320K",
                                       "-codec", "copy",
                                       "-acodec", "mp3",
                                       "-id3v2_version", "3",
                                       "-metadata:s:v", "title=Album cover",
                                       "-metadata:s:v", "comment=Cover (front)",
                                       "-metadata", "title=" ++ title,
                                       "-metadata", "album=" ++ album,
                                       "-metadata", "artist=" ++ artist,
                                       resultFilename] ""
            return ()
    return resultFilename
