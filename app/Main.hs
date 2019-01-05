-- vim: set et ts=4 sw=4:
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import BasicPrelude  -- basic-prelude
import Data.Text (pack)  -- text
import Downloader
import Options.Applicative
import System.Exit
import System.IO.Temp  -- temporary
import System.IO (hFlush, stdout)

parser :: Parser (Text, Text, Maybe Text)
parser = (,,) <$> strOption (long "track" <> metavar "TRACKNAME" <> help "The name of the track") <*>
                 strOption (long "artist" <> metavar "ARTISTNAME" <> help "The name of the artist") <*>
                 (optional $ strOption (long "video" <> metavar "URL" <> help "Skip the last.fm step and use this video url instead"))

parserInfo :: ParserInfo (Text, Text, Maybe Text)
parserInfo = info (parser <**> helper) (fullDesc <> progDesc "Downloader - download, convert and tag a song by track title and artist")

main :: IO ()
main = do
    (track, artist, suppliedVideoURL) <- execParser parserInfo
    let useProvidedVideo = suppliedVideoURL >>= \u -> return (putStrLn ("Using provided video: " ++ u) >> return u)
    let findOurOwnVideo = do
            url <- getLastFMURL track artist
            putStrLn $ "Found last.fm url: " ++ url
            videoURL <- getVideoURL url
            putStrLn $ "Found youtube video: " ++ videoURL
            return videoURL
    videoURL <- flip fromMaybe useProvidedVideo findOurOwnVideo
    withSystemTempDirectory "musicdownloader" $ \tempDirectory -> do
        resultFile <- downloadAudio videoURL (pack tempDirectory)
        putStrLn $ "Downloaded temporary audio to: " ++ resultFile
        (fingerprint, duration) <- fingerprintFile resultFile
        putStrLn "Scanned fingerprint"
        mbid <- mbidFromAcoustIDFingerprint fingerprint duration
        putStrLn $ "Found recording MBID: " ++ mbid
        (detectedTrack, detectedArtists) <- titleArtistFromMBID mbid
        putStrLn $ "Track: " ++ detectedTrack
        putStrLn $ "Artists: " ++ intercalate ", " detectedArtists
        nameMBIDs <- albumsFromMBID mbid
        (album, coverArt) <- firstAvailableCoverArt nameMBIDs
        putStrLn $ "Album: " ++ album
        if (track == detectedTrack) && (artist `elem` detectedArtists)
        then putStrLn "Matched track and artist"
        else do
            putStr "Mismatching track/artist. Continue anyway? (y/n) "
            hFlush stdout
            response <- getChar
            case response of
                'y' -> return ()
                _ -> exitFailure
        mp3Filename <- convertAndTag resultFile detectedTrack (head detectedArtists) album coverArt
        putStrLn $ "Converted final result to: " ++ mp3Filename