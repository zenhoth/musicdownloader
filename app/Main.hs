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

parser :: Parser (Text, Text)
parser = (,) <$> strOption (long "track" <> metavar "TRACKNAME" <> help "The name of the track") <*>
                 strOption (long "artist" <> metavar "ARTISTNAME" <> help "The name of the artist")

parserInfo :: ParserInfo (Text, Text)
parserInfo = info (parser <**> helper) (fullDesc <> progDesc "Downloader - download, convert and tag a song by track title and artist")

main :: IO ()
main = do
    (track, artist) <- execParser parserInfo
    url <- getLastFMURL track artist
    putStrLn $ "Found last.fm url: " ++ url
    videoURL <- getVideoURL url
    putStrLn $ "Found youtube video: " ++ videoURL
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
        mp3Filename <- convertAndTag resultFile track artist album coverArt
        putStrLn $ "Converted final result to: " ++ mp3Filename