-- vim: set et ts=4 sw=4:
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import BasicPrelude  -- basic-prelude
import Data.Text (pack, unpack)  -- text
import Downloader
import System.Exit
import System.IO.Temp  -- temporary


main :: IO ()
main = do
    (track:artist:[]) <- getArgs
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
        putStrLn $ "Found MBID: " ++ mbid
        (detectedTrack, detectedArtists) <- titleArtistFromMBID mbid
        putStrLn $ "Track: " ++ detectedTrack
        putStrLn $ "Artists: " ++ tshow detectedArtists
        if (track == detectedTrack) && (artist `elem` detectedArtists)
        then putStrLn "Matched!"
        else putStrLn "Not matched, exiting." >> exitFailure
        nameMBIDs <- albumsFromMBID mbid
        (album, coverArt) <- firstAvailableCoverArt nameMBIDs
        mp3Filename <- convertAndTag resultFile track artist album coverArt
        putStrLn $ "Converted final result to: " ++ mp3Filename
