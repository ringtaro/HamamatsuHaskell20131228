module FileUtil (readAllFiles, readAllFileContents, readAllLines, FileContent) where

import Control.Monad (forM)

type FileContent = (FilePath, String)

readAllFiles :: [FilePath] -> IO [String]
readAllFiles []    = (:[]) `fmap` getContents 
readAllFiles files = forM files $ \fpath -> readFile fpath

readAllFileContents :: [FilePath] -> IO ([FileContent])
readAllFileContents []    = (:[]) `fmap` ((,) "") `fmap` getContents 
readAllFileContents files = forM files $ \fpath -> ((,) fpath) `fmap` readFile fpath

readAllLines :: [FilePath] -> IO [String]
readAllLines [] = lines `fmap` getContents
readAllLines files = lines `fmap` concat `fmap` readAllFiles files
