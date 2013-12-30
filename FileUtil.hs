module FileUtil (readAllFiles, readAllLines) where

import Control.Monad (forM)

readAllFiles :: [FilePath] -> IO [String]
readAllFiles files = do
	if length files > 0
	then do
		forM files $ \fpath -> do
			cont <- readFile fpath
			return cont
	else do
		cont <- getContents
		return $ [cont]

readAllLines :: [FilePath] -> IO [String]
readAllLines files = if length files > 0
	then do
		fmap concat $ forM files $ \fpath -> do
			cont <- readFile fpath
			return $ lines cont
	else do
		cont <- getContents
		return $ lines cont
