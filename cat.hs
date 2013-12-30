import System.Environment (getArgs)
import Control.Monad (forM)

main :: IO ()
main = do
	files <- getArgs
	conts <- readAllFiles files
	mapM_ putStr conts

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
