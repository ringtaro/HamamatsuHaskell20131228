import System.Environment (getArgs)
import FileUtil (readAllFiles)

main :: IO ()
main = do
	files <- getArgs
	conts <- readAllFiles files
	mapM_ putStr conts

