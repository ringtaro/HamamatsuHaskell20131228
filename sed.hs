import System.Environment (getArgs)
import Text.Regex (Regex, mkRegex, subRegex)
import Control.Monad (forM)

main :: IO ()
main = do
	args <- getArgs
	if length args >= 2
	then do
		let
			reg = mkRegex (args !! 0)
			rep = args !! 1
			files = tail $ tail args
		results <- getSubstResults reg rep files
		mapM_ putStr results
	else do
		error "sed reg rep [files]"

getSubstResults :: Regex -> String -> [FilePath] -> IO [String]
getSubstResults reg rep files = do
	conts <- readAllFiles files
	return $ map (\cont -> subRegex reg cont rep) conts

readAllFiles :: [FilePath] -> IO [String]
readAllFiles []    = (:[]) `fmap` getContents 
readAllFiles files = forM files $ \fpath -> readFile fpath
