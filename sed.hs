import System.Environment (getArgs)
import Text.Regex (Regex, mkRegex, subRegex)
import FileUtil (readAllFiles)

main :: IO ()
main = do
	(reg, rep, files) <- getSedArgs
	results <- getSubstResults reg rep files
	mapM_ putStr results

getSedArgs :: IO (Regex, String, [FilePath])
getSedArgs = do
	args <- getArgs
	if length args >= 2
	then do
		let
			reg = mkRegex (args !! 0)
			rep = args !! 1
			files = tail $ tail args
		return (reg, rep, files)
	else do
		error "sed reg rep [files]"

getSubstResults :: Regex -> String -> [FilePath] -> IO [String]
getSubstResults reg rep files = do
	conts <- readAllFiles files
	return $ map (\cont -> subRegex reg cont rep) conts
