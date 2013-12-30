import System.Environment (getArgs)
import Data.List (isInfixOf)
import Control.Monad (forM_)
import FileUtil (readAllFiles)

main :: IO ()
main = do
	args <- getArgs
	if length args >= 1
	then do
		let
			target = head args
			files = tail args
		results <- getGrepResults target files
		displayGrepResult results
	else
		error "grep target [files]"

grep :: String -> String -> [String]
grep target cont = filter (isInfixOf target) (lines cont)

getGrepResults :: String -> [FilePath] -> IO [(FilePath, [String])]
getGrepResults target files = do
	conts <- readAllFiles files
	return $ zip files $ map (grep target) conts

displayGrepResult :: [(String, [String])] -> IO ()
displayGrepResult results = do
	let
		dispFilePath = length results > 1
	forM_ results $ \(fpath, result) -> do
		mapM_ putStrLn $ if dispFilePath then map ((fpath ++ ":") ++) result else result
