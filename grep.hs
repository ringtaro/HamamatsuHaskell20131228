import System.Environment (getArgs)
import Data.List (isInfixOf)
import Control.Monad (forM_)
import FileUtil (readAllFileContents)

main :: IO ()
main = do
	(target, files) <- getGrepArgs
	results <- getGrepResults target files
	displayGrepResult results

getGrepArgs :: IO (String, [FilePath])
getGrepArgs = do
	args <- getArgs
	if null args
	then
		error "grep target [files]"
	else do
		let
			target = head args
			files = tail args
		return (target, files)

grep :: String -> String -> [String]
grep target cont = filter (isInfixOf target) (lines cont)

getGrepResults :: String -> [FilePath] -> IO [(FilePath, [String])]
getGrepResults target files = (map (\(fpath, cont) -> (fpath, grep target cont))) `fmap` readAllFileContents files

displayGrepResult :: [(String, [String])] -> IO ()
displayGrepResult results = do
	let
		dispFilePath = length results > 1
	forM_ results $ \(fpath, result) -> do
		mapM_ putStrLn $ if dispFilePath then map ((fpath ++ ":") ++) result else result
