import System.Environment (getArgs)
import Control.Monad (forM)

main :: IO ()
main = do
	args <- getArgs
	let
		no = read $ head args
		files = tail args
	lins <- readAllLines files
	mapM_ putStrLn $ map (cutAt no) lins

cutAt :: Int -> String -> String
cutAt _ "" = []
cutAt no cs = tabwords cs !! no

tabwords :: String -> [String]
tabwords cs = bef : (if aft == "" then [] else tabwords $ tail aft)
	where
		bef = takeWhile (/= '\t') cs
		aft = dropWhile (/= '\t') cs

readAllLines :: [FilePath] -> IO [String]
readAllLines files = if length files > 0
	then do
		fmap concat $ forM files $ \fpath -> do
			cont <- readFile fpath
			return $ lines cont
	else do
		cont <- getContents
		return $ lines cont
