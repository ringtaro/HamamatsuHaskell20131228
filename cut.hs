import System.Environment (getArgs)
import FileUtil (readAllLines)

main :: IO ()
main = do
	(no, files) <- getCutArgs
	lins <- readAllLines files
	mapM_ putStrLn $ map (cutAt no) lins

getCutArgs :: IO (Int, [FilePath])
getCutArgs = do
	args <- getArgs
	if null args
	then do
		error "cut no [files]"
	else do
		let
			no = read $ head args
			files = tail args
		return (no, files)

cutAt :: Int -> String -> String
cutAt _ "" = []
cutAt no cs = tabwords cs !! no

tabwords :: String -> [String]
tabwords cs = bef : (if aft == "" then [] else tabwords $ tail aft)
	where
		bef = takeWhile (/= '\t') cs
		aft = dropWhile (/= '\t') cs

