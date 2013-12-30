import System.Environment (getArgs)
import FileUtil (readAllLines)

main :: IO ()
main = do
	args <- getArgs
	if length args > 0
	then do
		let
			no = read $ head args
			files = tail args
		lins <- readAllLines files
		mapM_ putStrLn $ map (cutAt no) lins
	else do
		error "cut no [files]"

cutAt :: Int -> String -> String
cutAt _ "" = []
cutAt no cs = tabwords cs !! no

tabwords :: String -> [String]
tabwords cs = bef : (if aft == "" then [] else tabwords $ tail aft)
	where
		bef = takeWhile (/= '\t') cs
		aft = dropWhile (/= '\t') cs

