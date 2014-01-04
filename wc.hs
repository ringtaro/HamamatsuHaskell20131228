import System.Environment (getArgs)
import Control.Monad (when)
import FileUtil (readAllFileContents, FileContent)

main :: IO ()
main = do
	files <- getArgs
	wcn_list <- getWCNList files
	displayWCNList wcn_list

type WC = (Int, Int, Int)
type WCN = (String, WC)

plusWC :: WC -> WC -> WC
plusWC (c1, w1, l1) (c2, w2, l2) = (c1 + c2, w1 + w2, l1 + l2)

showWC :: WCN -> String
showWC (fn, (c, w, l)) = show l ++ "\t" ++ show w ++ "\t" ++ show c ++ "\t" ++ fn

getWCN :: FileContent -> WCN
getWCN (fpath, cont) =
	let
		ccount = length cont
		wcount = length $ words cont
		lcount = length $ lines cont
	in (fpath, (ccount, wcount, lcount))

getWCNList :: [FilePath] -> IO [WCN]
getWCNList files = (map getWCN) `fmap` readAllFileContents files

displayWCNList :: [WCN] -> IO ()
displayWCNList wcn_list = do
	mapM_ (\wcn -> putStrLn $ showWC wcn) wcn_list
	when (length wcn_list > 1) $ do
		let
			totalwc = foldr plusWC (0, 0, 0) $ map (\(_, wcl) -> wcl) wcn_list
		putStrLn $ showWC ("total", totalwc)
