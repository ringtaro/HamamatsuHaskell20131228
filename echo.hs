import System.Environment (getArgs)

main :: IO ()
--main = do args <- getArgs ; putStrLn $ unwords args
--main = fmap unwords getArgs >>= putStrLn
main = putStrLn =<< unwords `fmap` getArgs