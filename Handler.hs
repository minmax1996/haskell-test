module Handler (someFunc, handle) where

import qualified RollLib (printRoll)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

handle :: String -> IO()
handle ('g':'r':':':xs) = putStrLn $ RollLib.printRoll xs

