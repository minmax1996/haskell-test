module Main where

import qualified Handler (someFunc, handle)
import Control.Monad

main :: IO ()
main = do
  line <- getLine
  unless (line == "q") $ do
    Handler.handle line
    main