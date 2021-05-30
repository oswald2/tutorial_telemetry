module Main where

import qualified MyLib                          ( someFunc )
import           RIO

import qualified Data.Text.IO                  as T


main :: IO ()
main = do
    T.putStrLn "Hello, Haskell!"
    MyLib.someFunc
