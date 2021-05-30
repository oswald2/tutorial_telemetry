module MyLib (someFunc) where

import RIO
import qualified Data.Text.IO                  as T


someFunc :: IO ()
someFunc = T.putStrLn "someFunc"
