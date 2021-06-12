{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main
    ( main
    ) where

import           RIO

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Language.C.Inline.Context     as CC
import           Language.C.Inline.Cpp         as C
import           Language.C.Types

import           CRC


C.context (C.cppCtx <> C.bsCtx)

C.include "Slice.hpp"
C.include "CRC.hpp"


ccalcCRC :: ByteString -> IO Word16 
ccalcCRC bs = do 
  fromIntegral <$> [C.block| unsigned short {
    Util::const_slice<std::uint8_t> slice((const unsigned char*)$bs-ptr:bs, $bs-len:bs);
    return calcCRC(slice);    
  }|]


prop_checkCRC :: Property 
prop_checkCRC = property $ do 
  bs <- forAll $ Gen.bytes (Range.linear 1 65535)

  cchk <- liftIO $ ccalcCRC bs 
  let hchk = calcCRC bs 
  hchk === cchk


main :: IO Bool
main = checkParallel $$(discover) 
