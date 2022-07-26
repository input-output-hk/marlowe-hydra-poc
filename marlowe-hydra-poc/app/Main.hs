{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Marlowe.Extended.V1 as ME
import Marlowe.Contracts as C

partyA, partyB :: ME.Party
partyA = ME.Role "a"
partyB = ME.Role "b"

coin :: ME.Token
coin = ME.Token "" "coin"

main :: IO ()
main = do
  print $ C.swap partyA ME.ada (ME.Constant 1) (POSIXTime 10) partyB coin (ME.Constant 10) (POSIXTime 10) ME.Close

