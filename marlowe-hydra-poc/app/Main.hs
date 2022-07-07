module Main where

import Language.Marlowe.Core.V1.Semantics.Types as MS
import Language.Marlowe.Core.V1.Semantics as MS
import Marlowe.Contracts as C

partyA, partyB :: MS.Party
partyA = MS.Role "a"
partyB = MS.Role "b"

coin :: MS.Token
coin = MS.Token "" "coin"

main :: IO ()
main = do
  putStrLn $ show $ C.swap partyA MS.ada (MS.Constant 1) (POSIXTime 10) partyB coin (MS.Constant 10) (POSIXTime 10) MS.Close
