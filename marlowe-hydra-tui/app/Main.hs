{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad            (unless)
import           Data.Aeson               (decode)
import qualified Data.ByteString.Char8    as S
import qualified Data.ByteString.Lazy     as L
import           Data.Maybe               (fromMaybe)
import           Hydra.Cardano.Api
import           Hydra.Client
import           Hydra.ClientInput
import           Hydra.ContestationPeriod
import           Hydra.Ledger             (IsTx (..))
import           Hydra.Ledger.Cardano     (mkSimpleTx)
import           Hydra.ServerOutput
import           Hydra.TUI.Options
import           Options.Applicative      (execParser, info)

main :: IO ()
main = execParser (info parseOptions mempty) >>= run

run :: Options -> IO ()
run opt = withClient @Tx opt callback action

callback :: IsTx tx => HydraEvent tx -> IO ()
callback ClientConnected    = putStrLn "Client Connected"
callback ClientDisconnected = putStrLn "Client Disconnected"
callback (Update o)         = putStrLn "Update" >> print o
callback Tick               = putStrLn "Tick"

action :: IsTx tx => Client tx IO -> IO ()
action Client {..} = loop
  where
    loop = do
      line <- S.getLine
      unless (S.null line) $
        sendInput (fromMaybe Abort (decode $ L.fromStrict line)) >> loop
