module Main where

import           Control.Monad            (void)
import           Hydra.TUI.Options
import           Options.Applicative      (execParser, info)
import           TUI                      (run)

main :: IO ()
main = execParser (info parseOptions mempty) >>= void . TUI.run
