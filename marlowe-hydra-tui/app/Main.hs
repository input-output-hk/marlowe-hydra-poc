module Main where

import           Control.Monad            (void)
import           Options.Applicative      (execParser, info)
import           TUI                      (run, parseExt)

main :: IO ()
main = execParser (info parseExt mempty) >>= void . TUI.run
