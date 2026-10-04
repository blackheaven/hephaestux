module Main (main) where

import qualified Hephaestux.Cli as Cli
import qualified Hephaestux.Commands as Commands

main :: IO ()
main = Cli.parseOptions >>= Commands.run
