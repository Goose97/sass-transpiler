module Main where

-- The transpile process consists of 2 phases
-- Parse phase: convert input file to AST. Result of this phase consists of:
-- 1) AST of SCSS
-- 2) mapping of mixins to theirs AST
-- 3) mapping of variables to theirs values
--
-- Serialize phase: serialize AST to CSS text. During this phase, mixin, variables, and SCSS functions will be evaluated

import qualified Parser
import qualified Serializer
import System.Environment

main :: IO ()
main = do
    [inputFile] <- getArgs
    content <- readFile inputFile
    case Parser.parse content of
        Left error -> print error
        Right sheet -> do
            let serialized = Serializer.serialize sheet
            putStr serialized
