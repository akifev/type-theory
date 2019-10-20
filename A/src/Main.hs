module Main where
import Lexer
import Parser
import Grammar

main :: IO ()
main = do
    input <- getLine
    -- putStrLn input
    putStrLn (show (parser (alexScanTokens input)))
