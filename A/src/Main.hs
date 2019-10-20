module Main where
import Lexer
import Parser
import Grammar

main :: IO ()
main = do
    input <- getContents
    let str =  unlines $ lines input
    putStrLn (show (parser (alexScanTokens str)))
