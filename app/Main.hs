{-# LANGUAGE OverloadedStrings #-}
module Main where

import DependencyGraph

import Turtle
import Data.Text

parser :: Parser Text
parser = argText "file" "Input file with dependency list"

main :: IO ()
main = do
    fname <- options "Dependency Graph Generator" parser
    graphFromFile (unpack fname) >>= putStrLn
