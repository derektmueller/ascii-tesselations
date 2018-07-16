module Main where

import Lib

main :: IO ()
main = do
  mapM_ putStrLn $ tesselate 4 2 Ninety [
    "####", 
    "#--#", 
    "#++#", 
    "####"]
  mapM_ putStrLn $ tesselate 4 2 Ninety [
    "/\\-/|-",
    "/\\/-\\/",
    "||\\\\-\\",
    "|\\|-|/",
    "|-\\|/|",
    "|\\-/-\\"]
  mapM_ putStrLn $ tesselate 10 6 Ninety [
    "^^>.",
    "^<.<",
    ">.>v",
    ".<vv"]

