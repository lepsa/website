module Main where

import Website

main :: IO ()
main = startServer "db.sqlite" 8080
