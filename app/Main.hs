module Main where

import Website

main :: IO ()
main = startServer "./db/db.sqlite" 8080
