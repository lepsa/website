module Main where

import           Prelude (IO)
import           Website (startServer)

main :: IO ()
main = startServer "./db/db.sqlite" 8080
