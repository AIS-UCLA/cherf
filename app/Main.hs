module Main where

import Client (client)
import Server (server)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("client" : t) = client t
parse ("server" : t) = server t
parse _ = usage

usage :: IO ()
usage = putStrLn "usage: cherf <client|server>"
