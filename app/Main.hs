module Main where
import System.Environment (getArgs)
import Server (server)
import Client (client)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("client":t) = client t
parse ("server":t) = server t
parse _            = usage

usage :: IO ()
usage = putStrLn "usage: cherf <client|server>"

