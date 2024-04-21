{-# LANGUAGE OverloadedStrings #-}
module External where

import Network.Socket
import Network.Socket.ByteString qualified as B
import Data.ByteString qualified as B
import Data.List.Split (splitOn)
import Data.Foldable
import Text.Printf
import Control.Lens hiding (Empty)
import Data.Function
import Data.String
import Data.Functor
import Debug.Trace

import Game

run :: PortNumber -> String-> IO ()
run port ip = do
  sock <- socket AF_INET Stream defaultProtocol
  setSockOpt sock ReuseAddr (1 :: Int)
  bind sock (SockAddrInet port (ipAs ip))
  listen sock 1
  (csock, _) <- accept sock
  let
    b = defaultBoard & coordAction .~ (toTuple . map (read :: String -> Int) . debug . words . debug . byteStrToStr <$> B.recv csock 1024)
                     & printAction .~ ((() <$) . B.send csock . fromString)
  gameLoop b
  mapM_ close [csock, sock]

ipAs :: Num a => String -> a
ipAs = fromInteger . sum . zipWith (\i x -> read x * 2^i) [0,8..] . splitOn "."

toTuple :: [a] -> (a, a)
toTuple (x:y:_) = (x, y)

debug x = traceShow x x 

byteStrToStr = map (toEnum . fromIntegral) . B.unpack
