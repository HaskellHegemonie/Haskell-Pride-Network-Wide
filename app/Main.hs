{-# LANGUAGE RecordWildCards #-}
module Main where
import Options.Applicative
import External
import Network.Socket

data Sample = Sample
        { port :: PortNumber
        , ip :: String
        }

main :: IO ()
main = do
  (Sample{..}) <- execParser opts
  run port ip


opts :: ParserInfo Sample
opts = info sample mempty
sample :: Parser Sample
sample = Sample
  <$> option auto (mconcat
                  [ long "port"
                  , short 'p'
                  , metavar "PORT"
                  , help "port to bind"
                  ])
  <*> strOption (mconcat
                [ long "ip"
                , help "ip address to listen to"
                , metavar "IP"
                , value "127.0.0.1"
                ])
