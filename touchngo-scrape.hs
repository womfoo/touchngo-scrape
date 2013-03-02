import Web.TouchnGo
import Data.Time
import System.Locale

main = do
  accounts <- fmap read $ readFile "touchngo.cfg"
  mapM_ writeData (accounts :: [Account])

writeData account = do
  timestamp <- fmap (formatTime defaultTimeLocale "%Y%m%d.%H%M%S") getCurrentTime

  putStrLn $ "Retrieving data for " ++ userid account
  (mfgno,csvdata) <- scrapeTNG account
  let filename = mfgno ++ "-" ++ timestamp ++ ".csv"

  putStr $ "Writing " ++ filename
  writeFile filename $! csvdata
  putStr " (Done)\n"
