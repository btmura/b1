import B1.Data.Price.Google

main = do
  stuff <- getGooglePrices "SPY"
  putStrLn $ "Prices: " ++ show stuff

