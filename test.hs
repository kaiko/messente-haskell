import Messente

smsSend = send "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
main = do
  -- If from is Nothing then messente default 'From' is used (configured from API setup at messente.com)
  result <- smsSend Nothing "+00000000000" "my first sms"
  case result of
    Right id -> putStrLn ("sms sent, id: " ++ id)
    Left (errNo, errStr) -> putStrLn ("sms not sent, error " ++ show errNo ++ "happened: " ++ errStr)

  -- star http server to get delivery feedback (must configure at messente.com)
  -- this function doesn't return (runs forever)
  listen 9000 delivery

delivery :: Delivery -> IO ()
delivery del = do
  case del of
    Delivered id  -> putStrLn $ "delivered: " ++ id
    DeliveryError id errNo errStr -> putStrLn $ "not delivered: " ++ id ++ " (" ++ errStr ++ ")"
    DeliveryProgress id status    -> putStrLn $ "progress: "      ++ id ++ " (" ++ status ++ ")"

