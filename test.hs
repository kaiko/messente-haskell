import Messente

smsSend = send "api-username" "api-password"
main = do
  -- If from is Nothing then messente default 'From' is used
  -- (configured from API setup at messente.com)
  result <- smsSend Nothing "+00000000000" "my first sms"
  putStrLn $ case result of
    Right id -> "sms sent, id: " ++ id
    Left (errNo, errStr) -> "not sent: " ++ show errNo ++ ", " ++ errStr

  -- star http server to get delivery feedback (must configure at messente.com)
  -- this function doesn't return (runs forever)
  listen 9000 delivery

delivery :: Delivery -> IO ()
delivery del = putStrLn $
  case del of
    Delivered id  -> "delivered " ++ id
    DeliveryError id errNo errStr -> "not delivered " ++ id ++ ": " ++ errStr
    DeliveryProgress id status    -> "progress "      ++ id ++ ": " ++ status

