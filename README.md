# messente-haskell

Non-official [Messente.com](https://messente.com/) SMS gateway API wrapper for Haskell.

API documentation: [https://messente.com/documentation/setup-and-activation](https://messente.com/documentation/setup-and-activation)


Features:
  * Only https is used. 
  * Uses backup server automatically.
  * Delivery report server included.

Missing features:
  * No credits API
  * No pricing API
  * Can't provide special parameters for sms like time_to_send, validity, dlr-url etc.

## Usage

Example code sends sms and waits for delivery raport (doesn't exit).
To get delivery raport, you must configure it from http://www.messente.com/ API setup.

    import Messente
    
    smsSend = send "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
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
        Delivered id  -> "delivered: " ++ id
        DeliveryError id errNo errStr -> "not delivered: " ++ id ++ " (" ++ errStr ++ ")"
        DeliveryProgress id status    -> "progress: "      ++ id ++ " (" ++ status ++ ")"

