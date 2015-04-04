{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-|
Module      : Messente
Description : Messente SMS Gateway wrapper library
License     : MIT
Maintainer  : kaiko@zazler.com

Example

@
import Messente

smsSend = send \"api-username\" \"api-password\"
main = do
  result <- smsSend Nothing \"+00000000000\" \"my first sms\"
  putStrLn $ case result of
    Right id -> \"sms sent, id: \" ++ id
    Left (errNo, errStr) -> \"not sent: \" ++ show errNo ++ \", \" ++ errStr

  listen 9000 delivery

delivery :: Delivery -> IO ()
delivery del = putStrLn $
  case del of
    Delivered id  -> \"delivered \" ++ id
    DeliveryError id errNo errStr -> \"not delivered \" ++ id ++ \": \" ++ errStr
    DeliveryProgress id status    -> \"progress \"      ++ id ++ \": \" ++ status
@
-}
module Messente
  ( Delivery(..)
  , MessenteError(..)
  , Messente.send
  , Messente.listen
  , Messente.verify
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import Data.Typeable
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Network hiding (accept, sClose)
-- Conduit is use to get https working
import Network.HTTP.Conduit
import Network.HTTP (urlDecode, urlEncode)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent
import Control.Exception

servers = [ "api2.messente.com", "api3.messente.com" ]

-- |Messente SMS textual id 
type SmsID = String

-- |Exceptions for Messente API
data MessenteError
  = WrongCredentials
  | InvalidIP
  | InvalidParameters [(String, String)]
  | InvalidSender String
  | MissingPin
  | ServersDown           -- ^ All servers gave @FAILED 209@
  | Unknown String        -- ^ Just in case. Argument contains http response as it is.
  deriving (Typeable)

instance Exception MessenteError
instance Show MessenteError where
  show WrongCredentials      = "Messente got wrong credentials"
  show InvalidIP             = "This IP is not whitelisted for Messente API"
  show MissingPin            = "PIN code field is missing in the template value"
  show ServersDown           = "All servers answered with \"FAILED 209\""
  show (InvalidParameters p) = "Messente got invalid parameters: " ++ makeQuery p
  show (InvalidSender s    ) = "You must register this sender from Messente API (" ++ s ++ ")"
  show (Unknown       s    ) = "Unknown Messente error: " ++ s

data Delivery
  = Delivered SmsID                 -- ^ Happy final state
  | DeliveryError SmsID Int String  -- ^ Negative final state
  | DeliveryProgress SmsID String   -- ^ Not final state. Informative report.
  deriving (Show)

-- |Sends sms and returns Either error or sms id
-- It takes arguments 'apiUser' 'apiPassword' 'from' 'to' 'content'
send :: String -> String -> Maybe String -> String -> String -> IO (Either (Int, String) SmsID)
send apiUser apiPassword from to content = 
  do resp <- doRequest servers $ "/send_sms/?" ++ makeQuery q
     case BL.unpack resp of
       ('O':'K':' ':inf) -> return $ Right $ takeWhile (/=' ') inf
       "ERROR 102"  -> return $ Left (102, "Invalid parameters") -- Left $ InvalidParameters q -- NOTE: can't make difference if problem is with API or this SMS! :(
       "ERROR 111"  -> throw $ InvalidSender $ fromMaybe "unspecified" from
       r            -> throw $ Unknown r
  where
    q  = [ ("username", apiUser)
         , ("password", apiPassword)
         , ("text",     content)
         , ("to",       to)
         ] ++ maybe [] (\f -> [("from", f)]) from

-- |Listens delivery reports
-- Takes port number and callback function as arguments.
listen :: Int -> (Delivery -> IO ()) -> IO ()
listen port fn =
  withSocketsDo $
    do sock <- listenOn $ PortNumber (fromIntegral port)
       loop sock
  where
    loop sock = do
      (conn, _) <- accept sock
      forkIO $ do
        mesg <- recv conn 9999
        sendAll conn (B.pack okResponse)
        sClose conn
        fn $ parseDeliveryReport mesg
      loop sock
    okResponse = "HTTP/1.0 200 OK"
              ++ "\r\nContent-Type: text/plain"
              ++ "\r\nServer: Messente Haskell Library 0.1" -- TODO version
              ++ "\r\nConnection: close"
              ++ "\r\n\r\n"

-- |Verify SMS delivery.
-- Takes apiUser apiPassword smsId
verify :: String -> String -> SmsID -> IO Delivery
verify apiUser apiPassword id =
  do resp <- doRequest servers ("/get_dlr_response/?" ++ makeQuery q)
     case resp of
       "OK DELIVERED" -> return $ Delivered id
       "OK SENT"      -> return $ DeliveryProgress id   "Unknown"
       "OK FAILED"    -> return $ DeliveryError    id 1 "Unknown"
       "ERROR 102"  -> throw $ InvalidParameters q
       "ERROR 111"  -> throw $ InvalidSender "unknown"
       "ERROR 109"  -> throw   MissingPin
       r            -> throw $ Unknown $ BL.unpack r
  where
    q = [ ("username",    apiUser    )
        , ("password",    apiPassword)
        , ("sms_uniq_id", id         ) ]

------------

doRequest :: [String] -> String -> IO BL.ByteString
doRequest [] _ = throw ServersDown
doRequest (serv:bserv) url = handle prob $ simpleHttp ("https://" ++ serv ++ url) >>= chk
  where
    chk :: BL.ByteString -> IO BL.ByteString
    chk "FAILED 209" = doRequest bserv url
    chk "ERROR 101"  = throw WrongCredentials
    chk "ERROR 103"  = throw InvalidIP
    chk s = return s
    prob :: SomeException -> IO BL.ByteString
    prob e = if null bserv 
              then throw e
              else doRequest bserv url

parseDeliveryReport :: B.ByteString -> Delivery
parseDeliveryReport httpMesg =
  status (val "sms_unique_id") (val "status") (val' "stat") (val' "err")
  where
    val key = case lookup key args of 
                Just v -> B.unpack v
                _ -> error ("Parameter " ++ B.unpack key ++ " missing from Messente Delivery Report request")
    val' key = fmap B.unpack $ lookup key args
    status id "DELIVERED" _ _ = Delivered id
    status id "SENT"      (Just s) _ = DeliveryProgress id s
    status id "FAILED"    (Just s) (Just code) = DeliveryError id (read code) s
    status id s1 s2 err = error $ "Unknown Messente status: " ++ s1 ++ " " ++ show s2 ++  " " ++ show err
    args = parseQuery $ B.takeWhile (/=' ') $ B.tail $ B.dropWhile (/='?') httpMesg -- "GET /?(.*?) "

makeQuery :: [(String, String)] -> String
makeQuery args = intercalate "&" $ map (\(k,v) -> k ++ "=" ++ urlEncode v) args

parseQuery :: B.ByteString -> [(B.ByteString, B.ByteString)]
parseQuery "" = []
parseQuery query = 
    let (x, xs) = breakC '&' query
    in breakC '=' x : parseQuery xs
  where
    breakC c bs = let (a,b) = B.break (==c) bs in (a, B.drop 1 b)

