{-# LANGUAGE OverloadedStrings #-}

import Network.Connection
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy.Char8 as BS
 
main = do
    request <- parseRequest "https://service.nalog.ru/inn.do"
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request manager
    print $ (BS.take 100 $ responseBody res) `BS.append` (BS.pack " ...")
    print $ responseCookieJar res
