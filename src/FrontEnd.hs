{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe
import Network.Wai.Middleware.Static    (staticPolicy, addBase)
import Data.Smashy.Types
import FrontEndView
import CheckSpelling
import Types
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy.Char8  as L8
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Data.Text as T

main :: IO ()
main = do
    
    hm <- loadTermMap
    rm <- loadReverseMap
    sm <- loadSpellingMap
    
    runSpock 8080 $
        spockT id $ do
            middleware . staticPolicy . addBase $ "static"
            spockApp hm rm sm

    where    
    spockApp :: HashMap Term TermInfo -> HashMap TermId Term -> HashMap Term [TermId] -> SpockT IO ()    
    spockApp hm rm sm = do
        
        get root $ html mainPage
                
        get ("spelling" <//> var) $ \t -> do
            let a = L8.pack t
            results <- liftIO $ T.pack . C8.unpack . C8.unwords <$> getSpelling hm rm sm (L8.pack t)
            text results