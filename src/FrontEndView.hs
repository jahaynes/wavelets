{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module FrontEndView where

import Prelude                              (($), (.))
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Text        (renderHtml)
import Text.Blaze.Html5.Attributes          (id, type_, src)
import Data.Text                            (Text)
import Data.Text.Lazy                       (toStrict)

mainPage :: Text
mainPage = toStrict . renderHtml $ page

    where
    page :: Html
    page = docTypeHtml $ do
            head $ do
                title "Welcome!"
                --link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
                script ! type_ "text/javascript" ! src "fayend.js" $ ""
            body $ do
                h1 "Greetings!"
                p $ "Hello "
                input ! id "prompt"
                p ! id "output" $ "Did you mean: "