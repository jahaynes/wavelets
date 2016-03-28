{-# LANGUAGE EmptyDataDecls #-}

module Hello where

import FFI

data Event
data Element

main :: Fay ()
main = addWindowEvent "load" onLoad

    where
    addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
    addWindowEvent = ffi "window.addEventListener(%1, %2)"

    onLoad :: Event -> Fay()
    onLoad event =
        getElementById "prompt" >>= \inputBox -> 
            addEventListener inputBox "onkeyup" $ \e -> do
                unsureSpelling <- getValue inputBox
                when (length unsureSpelling > 2) $ ahah ("http://127.0.0.1:8080/spelling/" ++ unsureSpelling) "prompt" "output"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.body.innerHTML = %1"

setHtml :: Element -> String -> Fay ()
setHtml = ffi "%1.innerHTML = %2"

getValue :: Element -> Fay String
getValue = ffi "%1.value"

addEventListener :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"        
        
addRequestListener :: Request -> String -> (Event -> Fay ()) -> Fay ()
addRequestListener = ffi "%1[%2] = %3"           
        
getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"        

getResponseText :: Request -> Fay String
getResponseText = ffi "%1.responseText"

data Request

ahah url sourceId targetId = do
    source <- getElementById sourceId
    target <- getElementById targetId

    req <- createRequest
    addRequestListener req "onreadystatechange" $ \_ -> do
        responseText <- getResponseText req
        setHtml target ("Did you mean: " ++ responseText)

    openRequest req url
    sendReq req ""

    where
    createRequest :: Fay Request
    createRequest = ffi "new XMLHttpRequest()"        

    openRequest :: Request -> String -> Fay ()
    openRequest = ffi "%1.open(\"GET\", %2, true)" 

    sendReq :: Request -> String -> Fay ()
    sendReq = ffi "%1.send(%2)"

{-
function ahah(url, target) {

  req = new XMLHttpRequest();

  if (req != undefined) {
    req.onreadystatechange = function() {ahahDone(url, target);};
    req.open("GET", url, true);
    req.send("");
  }
}  

function ahahDone(url, target) {
  if (req.readyState == 4) { // only if req is "loaded"
    if (req.status == 200) { // only if "OK"
      document.getElementById(target).innerHTML = req.responseText;
    } else {
      document.getElementById(target).innerHTML=" AHAH Error:\n"+ req.status + "\n" +req.statusText;
    }
  }
}

-}