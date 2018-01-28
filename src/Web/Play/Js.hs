{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Web.Play.Js where

import Web.Play.Types
import Web.Play.Html

import           Control.Lens
import           Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Monoid
import           Web.Page.Js as Js
import           Web.Socket

jsButtons :: [CtrButton] -> JStat
jsButtons buttons = mconcat (Prelude.map mkJsButton buttons)

jsTexts :: [CtrText] -> JStat
jsTexts texts = mconcat (Prelude.map mkJsText texts)

jsSliders :: [CtrSlider] -> JStat
jsSliders sliders = mconcat (Prelude.map mkJsSlider sliders)

mkJsSlider :: CtrSlider -> JStat
mkJsSlider cs =
  let hashParamName = lit ("#param" ++ cs ^. csDomName)
      hashTextName = lit ("#text" ++ cs ^. csDomName)
  in
  [jmacro|
   $(`(hashParamName)`).change(function () {
         var !avalue = $(`(hashParamName)`).val();
         $(`(hashTextName)`).value=avalue;
         `(ApplStat (cs ^. csOnChange) [ref "avalue"])`;
   });
   $(`(hashTextName)`).change(function () { 
         var !avalue = $(`(hashTextName)`).val();
         $(`(hashParamName)`).value=avalue;
         `(ApplStat (cs ^. csOnChange) [ref "avalue"])`;
   });
  |]

mkJsButton :: CtrButton -> JStat
mkJsButton cb =
  let domName = cb ^. cbDomName
      onClick = cb ^. cbOnClick in
  [jmacro|
   $(`(domName)`).click( function() {
         `(onClick)`;
   });
  |]

mkJsText :: CtrText -> JStat
mkJsText cs =
  let hashTextName = "#text" ++ (cs^.ctDomName)
  in
  [jmacro|
   $(`(hashTextName)`).change(function () { 
         var !avalue = $(`(hashTextName)`).val();
         `(ApplStat (cs ^. ctOnChange) [ref "avalue"])`;
   });
  |]

jsSocket :: JStat
jsSocket = 
  [jmacro|
   play.makeSocket = function(wshost,wsport,update,onOpen,onClose) {
     var uri = 'ws://' + wshost + ':' + wsport + '/';
     var ws = new WebSocket(uri);
     ws.onopen = function(event) {
         console.log('open signal received from server');
         onOpen(event);
     };
     ws.onclose = function(event) {
         console.log('close signal received from server');
         onClose(event)
     };
     ws.onmessage = function(event) {
         update(event.data);
     };
     return ws;
   };
  |]

jsApplyPlay :: PlayState -> JStat
jsApplyPlay p0 =
  [jmacro|
   play.state = `((ref . C.unpack . encode) p0)`;
   play.update = function(e) {
       var d = JSON.parse(e);
       switch (d.tag) {
          case "PlayStateOut":
              play.state = d.contents;
              play.setPlayEffects();
              break;
          case "StreamOut":
              play.handleStream(d.contents);
              break;
          case "LogOut":
              console.log(d.contents);
              break;
          case "SocketOut":
              play.ws.close();
              break;
          default:
              console.log(d.contents);
       };
   };
   play.setPlayEffects = function() {
       if (play.state._pPlaying) {
           $('#btnGo').addClass("on");
           $('#btnStop').removeClass("on");
       } else {
           $('#btnGo').removeClass("on");
           $('#btnStop').addClass("on");
       };
       $("#paramSpeed").val(play.state._pSpeed);
       $("#textSpeed").val(play.state._pSpeed);
       $("#textFrame").val(play.state._pFrame);
       if (play.state._pTotalFrames === null) {
           $("#textTotalFrame").val("?");
       } else {
           $("#textTotalFrame").val(play.state._pTotalFrames);
       };
   };
   play.onOpen = function (e) {
       $('#spinner').addClass("icon-spin");
   };
   play.onClose = function(e) {
       $('#spinner').removeClass("icon-spin");
   };
  |]

jsPlaySetup :: String -> Int -> JStat
jsPlaySetup host' port' =
  [jmacro|
   play.setup = function() {
       play.ws = play.makeSocket(`(host')`,`(port')`,play.update, play.onOpen, play.onClose);
       play.setPlayEffects();
   };
  |]

jsPlayOnLoad :: JStat
jsPlayOnLoad =
  [jmacro|
   window.onload = play.setup();
  |]

jsStatementsPlay :: PlayState -> String -> Int -> JStat
jsStatementsPlay p port host =
  jsSocket <>
  jsApplyPlay p <>
  jsPlaySetup port host <>
  jsButtons buttonsPlay <>
  jsSliders (slidersPlay (p ^. pSpeed)) <>
  jsTexts (textsPlay 0)

jsGlobalPlay :: JStat
jsGlobalPlay = varObj "play"

jsOnloadPlay :: JStat -> PlayState -> SocketConfig -> JStat
jsOnloadPlay handleStream p sc =
  [jmacro|
       `(jsStatementsPlay p (sc^.cHost) (sc^.cPort))`;
       play.setup();
       `(handleStream)`;
  |]

