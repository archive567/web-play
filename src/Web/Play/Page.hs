{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Web.Play.Page where

import Web.Play.Css
import Web.Play.Js
import Web.Play.Types
import Web.Play.Html

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Text (Text, pack)
import Web.Page.Js as Js
import Web.Page.Types

play :: Page
play = playWith def

playWith :: PlayConfig -> Page
playWith pc =
  playPageLibs (pc^.cPage.pagecLibs) <> 
  Page [] [] 
  cssPlay
  jsGlobalPlay
  (jsOnloadPlay (pc^.cHandleStream) (pc^.cState) (pc^.cSocket))
  mempty
  (sectionPlay buttonsPlay (slidersPlay (pc^.cState.pSpeed)) (textsPlay 0))

playEcho :: Page
playEcho = playWith (cHandleStream .~ jsEcho $ def)

jsEcho :: JStat
jsEcho =
  [jmacro|
   play.handleStream = function(e) {
       console.log(e);
       play.ws.send(JSON.stringify({tag:"Echo", contents: e}));
   };
  |]

playCssLibs :: [Text]
playCssLibs = 
  ["http://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css"]

playCssLibsLocal :: [Text]
playCssLibsLocal = 
  ["font-awesome-min.css"]

playJsLibs :: [Text]
playJsLibs = 
  [ "http://code.jquery.com/jquery-1.6.3.min.js"
  ]

playJsLibsLocal :: [Text]
playJsLibsLocal = 
  [ "jquery-2.1.3.min.js"
  ]

playPageLibs :: PageLibs -> Page
playPageLibs LinkedLibs =
    pageLibsCss .~ playCssLibs
  $ pageLibsJs .~ playJsLibs
  $ mempty
playPageLibs (LocalLibs dir) =
    pageLibsCss .~ ((\x -> pack dir <> "/" <> x) <$>  playCssLibsLocal)
  $ pageLibsJs .~ ((\x -> pack dir <> "/" <> x) <$>  playJsLibsLocal)
  $ mempty
