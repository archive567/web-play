{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Web.Play.Html where

import Web.Play.Types

import Control.Applicative
import Control.Lens
import Data.Monoid
import Web.Page

buttonsPlay :: [CtrButton]
buttonsPlay =
  [ CtrButton "btnQuit" (with i_ [class_ "icon-eject"] mempty) "#btnQuit"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Quit",contents:[]}}));
    |]
  , CtrButton "btnReset" (with i_ [class_ "icon-fast-backward"] mempty) "#btnReset"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"First",contents:[]}}));
    |]
  , CtrButton "btnStop" (with i_ [class_ "icon-stop"] mempty) "#btnStop"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Stop",contents:[]}}));
    |]
  , CtrButton "btnGo" (with i_ [class_ "icon-play"] mempty) "#btnGo"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Go",contents:[]}}));
    |]
  , CtrButton "btnStepForward" (with i_ [class_ "icon-step-forward"] mempty) "#btnStepForward"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Step",contents:1}}));
    |]
  , CtrButton "btnForward" (with i_ [class_ "icon-forward"] mempty) "#btnForward"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Step",contents:10}}));
    |]
  , CtrButton "btnFForward" (with i_ [class_ "icon-fast-forward"] mempty) "#btnFForward"
    [jmacro| 
     play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Last",contents:[]}}));
    |]
  ]

slidersPlay :: Double -> [CtrSlider]
slidersPlay s =
  [ CtrSlider "Speed" 0 5 0.1 s (with i_ [class_ "icon-fighter-jet icon-large"] mempty) "Speed"
    [jmacroE|
     function(d) {
         v = parseFloat(d);
         play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"Speed",contents:v}}));
     }
    |]
  ]

textsPlay :: Int -> [CtrText]
textsPlay frame =
  [ CtrText "Frame" (with i_ [id_ "spinner", class_ "icon-spinner icon-large"] mempty) frame "Frame"
    [jmacroE|
     function(d) {
         var v = parseInt(d);
         play.ws.send(JSON.stringify({tag:"PlayCommand",contents:{tag:"GoTo",contents:v}}));
     }
    |]
  , CtrText "TotalFrame" "of" frame "TotalFrame"
    [jmacroE|
     function(d) {
         return;
     }
    |]
  ]

sectionPlay :: [CtrButton] -> [CtrSlider] -> [CtrText] -> Html ()
sectionPlay buttons sliders texts =
  with div_ [class_ "play"] (mconcat 
    [ with div_ [id_ "buttons"] (mconcat ((\x -> with button_
      [ id_ (pack $ x ^. cbId)
      , type_ "button"
      ] (x^.cbValue)) <$> buttons))
    , with div_ [id_ "sliders"]
      (mconcat ((\x ->
       (x ^. csLabel) <>
       with (input_ mempty)
       [ id_ ("param" <> pack (x ^. csId))
       , type_ "range"
       , name_ (pack $ x ^. csId)
       , min_ (pack $ show (x ^. csMin))
       , max_ (pack $ show (x ^. csMax))
       , step_ (pack $ show (x ^. csStep))
       , value_ (pack $ show (x ^. csInitValue))
       ] <>
       with (input_ mempty)
       [ id_ ("text" <> pack (x ^. csId))
       , type_ "text"
       , value_ (pack $ show $ x^.csInitValue)
       ]) <$> sliders))
    , with div_ [id_ "framecount"] (mconcat ((\x ->
       (x ^. ctLabel) <>
       with (input_ mempty)
       [ id_ ("text" <> pack (x ^. ctId))
       , type_ "text"
       , value_ (pack $ show (x ^. ctValue))
       ]) <$> texts))
    ])
