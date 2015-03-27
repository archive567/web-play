{-# LANGUAGE OverloadedStrings #-}
module Web.Play.Css where

import Lucid.Page.Css as Css

cssPlay :: Css
cssPlay = ".play" ? do
  fontSize (px 10)
  fontFamily ["Arial", "Helvetica"] [sansSerif]
  marginLeft (px 30)
  marginTop (px 12)
  marginBottom (px 12)
  "#spinner" ?
      marginLeft (px 6)
  ".btn" ?
      marginLeft (px 6)
  "#buttons" ? do
      marginTop (px 2)
      marginBottom (px 6)
  "#btnGo.on" ?
      color green
  "#btnStop.on" ?
      color red
  ".slider" ? do
      float floatLeft
      width (px 100)
  ".box" ? do
      width (px 40)
      textAlign end
  ".label" ? do
      position relative
      float floatLeft
      marginRight (px 20)
      width (px 100)
  ".ctrl" ? do
      marginTop (px 8)
      marginBottom (px 8)
  "#paramSpeed" ? do
      width (px 60)
      height (px 6)
  "#textSpeed" ?
      width (px 30)
  "#textFrame" ? do
      width (px 30)
      marginLeft (px 6)
      marginRight (px 6)
  "#textTotalFrame" ? do
      width (px 30)
      marginLeft (px 6)
  "input" ? do
      backgroundColor transparent
      border solid (px 0) wheat
      -- width (100::Size Rel)
  "#sliders, #framecount" ?
      float floatLeft
