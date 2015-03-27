{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Web.Play.Types where

import Control.Lens
import Data.Aeson
import Data.Data
import Data.Default
import Data.Monoid
import GHC.Generics
import Lucid.Page
import Web.Socket

{- type representing the state of controllers on a page -}
data PlayState =
    PlayState
    { _pPlaying    :: Bool        -- whether to compute and stream the next frame
    , _pSleep      :: Double      -- sleep time between checking pGoing state
    , _pSpeed      :: Double      -- add delay effect to next frame
    , _pFrame      :: Int         -- current frame position
    , _pTargetFrame :: Maybe Int  -- move to new frame
    , _pTotalFrames :: Maybe Int -- may not be known
    , _pDropOk      :: Bool      -- can the stream be not streamed on rewind and fast forward etc
    , _pRedraw      :: Bool      -- instruction to View as to whether to redraw effect (False can be used to avoid overloading the view etc).
    , _pStep        :: Maybe Int  -- counter to an automatic stop (Nothing means no counter, Just 0 will turn _pPlaying from Go to Stop
    , _pFast        :: Bool      -- ignore Speed
    } deriving (Show, Read, Eq, Data, Typeable, Generic)

makeLenses ''PlayState

instance FromJSON PlayState
instance ToJSON PlayState

instance Default PlayState where
    def = defaultPlayState

defaultPlayState :: PlayState
defaultPlayState =
    PlayState False 0.2 1.0 0 Nothing Nothing True True Nothing False

data PlayConfig = PlayConfig
  { _cState :: PlayState
  , _cHandleStream :: JStat
  , _cSocket :: SocketConfig
  , _cPage :: PageConfig
  }

makeLenses ''PlayConfig

instance Default PlayConfig where
    def = PlayConfig def mempty def def

data PlayCommand
    = Go   -- compute (unpausing)
    | Stop -- pause computing
    | Quit -- stop computing and close down computation stream
    | First -- go to first frame
    | Last -- go (skip and/or fast) to last frame
    | Speed Double -- change speed
    | Step Int -- Go for x frames (skip if possible)
    deriving (Show, Read, Eq, Typeable, Data, Generic)

instance FromJSON PlayCommand
instance ToJSON PlayCommand

data SocketMessage = ServerReady deriving (Show, Read, Eq, Typeable, Data, Generic)

instance FromJSON SocketMessage
instance ToJSON SocketMessage

-- | dom elements
data CtrButton =
    CtrButton
    { _cbId :: String
    , _cbValue :: Html ()
    , _cbDomName :: String
    , _cbOnClick :: JStat }
makeLenses ''CtrButton

data CtrText =
    CtrText
    { _ctId :: String
    , _ctLabel :: Html ()
    , _ctValue :: Int
    , _ctDomName :: String
    , _ctOnChange :: JExpr }
makeLenses ''CtrText

data CtrSlider =
    CtrSlider
    { _csId :: String
    , _csMin :: Double
    , _csMax :: Double
    , _csStep :: Double
    , _csInitValue :: Double
    , _csLabel :: Html ()
    , _csDomName :: String
    , _csOnChange :: JExpr } 

makeLenses ''CtrSlider
