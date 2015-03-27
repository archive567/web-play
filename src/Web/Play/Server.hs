{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- http://localhost:8001/
-- type 'q' in terminal to quit server

module Web.Play.Server where

import Web.Play.MVC

import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.State.Strict (State)
import           Data.Aeson hiding ((.=))
import           Data.Default
import           MVC
import qualified Pipes.Prelude as Pipes
import           Lucid.Page.Server
import Lucid.Page.Html (Html)
import Lucid.Page.Render
import           Web.Play.Page
import           Web.Play.Types
import           Web.Socket
{-
servePlay
  :: (FromJSON b, ToJSON b, Show b, Eq b)
  => Producer b IO ()
  -> PlayConfig
  -> Page
  -> IO ()
servePlay prod pc page = do
  aRun <- async $
    runMVC
    (pc^.cState)
    (asPipe $ mainPipe cat)
    (vcPlay (pc^.cState) (pc^.cSocket) prod (return ()))
  aServe <- async $
      serve $ ok $ toResponse page
  res <- wait aRun
  cancel aServe
  return ()
-}
runPlay
  :: (ToMessage a, FromJSON b, ToJSON b, Show b, Eq b)
  => Producer b IO ()
  -> a
  -> PlayState
  -> IO PlayState
runPlay prod page'' p = do
  aRun <- async $
    runMVC
    p
    (asPipe $ mainPipe cat)
    (vcPlay p defaultSocketConfig prod (return ()))
  aServe <- async $
      serve $ ok $ toResponse page''
  res <- wait aRun
  cancel aServe
  return res

-- wrapping the model in a new state (to enable testing, replay etc)
data SWrap o a b = SWrap
  { _wrapOrig :: o
  , _wrapOuts :: [a]
  , _wrapIns :: [b]
  } deriving (Show, Eq)

makeLenses ''SWrap

runMVCWrapped :: o -> Pipe b a (State o) () -> Managed (View a, Controller b) -> IO (SWrap o a b)
runMVCWrapped s pipe vc = do
  s' <- runMVC (SWrap s [] []) (asPipe $ wrapPipe pipe) vc
  return (wrapOuts %~ reverse $ wrapIns %~ reverse $ s')
  where
    wrapPipe :: Pipe b a (State o) () -> Pipe b a (State (SWrap o a b)) ()
    wrapPipe p = 
      Pipes.chain (\x -> wrapIns %= (:) x)
      >-> hoist (zoom wrapOrig) p
      >-> Pipes.chain (\x -> wrapOuts %= (:) x)

runPlayWrapped
  :: (ToMessage a, FromJSON b, ToJSON b, Show b, Eq b)
  => Producer b IO ()
  -> a
  -> PlayState
  -> IO (SWrap PlayState (Out b) (In b))
runPlayWrapped prod page'' p = do
  aRun <- async $ runMVCWrapped p (mainPipe cat) (vcPlay p defaultSocketConfig prod (return ()))
  aServe <- async $
    serve $ ok $ toResponse page''
  res <- wait aRun
  cancel aServe
  return res

runPlayWrappedAuto
  :: (ToMessage a, FromJSON b, ToJSON b, Show b, Eq b)
  => Producer b IO ()
  -> Producer (In b) IO ()
  -> a
  -> PlayState
  -> IO (SWrap PlayState (Out b) (In b))
runPlayWrappedAuto prod auto page'' p = do
  aRun <- async $ runMVCWrapped p (mainPipe cat) (vcPlay p defaultSocketConfig prod auto)
  aServe <- async $
      serve $ ok $ toResponse page''
  res <- wait aRun
  cancel aServe
  return res

responsePlay
  :: (FromJSON b,ToJSON b, Show b, Eq b)
  => Producer b IO ()
  -> Html ()
  -> PlayState
  -> ServerPartT IO Response
responsePlay prod page'' p = do
  _ <- liftIO $ async $
       runMVC p (asPipe $ mainPipe cat) (vcPlay defaultPlayState defaultSocketConfig prod (return ()))
  ok $ toResponse page''

-- tests
tPlayAuto
  :: Int
  -> Producer (In [Int]) IO ()
  -> IO (SWrap PlayState (Out [Int]) (In [Int]))
tPlayAuto n auto = do
  let prod = MVC.each [1..n] >-> Pipes.map (:[])
      p = pTotalFrames .~ Just n $ defaultPlayState
  runMVCWrapped p (mainPipe cat) (vcPlay p defaultSocketConfig prod auto)

tGoStop
  :: IO (SWrap PlayState (Out [Int]) (In [Int]))
tGoStop = tPlayAuto 10 $ do
  yield $ PlayCommand Go
  lift $ sleep 3.3
  yield $ PlayCommand Stop
  lift $ sleep 1.2
  yield ServerQuit

testPlay'
  :: PlayState
  -> Int
  -> IO (SWrap PlayState (Out Int) (In Int))
testPlay' p n =
  runPlayWrappedAuto
   (MVC.each [1..n])
   (return ())
   (renderPage $ playWith (PlayConfig p jsEcho def def))
   p

testManual :: IO (SWrap PlayState (Out Int) (In Int))
testManual = testPlay' (pTotalFrames .~ Just 1000 $ defaultPlayState) 1000



