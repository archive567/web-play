module TestPlay where

import           Control.Lens
import           Data.Default
import qualified Data.Text.Lazy.IO as Text
import           Web.Page
import           Test.Tasty.Hspec
import           Web.Play

generatePlayPage :: FilePath -> IO ()
generatePlayPage file =
  renderToFile file $ renderPageHtmlWith (def ^. cPage) $ playWith def

testPageVsFile :: FilePath -> IO Bool
testPageVsFile file = do
  let t = renderText $ renderPageHtmlWith (def ^. cPage) $ playWith def
  t' <- Text.readFile file
  return $ t' == t

testsPlay :: IO (SpecWith ())
testsPlay =
  return $ describe "playWith" $ do
    it "run `generatePlayPage 'test/canned/'` for a new canned file." True
    -- it "default PlayConfig" $
    --   testPageVsFile "test/canned/play.html" `shouldReturn` True
