{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString                             (ByteString)
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Heist
import           Heist.Interpreted
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Text.XmlHtml                                as X
------------------------------------------------------------------------------
import           Application
import           Control.Monad
import           Data.Monoid
import           Feedback
import           Pdf
import Data.Maybe

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("",     serveDirectory "static")]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "site2pdf" "Convert Websites to PDFs" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    p <- nestSnaplet "pdf" pdf pdfInit
    f <- nestSnaplet "feedback" feedback $ feedbackInit "feedback.txt"
    addStringSplices h
    addRoutes routes
    return $ App h p f

addStringSplices :: HasHeist b => Snaplet (Heist b) -> Initializer b v ()
addStringSplices h = addConfig h $ mempty
    {
      hcInterpretedSplices = do
         "faq"   ## mapSplices faqSplice [Faq "What does this site do?" "It scrapes publically available sites to PDFs.",
                                          Faq "Does it support flash?" "Due to technical limitations, no."]
         "error" ## messageSplice =<< (getQueryParam "error")
         "msg" ## messageSplice =<< (getQueryParam "msg")
    }

data Faq = Faq { question :: T.Text, answer :: T.Text }

faqSplice :: Monad m => Faq -> Splice m
faqSplice faq = runChildrenWith $ do
  "question" ## textSplice (question faq)
  "answer"   ## textSplice (answer faq)


messageSplice :: Monad m => Maybe ByteString -> Splice m
messageSplice (Just msg) = runChildrenWith $ do
    "message" ## textSplice (T.decodeUtf8 msg)
    "display" ## textSplice "block"
messageSplice Nothing = runChildrenWith $ do
    "message" ## textSplice ""
    "display" ## textSplice "none"

