{-# LANGUAGE OverloadedStrings #-}

module Feedback where

import           Control.Concurrent    (forkIO)
import           Control.Monad         (when)
import           Control.Monad.Trans   (liftIO)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Maybe            (isJust)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Snap.Core
import           Snap.Snaplet
import           System.Posix          (fileExist)

data Feedback = Feedback

feedbackInit :: FilePath -> SnapletInit a Feedback
feedbackInit logFile = makeSnaplet "feedback" "Feedback snaplet" Nothing $ do
      addRoutes [("submit/", feedbackSubmit logFile)]
      return Feedback

feedbackSubmit :: FilePath -> Handler a m ()
feedbackSubmit log = do
  param <- getPostParam "feedbackField"
  msg <- liftIO $ logFeedback param log
  redirect $ "/?msg=" `C8.append` (urlEncode msg)

logFeedback :: Maybe BS.ByteString -> FilePath -> IO (BS.ByteString)

logFeedback (Just "") log = return "Fine, keep it to yourself. See if I care."
logFeedback (Just msg) log = do
  forkIO $ BS.appendFile log (msg `C8.append` "\r\n") -- Forked to stop any blocking
  return "Thanks, we'll action that straight away."
logFeedback Nothing log = return "Fine, keep it to yourself. See if I care."
