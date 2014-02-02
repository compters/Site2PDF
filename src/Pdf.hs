{-# LANGUAGE OverloadedStrings #-}

module Pdf where

import           Control.Monad         (liftM)
import           Control.Monad.Trans
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.UUID
import           Data.UUID.V4
import           Execute
import           Snap.Core
import           Snap.Snaplet
import           System.Posix          (fileExist)
import           Text.Printf           (printf)

data PDF = PDF

-- Hardcoded error redirect as there isn't any need to provide detailed messages to the users.
-- Look into replacing this with something that stores it in the user's session?
redirectError :: Handler a m ()
redirectError = redirect $ "/?error=" `C8.append` (urlEncode "There was an error.")

-- 30 Seconds should be more than enough
scrapeTimeout :: Int
scrapeTimeout = 30000

pdfInit :: SnapletInit a PDF
pdfInit = makeSnaplet "pdf" "PDF snaplet" Nothing $ do
      addRoutes [("scrape/", pdfScraper)]
      return PDF

pdfScraper :: Handler a m ()
pdfScraper = do
  param <- getPostParam "url"
  maybe redirectError
    scrapeIt (param >>= urlDecode)

sendPdfToBrowser :: FilePath -> Handler a m ()
sendPdfToBrowser file = do
  modifyResponse (setContentType "application/pdf")
  sendFile file

randomFileName :: IO (FilePath)
randomFileName = do
  uuid <- nextRandom
  return $ printf "output/%s.pdf" (toString uuid)

scrapeIt :: BS.ByteString -> Handler a m ()
scrapeIt url = do
  file <- liftIO randomFileName
  -- Assuming here that wkhtmltopdf is in the bin directory rather than being globally available
  result <- liftIO $ executeCommand "bin/wkhtmltopdf" ["--quiet", C8.unpack url, file] scrapeTimeout
  either (sendPdf file) sendTimeout result
  where
        sendTimeout :: ProcessTimeout -> Handler a m ()
        sendTimeout _ = redirectError

        sendPdf :: String -> ProcessExit -> Handler a m ()
        sendPdf file (OK _) = sendPdfToBrowser file

        sendPdf file (Error (_, msg)) = do
          -- If the file has data then wkhtmltopdf may have exited with an error when in face
          -- it's only a warning. Ignore it and send
          exists <- liftIO $ fileExist file
          case exists of
            False -> redirectError
            True -> sendPdfToBrowser file

