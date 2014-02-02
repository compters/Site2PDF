module Execute where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           GHC.IO.Handle
import           System.Exit
import           System.Process

data ProcessExit = OK String
                 | Error (Int, String)

data ProcessTimeout = Timeout String                 

executeProcess :: CreateProcess -> Int -> IO (Either ProcessExit ProcessTimeout)
executeProcess cmd timeoutMs = do
  processDetails <- createProcess cmd
  race (waitForExit processDetails) (killAfter timeoutMs)
  where
    waitForExit :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) ->  IO ProcessExit
    waitForExit (_, stdOut, stdErr, pHandle) = catch (do
        exitStatus <- waitForProcess pHandle
        case exitStatus of
          ExitSuccess -> do
            output <- maybeReadHandle stdOut
            return $ OK output
          ExitFailure exitCode -> do
            errMsg <- maybeReadHandle stdErr
            return $ Error(exitCode, errMsg))
      (killOnError pHandle)

    killOnError :: ProcessHandle -> SomeException -> IO ProcessExit
    killOnError pHandle _ = do
      terminateProcess pHandle
      return $ Error (1, "Killed")

    maybeReadHandle :: Maybe Handle -> IO String
    maybeReadHandle (Just h) = hGetContents h
    maybeReadHandle (Nothing) = return ""

    killAfter :: Int -> IO ProcessTimeout
    killAfter t = do
      threadDelay (t * 1000)
      return $ Timeout ("Timed out after " ++ (show t))


executeCommand :: String -> [String] -> Int -> IO (Either ProcessExit ProcessTimeout)
executeCommand cmd args timeout = executeProcess
                                  (proc cmd args){ std_out = CreatePipe, std_err = CreatePipe } timeout


executeCommandInDirectory :: String -> [String] -> String -> Int -> IO (Either ProcessExit ProcessTimeout)
executeCommandInDirectory cmd args dir timeout = executeProcess
                                  (proc cmd args) { std_out = CreatePipe
                                                  , std_err = CreatePipe
                                                  , cwd = Just dir } timeout
