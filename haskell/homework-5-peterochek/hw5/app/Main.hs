module Main
  ( main
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Set (fromList)
import HW5.Action (HIO (..), HiPermission (..))
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case parse input of
            Left parseErr -> do
              outputStrLn (show parseErr)
            Right expr -> do
              line <-
                liftIO $
                runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              case line of
                Left err -> do
                  outputStrLn (show err)
                Right res -> do
                  outputStrLn (show (prettyValue res))
          loop
