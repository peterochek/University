{-# LANGUAGE DerivingVia #-}

module HW5.Action
  ( HIO (HIO, runHIO)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString (ByteString)
import Data.Set (Set, notMember)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import Prelude hiding (read)
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random.Stateful (getStdRandom, uniformR)

import HW5.Base (HiAction (..), HiMonad, HiValue (..), Wrapper (wrap, wrapped), runAction)

import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as IO (putStrLn)
import HW5.Utils (makeAbsolute)


data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Enum, Bounded)

instance Show HiPermission where
  show permission = case permission of
    AllowRead  -> "read"
    AllowWrite -> "write"
    AllowTime  -> "time"

newtype PermissionException = PermissionRequired HiPermission
  deriving (Eq)

instance Show PermissionException where
  show (PermissionRequired p) = show p ++ "!"

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
  via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction action = case action of
    HiActionRead file        -> read file
    HiActionWrite file bytes -> write file bytes
    HiActionMkDir path       -> mkdir path
    HiActionChDir path       -> cd path
    HiActionCwd              -> cwd
    HiActionNow              -> now
    HiActionRand lower upper -> rand lower upper
    HiActionEcho text        -> echo text

ensure :: HiPermission -> IO a -> HIO a
ensure var task = HIO $ \allowed -> do
  when (var `notMember` allowed) $ throwIO (PermissionRequired var)
  task

read :: FilePath -> HIO HiValue
read path = ensure AllowRead $ do
  absPath <- makeAbsolute path
  exists <- doesDirectoryExist absPath
  if exists then do
    list <- listDirectory absPath
    wrapped (wrap <$> list)
  else do
    bytes <- BS.readFile absPath
    let content = either (const (HiValueBytes bytes)) wrap (decodeUtf8' bytes)
    return content

write :: FilePath -> ByteString -> HIO HiValue
write path bytes = ensure AllowWrite $ do
  absPath <- makeAbsolute path
  BS.writeFile absPath bytes
  return HiValueNull

mkdir :: FilePath -> HIO HiValue
mkdir dir = ensure AllowWrite $ do
  mkDir <- makeAbsolute dir
  createDirectory mkDir
  return HiValueNull

cd :: FilePath -> HIO HiValue
cd path = ensure AllowRead $ do
  cdDir <- makeAbsolute path
  setCurrentDirectory cdDir
  return HiValueNull

cwd :: HIO HiValue
cwd = ensure AllowRead (wrap . Text.pack <$> getCurrentDirectory)

now :: HIO HiValue
now = ensure AllowTime (wrap <$> getCurrentTime)

rand :: Int -> Int -> HIO HiValue
rand lower upper = do
  rnd <- getStdRandom $ uniformR (lower, upper)
  wrapped rnd

echo :: Text -> HIO HiValue
echo str = ensure AllowWrite (IO.putStrLn str >> return HiValueNull)
