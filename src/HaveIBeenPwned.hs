{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Query haveibeenpwned database to check basic password strength in a secure way.
--
--   By checking new user passwords against a database of leaked passwords you
--   get some means for rejecting very weak or just leaked passwords.
module HaveIBeenPwned where

import "cryptohash" Crypto.Hash -- or maybe i wanted cryptonite?
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text(Text)
import Data.Text.Encoding(encodeUtf8)
import qualified Data.Text.Lazy.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status(Status(..))
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Default (def)
import Safe (readMay)

data HaveIBeenPwnedConfig = HaveIBeenPwnedConfig
  { _haveIBeenPwnedConfig_manager :: Manager
  , _haveIBeenPwnedConfig_apihost :: Text
  }

-- | Result of a password check.
--
--   It is either considered secure, insecure or we can't say because of an
--   error.
data HaveIBeenPwnedResult =
    HaveIBeenPwnedResult_Secure
    -- ^ We could not find the password in any database, thus it is considered
    -- "secure" as far as this library is concerned.
  | HaveIBeenPwnedResult_Pwned Int
    -- ^ How many times the password was found in public places. Usually this
    -- will be a value greater than 0, but in any case if you hit this
    -- constructor you must assume tha password has been leaked.
  | HaveIBeenPwnedResult_Error
    -- ^ The check failed for some reason. We can't say anything about the
    -- password quality.
  deriving (Eq, Ord, Show)

class Monad m => MonadPwned m where
  -- | Returns the number of disclosures the supplied password has been seen in.
  --
  -- If this is not zero, do not use the supplied password, it is known to hackers.
  -- If it *is* zero, it might still not be safe, only that if it is
  -- compromised, that is not yet known.
  --
  -- https://haveibeenpwned.com/API/v2#SearchingPwnedPasswordsByRange
  haveIBeenPwned :: Text -> m HaveIBeenPwnedResult

newtype PwnedT m a = PwnedT { unPwnedT :: ReaderT HaveIBeenPwnedConfig m a }
  deriving (Functor, Applicative, Monad , MonadIO, MonadLogger
    , MonadTrans
    )

runPwnedT :: PwnedT m a -> HaveIBeenPwnedConfig -> m a
runPwnedT (PwnedT (ReaderT f)) = f

mapPwnedT :: (m a -> n b) -> PwnedT m a -> PwnedT n b
mapPwnedT f = PwnedT . mapReaderT f . unPwnedT

instance MonadReader r m => MonadReader r (PwnedT m) where
  ask = lift ask
  local = mapPwnedT . local
  reader = lift . reader

instance (MonadLogger m, MonadIO m) => MonadPwned (PwnedT m) where
 haveIBeenPwned password = do
  let (pfx, rest) = passwdDigest password
  cfg <- PwnedT ask
  let request = parseRequest_ $ T.unpack $ T.concat [_haveIBeenPwnedConfig_apihost cfg, "/", pfx]
  result' <- liftIO $ try $ httpLbs request (_haveIBeenPwnedConfig_manager cfg)
  case result' of
    Left err -> do
      $(logError) $ T.pack $ show @ HttpException $ err
      return HaveIBeenPwnedResult_Error
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let r = parseHIBPResponse (responseBody result) rest
        case r of
          HaveIBeenPwnedResult_Error ->
            $(logError) $ "Parsing number of occurrences failed. (Not an Int)."
          _ -> pure ()
        pure r
      Status code phrase -> do
        $(logError) $ T.pack $ show $ Status code phrase
        return HaveIBeenPwnedResult_Error


-- | Get the sha1 digest for the supplied password, split into two parts, to agree with the
--   hibp api.
passwdDigest :: Text -> (Text, Text)
passwdDigest passwd = (T.take 5 digest, T.drop 5 digest)
  where digest = T.toUpper $ T.pack $ show $ sha1 $ encodeUtf8 passwd
        sha1 :: ByteString -> Digest SHA1
        sha1 = hash

-- | The hibp response is a line separated list of colon separated hash
-- *suffixes* and a number indicating the number of times that password(hash)
-- has been seen in known publicly disclosed leaks
parseHIBPResponse :: LBS.ByteString -> Text -> HaveIBeenPwnedResult
parseHIBPResponse response suffix =
  let
    digests :: [(LT.Text, Maybe Int)]
    digests = fmap (fmap (readMay . LT.unpack . LT.drop 1) . LT.breakOn ":") $ LT.lines $ Data.Text.Lazy.Encoding.decodeUtf8 response
  in case filter ((LT.fromStrict suffix ==) . fst) digests of
    ((_,n):_) -> maybe HaveIBeenPwnedResult_Error HaveIBeenPwnedResult_Pwned n
    [] -> HaveIBeenPwnedResult_Secure

-- | A really simple demo of the hibp functionality
consoleHaveIBeenPwned :: IO ()
consoleHaveIBeenPwned = do
  runStdoutLoggingT $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    p <- liftIO $ getPassword
    let hibpEnv = HaveIBeenPwnedConfig mgr "https://api.pwnedpasswords.com/range"
    p' <- flip runPwnedT hibpEnv $ haveIBeenPwned $ T.pack p
    liftIO $ case p' of
      HaveIBeenPwnedResult_Secure ->
        putStrLn "Your password does not appear in any known breaches.  Practice good password hygene."
      HaveIBeenPwnedResult_Pwned p'' ->
        putStrLn $ "You have been pwned! Your password has appeared in breaches " ++ show p'' ++ " times."
      HaveIBeenPwnedResult_Error ->
        putStrLn "Network Error, try again later"

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  password <- withEcho False getLine
  putChar '\n'
  return password

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
