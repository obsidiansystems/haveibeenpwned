haveibeenpwned
======================
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/haveibeenpwned.svg)](https://hackage.haskell.org/package/haveibeenpwned) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/haveibeenpwned/badge)](https://matrix.hackage.haskell.org/#/package/haveibeenpwned)   [![Github CI](https://github.com/obsidiansystems/haveibeenpwned/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/haveibeenpwned/actions) [![travis-ci](https://api.travis-ci.org/obsidiansystems/haveibeenpwned.svg?branch=develop)](https://travis-ci.org/obsidiansystems/haveibeenpwned) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/haveibeenpwned/blob/master/LICENSE)

A [haskell](https://haskell.org) library for checking passwords against the
[haveibeenpwned.com](https://haveibeenpwned.com) database.

By means of this library you can do some basic strength check on new user
passwords. Common weak passwords like many plain English words or also many
stronger passwords which happen to have been leaked will likely be found in the
database and can thus be rejected.

Example
-------

The example below can be built and run using `cabal build exe:readme` or `cabal
repl exe:readme`.

```haskell 

> {-# LANGUAGE OverloadedStrings #-}
>
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Logger (runStdoutLoggingT)
> import Control.Exception (bracket_)
> import Data.Text as T (pack)
> import Network.HTTP.Client (newManager)
> import Network.HTTP.Client.TLS (tlsManagerSettings)
> import System.IO (hFlush, stdout, hGetEcho, stdin, hSetEcho)
>
> import HaveIBeenPwned
>
> -- | A really simple demo of the hibp functionality. Asks the user to enter
> -- a password and then uses the hibp api to check whether that password has
> -- been pwned.
> consoleHaveIBeenPwned :: IO ()
> consoleHaveIBeenPwned = do
>   runStdoutLoggingT $ do
>     mgr <- liftIO $ newManager tlsManagerSettings
>     p <- liftIO $ getPassword
>     let hibpEnv = HaveIBeenPwnedConfig mgr "https://api.pwnedpasswords.com/range"
>     p' <- flip runPwnedT hibpEnv $ haveIBeenPwned $ T.pack p
>     liftIO $ case p' of
>       HaveIBeenPwnedResult_Secure ->
>         putStrLn "Your password does not appear in any known breaches.  Practice good password hygene."
>       HaveIBeenPwnedResult_Pwned p'' ->
>         putStrLn $ "You have been pwned! Your password has appeared in breaches " ++ show p'' ++ " times."
>       HaveIBeenPwnedResult_Error ->
>         putStrLn "Network Error, try again later"
>
> getPassword :: IO String
> getPassword = do
>   putStr "Password: "
>   hFlush stdout
>   password <- withEcho False getLine
>   putChar '\n'
>   return password
>
> withEcho :: Bool -> IO a -> IO a
> withEcho echo action = do
>   old <- hGetEcho stdin
>   bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
>
> main :: IO ()
> main = consoleHaveIBeenPwned

```

***
![Obsidian Systems](https://obsidian.systems/static/images/ObsidianSystemsLogo.svg)
