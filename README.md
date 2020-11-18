haveibeenpwned [![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/haveibeenpwned.svg)](https://hackage.haskell.org/package/haveibeenpwned) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/haveibeenpwned/badge)](https://matrix.hackage.haskell.org/#/package/haveibeenpwned)   [![Github CI](https://github.com/obsidiansystems/haveibeenpwned/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/haveibeenpwned/actions) [![travis-ci](https://api.travis-ci.org/obsidiansystems/haveibeenpwned.svg?branch=develop)](https://travis-ci.org/obsidiansystems/haveibeenpwned) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/haveibeenpwned/blob/master/LICENSE)

======================

A [haskell](https://haskell.org) library for checking passwords against the [haveibeenpwned.com](https://haveibeenpwned.com) database.

By means of this library you can do some basic strength check on new user
passwords. Common weak passwords like many plain English words or also many
stronger passwords which happen to have been leaked will likely be found in the
database and can thus be rejected.
