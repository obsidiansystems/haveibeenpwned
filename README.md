haveibeenpwned [![travis-ci](https://api.travis-ci.org/obsidiansystems/haveibeenpwned.svg?branch=develop)](https://travis-ci.org/obsidiansystems/haveibeenpwned)
======================

A [haskell](https://haskell.org) library for checking passwords against the [haveibeenpwned.com](https://haveibeenpwned.com) database.

By means of this library you can do some basic strength check on new user
passwords. Common weak passwords like many plain English words or also many
stronger passwords which happen to have been leaked will likely be found in the
database and can thus be rejected.
