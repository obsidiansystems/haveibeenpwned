# Revision history for haveibeenpwned

## Future Release

Breaking change in order to make the API and the implementation more secure.

- There is a new HaveIBeenPwnedResult_Secure constructor which signals that the given password was not found in any database.
- The `HaveIBeenPwnedResult_Disclosed` constructor has been renamed to `HaveIBeenPwnedResult_Pwned`, as its behaviour changed. (Valid passwords are no longer signalled by this constructor.)

## 0.1.0.0

* Initial release
