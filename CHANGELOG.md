# Revision history for haveibeenpwned

## 0.2.0.0

* Breaking change in order to make the API and the implementation more secure.
  * There is a new HaveIBeenPwnedResult_Secure constructor which signals that the given password was not found in any database.
  * The `HaveIBeenPwnedResult_Disclosed` constructor has been renamed to `HaveIBeenPwnedResult_Pwned`, as its behaviour changed. (Valid passwords are no longer signalled by this constructor.)
* Also internally, a "not found in database" is no longer represented as a disclosed count of zero. This improves security in the case of an incorrect database entry, having a disclosed count of 0, which would make this library report that password as "secure", although it actually has been leaked.

## 0.1.0.0

* Initial release
