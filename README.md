# htmx

For generating HTMX **safely** using good ol' Haskell data types.

This package acts as a basis for the rest of the "HTMX in Haskell" ecosystem.
The dependants of this package are:

- `lucid-htmx` - For use in with the `lucid` HTML DSL.
- `servant-htmx` - For generating type-safe HTMX AJAX calls to your `servant` API.

Ideas that may be implemented in the future if there's enough interest or need:

- `mustache-htmx` - For using `htmx` seamlessly with `mustache` templates.
- `hamlet-htmx` - For using `htmx` in `hamlet` templates. Great for users of `yesod`. 
