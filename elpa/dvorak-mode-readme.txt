Just require this package immediately after the call to `package-initialize`.

(package-initialize)
(require 'dvorak-mode)

Then any key bindings defined after this line in any external package will be
translated to its Dvorak equivalent.  For example:

    C-c ! n

becomes

    C-j % b

Note that will override `define-key` command so your personal customization
key bindings will be translated too.
