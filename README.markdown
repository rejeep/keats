# Keats

Keats is a mode for storing notes about Emacs key bindings so you
easier remember them.

Keats is really two modes. One major and one minor. Read below for
more information.

## Keats Mode
This is the minor mode and can control the keats with keybindings
without opening another buffer.

Add keats to Emacs load-path
    (add-to-list 'load-path "/path/to/directory/or/file")

Then require keats
    (require 'keats)

## Keats Interactive Mode
The major mode opens a buffer with keats. This mode is easier to work
with, specially if you want to add, edit or remove many keats.

Require the interactive mode (requires the above)
    (require 'keats-interactive)
