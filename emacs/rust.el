
;(setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
(setq racer-cmd "/home/benedikt/github/racer/target/release/racer")
(add-to-list 'load-path "/home/benedikt/github/racer/editors/emacs")
(eval-after-load "rust-mode" '(require 'racer))
