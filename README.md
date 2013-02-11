# navorski.el

navorski.el is an emacs extension that facilitates the management of
terminals (local or remote).

## Emacs version

This extension has been tested with emacs 24.1.1 on Mac OS X Lion
and Gentoo

## Dependencies

This extension will require:

* [dash.el](https://github.com/magnars/dash.el)
* [multi-term](https://github.com/emacsmirror/multi-term)

## Installation

Currently this extension is only available through [el-get](https://github.com/dimitri/el-get)

## Getting Started

navorski.el generates all the functions you will need to interact with
a terminal through the macro `nav/defterminal`.

Creating terminal profiles is really easy, for this reason every
terminal profile should have one purpose, if you need to have a
your terminal, to do something specific just create a new profile
and call the generated `pop-to-buffer` function.

```elisp
(require 'navorski)

(nav/defterminal production-console
  :buffer-name "production-console"
  :screen-session-name "production_console"
  :remote-host "user@production"
  :init-script "cd project-path && bundle exec rails c\n")

;; NOTE: the newline in the init-script is important

;; M-x: (nav/rails-console-production-pop-to-buffer)
;; will open a new buffer to a production rails console

(nav/defterminal ipython
  :buffer-name "dev-console"
  :proccess-path "/usr/bin/ipython"
  :setup-tramp nil)

;; M-x: (nav/ipython-pop-to-buffer)
;; will open a new buffer to a local ipython session.
```

navorski.el will use an SSH connection when a :remote-host option
is provided, check out the function documentation for all possible
options.

## License