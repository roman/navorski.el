# navorski.el

navorski.el is an emacs extension that facilitates the management of
terminals (local or remote).

## Emacs version

This extension has been tested with emacs 24.3.1 on Mac OS X Lion/Mavericks
and Gentoo.

## Dependencies

This extension will require:

* [dash.el](https://github.com/magnars/dash.el)
* [multi-term](https://github.com/emacsmirror/multi-term)

## Installation

You can install this extensions through MELPA

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
  :setup-tramp nil
  :init-script ("cd project-path" "bundle exec rails c")

;; M-x: (nav/rails-console-production-pop-to-buffer)
;; will open a new buffer to a production rails console

(nav/defterminal ipython
  :buffer-name "ipython-console"
  :proccess-path "/usr/bin/ipython")

;; M-x: (nav/ipython-pop-to-buffer)
;; will open a new buffer to a local ipython session.
```

navorski.el will use an SSH connection when a :remote-host option is
provided, check out the documentation of `nav/defterminal` for all
available options.

## Why the name navorski?

Viktor Navorski is a fictional character from the movie [The
Terminal](http://en.wikipedia.org/wiki/The_Terminal), that _lived_ in
a terminal. Got the pun right?

## License

```
 navorski.el - Making you live in the terminal

 Copyright (C) 2014  Roman Gonzalez
 Copyright (C) 2013  Roman Gonzalez, Birdseye Software Inc.

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ```