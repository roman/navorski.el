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

navorski.el provides all the functions you will need to interact with
a terminal through the `nav/defterminal` macro.

When using `defterminal`, a variable with the name used on the first
parameter is created, you can use this variable to call different
functions. Let's suppose you are working on a project named foobar
and you want to have a dedicated ipython terminal for it:

```elisp
(nav/defterminal foobar-ipython
  :buffer-name "ipython-console"
  :program-path "/usr/bin/ipython"
  :cwd "/Users/johndoe/Projects/foobar")
```

Once this is declared, you can call `(nav/pop-to-buffer
foobar-ipython)` and a new buffer with the process specified in that
profile will be executed. This particular example creates a
rudimentary ipython inferior-mode. You can then create your own
keybindings or helper functions to interact with this process buffer.

## Example + explanations

```elisp
(require 'navorski)

(nav/defterminal production-console
  :buffer-name "production-console"
  :interactive t                                         ;; (1)
  :remote-host "user@production"                         ;; (2)
  :screen-session-name "production_console"              ;; (3)
  :init-script ("cd project-path" "bundle exec rails c") ;; (4)
  )

;; M-x: nav/production-console-pop-to-buffer
;; will open a new buffer to a production rails console

(nav/defterminal foobar-ipython
  :buffer-name "ipython-console"
  :cwd "/Users/johndoe/Projects/foobar" ;; (5)
  :program-path "/usr/bin/ipython"      ;; (6)
  )

```

### (1) Creating interactive profile functions on the fly

If you don't want to get your hands dirty doing custom functions
that used the terminal profile, you can pass the `:interactive`
flag to the `defterminal` call. This will generate the following
functions:

* `nav/<profile-name>-pop-to-buffer`
* `nav/<profile-name>-kill-buffer`
* `nav/<profile-name>-send-string`
* `nav/<profile-name>-send-region`

Creating terminal profiles is really easy, for this reason every
terminal profile should have one purpose, if you need to have a
terminal to do something specific, just create a new profile
and call the generated `pop-to-buffer` function. An example of this:

### (2) Remote Terminals

Your terminals don't need to be only local, you can specify a
`:remote-host` option to create a remote terminal via SSH.

Another way to create a remote-terminal without a `defterminal`
profile is via the function `nav/remote-term`. Navorski offers by
default a tramp setup on the remote machine, _this feature will only
work if the SSH authentication happens via public/private keys_.

### (3) Persistent Terminals

Another nice feature is the ability to automatically create a GNU
Screen session on an specified terminal, this is really useful when
working with remote terminals. Just need to provide the
`:screen-session-name` to a `defterminal` profile.

Another way to create a persistent-terminal without a `defterminal`
profile is via the function `nav/persistent-term`. It's important to
note that the `:program-args` parameter will not work when using
persistent terminals, this is a limitation of the GNU Screen
program. You can however trick this by creating a custom bash script
with all the arguments you need and call that script from navorski
instead.

### (4) Executing bash when terminal is initialized

Sometimes in order to get a terminal to a desired state you want to
execute a group of commands after it is created. The `:init-script`
option allows you to pass a list of strings that represent bash
commands.

### (5) Change working directory of a program execution

With the `:cwd` option, you may specify either:

  * a string that will be the `default-directory` when a navorski
    command is called

  * a function that recevies the `default-directory` and returns a new
    value for it when a navorski command is called (this is
    particularly useful in combiation of `locate-dominating-file`)

This option won't work correctly on remote terminals, you may do a
`cd` command on the :init-script option instead.

### (6) Create buffer processes that are not just terminals

The `:program-path` allows you to specify a different program for the
created terminal buffer. This particular example creates an ipython
inferior mode.

## Why the name navorski?

Viktor Navorski is a fictional character from the movie [The
Terminal](http://en.wikipedia.org/wiki/The_Terminal), that _lived_ in
a terminal. Got the pun right?

## License

```
 navorski.el - Making you live in the terminal

 Copyright (C) 2014  Roman Gonzalez and collaborators.
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
