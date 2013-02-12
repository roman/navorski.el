;;; navorski.el --- Helping you live in the terminal, like Viktor did.

;; Copyright (C) 2013 Birdseye Software.

;; Author: Roman Gonzalez <roman@birdseye-sw.com>, Tavis Rudd <tavis@birdseye-sw.com>
;; Version: 0.1.0
;; Keywords: terminal

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  This program is
;; distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.  You should have received a copy of the
;; GNU General Public License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Require:

(require 'multi-term)
(require 'assoc)
(require 'dash)

;;; Code:

(defcustom navorski-buffer-name "terminal"
  "Default buffer name for terminal."
  :type 'string)

(defcustom navorski-unique-buffer nil
  "Tells navorski to create only one terminal buffer."
  :type 'bool)

(defcustom navorski-screen-session-name nil
  "Default GNU screen session name."
  :type 'string)

(defcustom navorski-program-path nil
  "Path for program to execute."
  :type '(repeat string))

(defcustom navorski-directory-path nil
  "Directory path where the terminal is going to be excuted."
  :type 'string)

(defcustom navorski-program-args nil
  "Arguments for program."
  :type '(repeat string))

(defcustom navorski-remote-host nil
  "Default remote host SSH address (e.g user@host)."
  :type 'string)

(defcustom navorski-setup-tramp t
  "Setups tramps in the remote terminal."
  :type 'bool)

(defcustom navorski-remote-port nil
  "Default SSH port."
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -navorski-get-shell-path (&optional shell-path)
  (or shell-path
      navorski-program-path
      (getenv "SHELL")
      (getenv "ESHELL")
      "/bin/sh"))

(defun -navorski-create-buffer-name (buffer-name &optional index)
  (if index
      (format "%s<%s>"
              buffer-name
              current-index)
    (format "%s" buffer-name)))

(defun -navorski-next-buffer-name (&optional buffer-name)
  (let* ((term-count        (length (multi-term-list)))
         (current-index     nil)
         (buffer-name       (or buffer-name
                                navorski-buffer-name)))
    (while (buffer-live-p
            (get-buffer
             (format "*%s*"
                     (-navorski-create-buffer-name
                      buffer-name
                      current-index))))
      (setq current-index (if term-count (1+ term-count) 1)))
    (-navorski-create-buffer-name buffer-name current-index)))

(defun -navorski-get-buffer (&optional buffer-name program-args unique?)
  "Get term buffer."
  (with-temp-buffer
    (let* ((shell-name (-navorski-get-shell-path))
           (unique? (or unique? navorski-unique-buffer))
           (buffer-name (or buffer-name navorski-buffer-name "terminal"))
           (buffer-name (if unique?
                            buffer-name
                          (-navorski-next-buffer-name buffer-name)))
           (current-directory (or default-directory
                                  (and navorski-directory-path
                                       (expand-file-name navorski-directory-path))))
           (program-args (or program-args
                             navorski-program-args))
           (term-buffer (get-buffer buffer-name))
           (term-buffer (or (and unique? term-buffer)
                            (if program-args
                                (apply #'make-term
                                       buffer-name shell-name nil program-args)
                              (make-term buffer-name shell-name)))))
      (message "%s %s" navorski-program-path navorski-program-args)
      (with-current-buffer term-buffer
        (multi-term-internal)
        (let* ((term-process (get-buffer-process term-buffer))
               (current-sentinel (process-sentinel term-process)))
          (set-process-sentinel term-process
                               `(lambda (proc change)
                                  (when (string-match "exited abnormally with code" change)
                                    (message "navorski: process exited abnormally")
                                    (sleep-for 1))
                                 (funcall ',current-sentinel proc change)))))
      (switch-to-buffer term-buffer)
      term-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; derived from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
;;; stolen from http://github.com/tavisrudd/emacs.d
(defun nav/setup-tramp (&optional remote-host)
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (interactive)
  (term-send-raw-string
   (concat "\n
function eterm_set_variables {\n"
    "local emacs_host=\"" (car (split-string (system-name) "\\.")) "\"\n"
    (if remote-host
        (format  "local tramp_hostname=\"%s\"\n" remote-host)
        "local tramp_hostname=${TRAMP_HOSTNAME-$(hostname)}\n")
    "if [[ $TERM == \"eterm-color\" ]]; then\n"
    "    if [[ $tramp_hostname != \"$emacs_host\" ]]; then\n"
    "       echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)}\n"
    "       echo -e \"\\033AnSiTh\" $tramp_hostname\n"
    "   fi\n"
    "   echo -e \"\\033AnSiTc\" $(pwd)\n"
    "elif [[ $TERM == \"screen\" || $TERM  == \"screen-256color\" ]]; then\n"
    "   if [[ $tramp_hostname != \"$emacs_host\" ]]; then\n"
    "       echo -e \"\\033P\\033AnSiTu\\033\\\\\" ${TRAMP_USERNAME-$(whoami)}\n"
    "       echo -e \"\\033P\\033AnSiTh\\033\\\\\" $tramp_hostname\n"
    "   fi\n"
    "   echo -e \"\\033P\\033AnSiTc\\033\\\\\" $(pwd)\n"
    "fi\n"
"}\n"
"function eterm_tramp_init {\n"
    "for temp in cd pushd popd; do\n"
    "   alias $temp=\"eterm_set_cwd $temp\"\n"
    "done\n"
    "# set hostname, user, and cwd now\n"
    "eterm_set_variables\n"
"}\n"
"function eterm_set_cwd {\n"
    "$@\n"
    "eterm_set_variables\n"
"}\n"
"eterm_tramp_init\n"
"export -f eterm_tramp_init\n"
"export -f eterm_set_variables\n"
"export -f eterm_set_cwd\n"
"clear\n"
"echo \"tramp initialized\"\n"
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav/term (&optional buffer-name program-path program-args unique?)
  (interactive)
  (let* ((navorski-program-path (or program-path
                                    navorski-program-path))
         (navorski-buffer-name (or buffer-name
                                   navorski-buffer-name))
         (navorski-program-args (or program-args
                                    navorski-program-args))
         (term-buffer (-navorski-get-buffer unique?)))
    term-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav/remote-term (&optional user+host buffer-name screen-shell unique?)
  "Creates a multi-term in a remote host. A user + host (e.g
user@host) value will be required to perform the connection."
  (interactive)
  (let* ((user+host (or user+host
                        navorski-remote-host
                        (read-from-minibuffer
                         "SSH address (e.g user@host): " nil nil nil
                         'nav/remote-term)))
         (screen-shell (-navorski-get-shell-path screen-shell))
         (screen-shell (if screen-shell
                           (list "-t" (format "\"%s\"" screen-shell))))
         (remote-port (if navorski-remote-port
                          (list "-p" navorski-remote-port)))
         (navorski-program-path "ssh")
         (navorski-buffer-name (or buffer-name
                                   navorski-buffer-name
                                   "remote-terminal"))
         (navorski-unique-buffer (or unique?
                                     navorski-unique-buffer))
         (navorski-program-args (append remote-port
                                        (list user+host)
                                        screen-shell))
         (term-buffer (-navorski-get-buffer)))
    (with-current-buffer term-buffer
      (let ((user-host (split-string user+host "@")))
        (when navorski-setup-tramp
          (if (= (length user-host) 1)
              (nav/setup-tramp (car user-host))
            (nav/setup-tramp (nth 1 user-host))))))
    term-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav/persistent-term (&optional session-name buffer-name screen-shell unique?)
  "Creates a multi-term inside a GNU screen session. A screen
session name is required."
  (interactive)
  (let* ((session-name (or session-name
                           navorski-screen-session-name
                           (read-from-minibuffer "Session name: " nil nil nil
                                                 'nav/persistent-term)))
         (screen-shell (-navorski-get-shell-path screen-shell))
         (navorski-unique-buffer (or unique?
                                     navorski-unique-buffer))
         (navorski-buffer-name (or buffer-name
                                     navorski-buffer-name
                                     session-name))
         (navorski-program-path "screen")
         (navorski-program-args (append (list "-x" "-R"
                                              "-S" session-name
                                              "-s" screen-shell))))
    (-navorski-get-buffer unique?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav/remote-persistent-term (&optional user+host session-name buffer-name screen-shell unique?)
  "Creates multi-term buffer on a GNU screen session in a remote
host. A user + host (e.g user@host) value is required as well as
a GNU screen session name."
  (interactive)
  (let* ((session-name (or session-name
                           navorski-screen-session-name
                           (read-from-minibuffer "Session name: ")))
         (screen-shell (-navorski-get-shell-path screen-shell))
         (user+host (or user+host
                        navorski-remote-host
                        (read-from-minibuffer "SSH address (e.g user@host): ")))
         (remote-port (if navorski-remote-port
                          (list "-p" navorski-remote-port)))
         (navorski-unique-buffer (or unique?
                                     navorski-unique-buffer))
         (navorski-buffer-name (or buffer-name
                                   navorski-buffer-name
                                   session-name))
         (navorski-program-path "ssh")
         (navorski-program-args (append
                                 remote-port
                                 (list user+host
                                       "-t"
                                       "screen"
                                       "-x"
                                       "-R"
                                       "-S"
                                       session-name
                                       "-s"
                                       screen-shell)))
         (term-buffer (-navorski-get-buffer unique?)))
    (with-current-buffer (-navorski-get-buffer)
      (let ((user-host (split-string navorski-remote-host "@")))
        (when navorski-setup-tramp
            (if (= (length user-host) 1)
                (nav/setup-tramp (car user-host))
              (nav/setup-tramp (nth 1 user-host))))))
    term-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -navorski-term-from-globals ()
  "Creates a terminal depending on global variables."
  (cond
   ((and navorski-remote-host
          navorski-screen-session-name)
    (nav/remote-persistent-term))
   (navorski-remote-host
    (nav/remote-term))
   (navorski-screen-session-name
    (nav/persistent-term))
   (t
    (nav/term))))

(defmacro nav/defterminal (profile-name &rest args)
  "Creates a unique terminal with specified settings.

   Possible settings are:

     - :buffer-name (string)

     The name of the terminal buffer (without surrounding
     *earmufs*)

     - :remote-host (string)

     An SSH address to connect to.

     - :remote-port (string)

     A different SSH port from the default one (22)

     - :program-path (string)

     A program that you want to execute (instead of /bin/bash)

     - :program-args (string)

     Program arguments to provide to program NOTE: doesn't work
     on persistent sessions.

     - :screen-session-name (string)

     GNU screen session to use for this terminal.

     - :init-script (string)

     Sends an initial string to the terminal.

     - :setup-tramp (bool)

     Enables tramp integration.

  This macro will generate:

     - a minor mode called <profile-name>-terminal-mode

     - nav/<profile-name>-create-buffer

       Creates the terminal buffer in the background.

     - nav/<profile-name>-pop-to-buffer

       Pops the terminal buffer (creates it in case is not existing
       via <profile-name>-create-buffer).

     - nav/<profile-name>-send-string

       Sends a raw string to the terminal buffer, in case it
       exists, otherwise does nothing

     - nav/<profile-name>-send-region

       Sends a region to the terminal buffer, in case it
       exists, otherwise does nothing

     - nav/<profile-name>-kill-buffer

       Kills the buffer for this terminal, in case it exists,
       otherwise does nothing.

  Examples:

  (nav/defterminal remote-irb
    :remote-host vagrant@33.33.33.10
    :program-path \"/usr/local/bin/irb\"
    :screen-session-name \"irb\"
    :setup-tramp nil)

  Generates:
  nav/remote-irb-create-buffer
  nav/remote-irb-pop-to-buffer
  nav/remote-irb-kill-buffer
  nav/remote-irb-send-string
  nav/remote-irb-send-region
  ;;;

  (nav/defterminal root-production
    :remote-host root@production-site.com
    :screen-session \"root\"
    :setup-tramp t)

  Generates:
  nav/root-production-create-buffer
  nav/root-production-pop-to-buffer
  nav/root-production-kill-buffer
  nav/root-production-send-string
  nav/root-production-send-region"
  (let* ((args (-map (lambda (it) `(,(nth 0 it) . ,(nth 1 it)))
                     (-partition 2 args)))
         (let-options `(let ((navorski-buffer-name ,(aget args :buffer-name))
                             (navorski-remote-host ,(aget args :remote-host))
                             (navorski-remote-port ,(aget args :remote-port))
                             (navorski-setup-tramp ,(aget args :setup-tramp t))
                             (navorski-program-path ,(aget args :program-path))
                             (navorski-program-args ,(aget args :program-args))
                             (navorski-screen-session-name ,(aget args :screen-session-name))
                             (navorski-unique-buffer t)
                             (init-script ,(aget args :init-script))))))

    `(progn

       (define-minor-mode ,(intern (format "%s-terminal-mode" profile-name))
         ,(format "Minor mode for navorski terminal `%s'." profile-name)
         nil
         :group `,(intern "%s-navorski-terminal" profile-name)
         :keymap (make-sparse-keymap))

       ;; nav/<profile-name>-create-buffer
       (defun ,(intern (format "nav/%s-create-buffer" profile-name)) ()
         (interactive)
         ,(append let-options
                  `((let ((term-buffer (or (get-buffer (format "*%s*" navorski-buffer-name))
                                            (-navorski-term-from-globals))))
                      (with-current-buffer term-buffer
                        (,(intern (format "%s-terminal-mode" profile-name)))
                        (when init-script
                          (term-send-raw-string init-script)))
                      term-buffer))))

       (defun ,(intern (format "nav/%s-get-buffer" profile-name)) ()
         (interactive)
         ,(append let-options
                  '((get-buffer (format "*%s*" navorski-buffer-name)))))

       ;; nav/<profile-name>-pop-to-buffer
       (defun ,(intern (format "nav/%s-pop-to-buffer" profile-name)) ()
          (interactive)
          ,(append let-options
                   `((let ((term-buffer (,(intern (format "nav/%s-create-buffer"
                                                          profile-name)))))
                       (pop-to-buffer (format "*%s*" navorski-buffer-name))))))

       ;; nav/<profile-name>-send-region
       (defun ,(intern (format "nav/%s-send-region" profile-name)) (start end)
         (interactive "r")
         ,(append let-options
                  '((let ((term-buffer (get-buffer (format "*%s*"
                                                           navorski-buffer-name))))
                      (when term-buffer
                        (term-send-region term-buffer start end))))))

       ;; nav/<profile-name>-send-string
       (defun ,(intern (format "nav/%s-send-string" profile-name)) (raw-str)
         (interactive ,(format "sSend to %s: " profile-name))
         ,(append let-options
                  '((let ((term-buffer (get-buffer (format "*%s*"
                                                           navorski-buffer-name))))
                      (when term-buffer
                        (with-current-buffer term-buffer
                          (term-send-raw-string raw-str)))))))

       ;; nav/<profile-name>-kill-buffer
       (defun ,(intern (format "nav/%s-kill-buffer" profile-name)) ()
          (interactive)
          ,(append let-options
                   '((let ((term-buffer (get-buffer (format "*%s*"
                                                            navorski-buffer-name))))
                       (when term-buffer
                         (kill-buffer term-buffer))))))

       )))

;; End:
(provide 'navorski)
;;; navorski.el ends here
