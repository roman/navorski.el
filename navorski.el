;;; navorski.el --- Helping you live in the terminal, like Viktor did.

;; Copyright (C) 2013, 2014 Roman Gonzalez .
;; Copyright (C) 2013 Birdseye Software, Inc.

;; Author: Roman Gonzalez <romanandreg@gmail.com>, Tavis Rudd <tavis@birdseye-sw.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
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

(require 'time-stamp)
(require 'multi-term)
(require 'assoc)
(require 'dash)

;;; Code:

(defcustom navorski-buffer-name "terminal"
  "Default buffer name for terminal."
  :type 'string)

(defcustom navorski-verbose nil
  "Print debug information on message buffer"
  :type 'bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -navorski-term-reverse-search ()
  (interactive)
  (if (term-in-line-mode)
      (isearch-backward)
    (term-send-reverse-search-history)))

(defun -navorski-term-dabbrev ()
  (interactive)
  (let ((beg (point)))
    (dabbrev-expand nil)
    (kill-region beg (point)))
  (term-send-raw-string (substring-no-properties (current-kill 0))))

(defun -navorski-term-backward-kill-word ()
  (interactive)
  (if (term-in-line-mode)
      (backward-kill-word 1)
    (term-send-backward-kill-word)))

(defun -navorski-insert-path (file)
  "insert file"
  (interactive "FPath: ")
  (insert (replace-regexp-in-string (getenv "HOME") "~" (expand-file-name file))))

(defun -navorski-term-insert-path ()
  (interactive)
  (let ((beg (point)))
    (call-interactively '-navorski-insert-path)
    (kill-region beg (point)))
  (term-send-raw-string (substring-no-properties (current-kill 0))))

(defun -navorski-term-yank ()
  (interactive)
  (if (term-in-line-mode)
      (yank)
    (term-paste)))

(defun -navorski-interrupt-process ()
  (interactive)
  (term-send-raw-string (kbd "C-C")))

(setq term-bind-key-alist
      '(("C-c C-c" . -navorski-interrupt-process)
        ("C-x C-x" . term-send-raw)
        ("C-x C-e" . (lambda ()
                       (interactive)
                       (term-send-raw-string "\C-x\C-e")))
        ("C-s" . isearch-forward)
        ("C-r" . -navorski-term-reverse-search)
        ("C-m" . term-send-raw)

        ("M-/" . -navorski-term-dabbrev)
        ("M-RET" . find-file-at-point)
        ("M-DEL" . -navorski-term-backward-kill-word)
        ("M-`" . -navorski-term-insert-path)
        ("M-k" . term-send-raw-meta)
        ("M-y" . term-send-raw-meta)
        ("M-u" . term-send-raw-meta)
        ("C-M-k" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-k")))
        ("C-M-l" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-l")))
        ("C-M-d" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-d")))
        ("C-M-t" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-t")))
        ("M-h" . term-send-raw-meta)
        ("M-s" . term-send-raw-meta)
        ("M-t" . term-send-raw-meta)

        ("M-c" . term-send-raw-meta)
        ("M-l" . term-send-raw-meta)
        ("M-|" . term-send-raw-meta)

        ("M-f" . term-send-forward-word)
        ("M-b" . term-send-backward-word)
        ("M-o" . term-send-backspace)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("M-N" . term-send-backward-kill-word)
        ("M-r" . term-send-reverse-search-history)

        ("M-," . term-send-input)

        ("M-." . comint-dynamic-complete)
        ("" . term-send-backward-word)
        ("" . term-send-forward-word)
        ("M-d" . term-send-forward-kill-word)
        ("C-y" . -navorski-term-yank)

        ;; ("M-g" . -navorski-term-toggle-mode)
        ;; ("M-]" . -navorski-term-cd-dir-path)
        ;; ("M-;" . -navorski-term-insert-dir-path)
        ;; ("M-'" . -navorski-term-insert-file-path)
        ;; ("M-\"" . -navorski-term-toggle-filename-rel-abs)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -navorski-merge-alist (a1 a2)
  (let ((a1_ (copy-alist a1)))
    (dolist (x a2)
      (let ((r (assoc (car x) a1_)))
        (if (null r)
            (add-to-list 'a1_ x)
          (setf (cdr r) (cdr x)))))
    a1_))

(defun -navorski-get-shell-path (profile)
  (or (-navorski-profile-get profile :program-path)
      multi-term-program
      (getenv "SHELL")
      (getenv "ESHELL")
      "/bin/sh"))

(defun -navorski-indexed-buffer-name (buffer-name &optional index)
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
                     (-navorski-indexed-buffer-name
                      buffer-name
                      current-index))))
      (setq current-index (if term-count (1+ term-count) 1)))
    (-navorski-indexed-buffer-name buffer-name current-index)))

(defun -navorski-get-buffer-name (profile)
  (let ((buffer-name (format "%s"
                             (or (aget profile :buffer-name)
                                 (aget profile :profile-name)))))
    (if (aget profile :unique)
        buffer-name
      (-navorski-next-buffer-name buffer-name))))



(defun -navorski-decorate-multi-term-sentinel (profile term-buffer)
  (let* ((term-process (get-buffer-process term-buffer))
         (current-sentinel (process-sentinel term-process)))
    (set-process-sentinel term-process
                          `(lambda (proc change)
                             (if (string-match "exited abnormally with code \\([0-9]+\\)" change)
                                 (if (string-equal (match-string 1 change) "126")
                                     (message "navorski: %s" change)
                                   (funcall ',current-sentinel proc change))
                               (funcall ',current-sentinel proc change))))))

(defun -navorski-get-buffer (profile)
  "Get term buffer."
  (with-temp-buffer
    (let* ((shell-name   (-navorski-get-shell-path profile))
           (buffer-name  (-navorski-get-buffer-name profile))
           (program-args (-navorski-profile-get profile :program-args))
           (init-script  (-navorski-profile-get profile :init-script))
           (term-buffer  (or (and (aget profile :unique)
                                  (get-buffer buffer-name))
                             (if program-args
                                 (apply #'make-term
                                        buffer-name shell-name nil program-args)
                               (make-term buffer-name shell-name)))))
      (when navorski-verbose
        (message "navorski executing: %s %s"
                 shell-name
                 (or (and program-args
                          (mapconcat 'identity program-args " "))
                     "")))
      (with-current-buffer term-buffer
        (multi-term-internal)
        (-navorski-decorate-multi-term-sentinel profile term-buffer)
        (-map (lambda (s) (term-send-raw-string (concat s "\n")))
              init-script))
      (switch-to-buffer term-buffer)
      term-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav/term (&optional profile)
  (interactive)
  (-navorski-get-buffer profile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -navorski-profile-get (profile key)
  (let ((result (aget profile key )))
    (if (equal result key)
        nil
      result)))

(defun -navorski-profile-set (profile0 key val)
  (let* ((profile (copy-alist profile0))
         (pair (assoc key profile)))
    (if pair
        (progn
          (setf (cdr pair) val)
          profile)
      (append profile `((,key . ,val))))))

(defun -navorski-profile-modify (profile key f)
  (-navorski-profile-set
   profile
   key
   (funcall f (-navorski-profile-get profile key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; derived from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
;; stolen from http://github.com/tavisrudd/emacs.d
(defun -navorski-remote-term-setup-tramp-string (&optional remote-host)
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (concat "
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
"echo \"tramp initialized\""))


(defun -navorski-get-ssh-host (profile)
  (let ((user-host (split-string (aget profile :remote-host) "@")))
    (if (= (length user-host) 1)
        (car user-host)
      (nth 1 user-host))))

(defun -navorski-remote-term-setup-program-args (profile)
  (let ((remote-host (-navorski-profile-get profile :remote-host))
        (remote-port (-navorski-profile-get profile :remote-port))
        (remote-program-args (-navorski-profile-get profile :program-args))
        (remote-program-path (-navorski-get-shell-path profile)))
    (-navorski-profile-modify
     profile
     :program-args
     (lambda (args)
       (append (list "-t")
               (when remote-port
                 (list "-p" remote-port))
               (list remote-host)
               (list remote-program-path)
               remote-program-args)))))

(defun -navorski-remote-term-setup-tramp (profile)
  (let ((use-tramp (-navorski-profile-get profile :use-tramp)))
    (if use-tramp
        (-navorski-profile-modify
         profile
         :init-script
         (lambda (val)
           (append val (list (-navorski-remote-term-setup-tramp-string profile)))))
      profile)))

(defun -navorski-remote-term-setup-remote-host (profile)
  (-navorski-profile-modify
   profile
   :remote-host
   (lambda (remote-host)
     (or remote-host
         (read-from-minibuffer
          "SSH address (e.g user@host): " nil nil nil
          'nav/remote-term)))))

(defun -navorski-remote-term-setup-buffer-name (profile)
  (-navorski-profile-modify
   profile
   :buffer-name
   (lambda (buffer-name)
     (or buffer-name
         "remote-terminal"))))

(defun -navorski-remote-term-to-local-term (profile)
  (-> profile
    (-navorski-remote-term-setup-buffer-name)
    (-navorski-remote-term-setup-remote-host)
    (-navorski-remote-term-setup-program-args)
    (-navorski-profile-set :program-path "ssh")
    (-navorski-remote-term-setup-tramp)))

(defun nav/remote-term (&optional remote-profile)
  "Creates a multi-term in a remote host. A user + host (e.g
user@host) value will be required to perform the connection."
  (interactive)
  (-navorski-get-buffer
   (-navorski-remote-term-to-local-term remote-profile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -navorski-persistent-term-setup-session-name (profile)
  (-navorski-profile-modify
   profile
   :screen-session-name
   (lambda (session-name)
     (or session-name
         (read-from-minibuffer "GNU Screen session name: "
                               nil nil nil
                               'nav/persistent-term)))))

(defun -navorski-persistent-term-setup-program-args (profile)
  (let ((inner-program-path (-navorski-get-shell-path profile))
        (inner-program-args (-navorski-profile-get profile :program-args))
        (screen-name (-navorski-profile-get profile :screen-session-name))
        (screen-args (-navorski-profile-get profile :screen-args)))
    (when inner-program-args
      (message "navorski: can't have arguments for commands on GNU screen sessions"))
    (-navorski-profile-set
     profile
     :program-args
     (append (list "-x" "-R" "-S" screen-name "-s" inner-program-path)
             screen-args))))

(defun -navorski-persistent-term-to-local-term (profile)
  (-> profile
    (-navorski-persistent-term-setup-session-name)
    (-navorski-persistent-term-setup-program-args)
    (-navorski-profile-set :program-path "screen")))

(defun nav/persistent-term (&optional profile)
  "Creates a multi-term inside a GNU screen session. A screen
session name is required."
  (interactive)
  (-navorski-get-buffer
   (-navorski-persistent-term-to-local-term profile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav/remote-persistent-term (&optional profile)
  "Creates multi-term buffer on a GNU screen session in a remote
host. A user + host (e.g user@host) value is required as well as
a GNU screen session name."
  (interactive)
  (-navorski-get-buffer
   (-navorski-remote-term-to-local-term
    (-navorski-persistent-term-to-local-term profile))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DO NOT USE THIS FUNCTION - Use -navorski-get-terminal instead
(defun -navorski-profile-create-terminal (profile)
  (cond
   ((and (aget profile :remote-host)
         (aget profile :screen-session-name))
    (nav/remote-persistent-term profile))
   ((aget profile :remote-host)
    (nav/remote-term profile))
   ((aget profile :screen-session-name)
    (nav/persistent-term profile))
   (t (nav/term profile))))

(defun -navorski-profile-get-buffer (profile)
  (let ((term-buffer (get-buffer
                      (format "*%s*"
                              (aget profile :buffer-name)))))
    (save-window-excursion
      (if (not term-buffer)
          (-navorski-profile-create-terminal profile)
        term-buffer))))

(defun -navorski-profile-pop-to-buffer (profile)
  (pop-to-buffer (-navorski-profile-get-buffer profile)))

(defun -navorski-profile-kill-buffer (profile &optional kill-process)
  (let ((term-buffer (-navorski-profile-get-buffer profile)))
    (when kill-process
      (process-kill-without-query
       (get-buffer-process term-buffer)))
    (kill-buffer term-buffer)))

(defun -navorski-profile-send-string (profile str)
  (with-current-buffer (-navorski-profile-get-buffer profile)
    (term-send-raw-string (concat str "\n"))))

(defun -navorski-profile-send-region (profile start end)
  (-navorski-profile-send-string profile (buffer-substring start end)))

(defmacro -navorski-defterminal (profile-name profile)
  `(progn
     (define-minor-mode ,(intern (format "%s-terminal-mode" profile-name))
       ,(format "Minor mode for navorski terminal `%s'." profile-name)
       nil
       :group `,(intern "%s-navorski-terminal" profile-name)
       :keymap (make-sparse-keymap))

     (defun ,(intern (format "nav/%s-get-buffer" profile-name)) ()
       (interactive)
       (-navorski-profile-get-buffer ',profile))

     ;; nav/<profile-name>-pop-to-buffer
     (defun ,(intern (format "nav/%s-pop-to-buffer" profile-name)) ()
       (interactive)
       (-navorski-profile-pop-to-buffer ',profile))

     ;; nav/<profile-name>-send-region
     (defun ,(intern (format "nav/%s-send-region" profile-name)) (start end)
       (interactive "r")
       (-navorski-profile-send-region ',profile start end))

     ;; nav/<profile-name>-send-string
     (defun ,(intern (format "nav/%s-send-string" profile-name)) (raw-str)
       (interactive ,(format "sSend to %s: " profile-name))
       (-navorski-profile-send-string ',profile raw-str))

     ;; nav/<profile-name>-kill-buffer
     (defun ,(intern (format "nav/%s-kill-buffer" profile-name)) (&optional kill-process)
       (interactive)
       (-navorski-profile-kill-buffer ',profile kill-process))))

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

     - :use-tramp (bool)

     Enables tramp integration.

  This macro will generate:

     - a minor mode called <profile-name>-terminal-mode

     - nav/<profile-name>-get-buffer

       Creates a terminal buffer and returns it.

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
    :use-tramp nil)

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
    :use-tramp t)

  Generates:
  nav/root-production-create-buffer
  nav/root-production-pop-to-buffer
  nav/root-production-kill-buffer
  nav/root-production-send-string
  nav/root-production-send-region"

  (let* ((profile (-map (lambda (it) `(,(nth 0 it) . ,(nth 1 it)))
                        (-partition 2 (append args (list :unique t :profile-name `,profile-name))))))
    `(-navorski-defterminal ,profile-name ,profile)))

(put 'nav/defterminal 'lisp-indent-function 'defun)

;; End:
(provide 'navorski)
;;; navorski.el ends here
