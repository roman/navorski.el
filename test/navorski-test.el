(require 'ert)
(require 'navorski)

(ert-deftest navorski-merge-alist ()
  (should (equal '((:hello . "mundo"))
                 (-navorski-merge-alist
                  '((:hello . "world"))
                  '((:hello . "mundo"))))))

(ert-deftest navorski-profile-set ()
  (should (equal
           '((:program-path . "ssh"))
           (-navorski-profile-set '() :program-path "ssh"))))

(ert-deftest navorski-profile-get ()
  (should (equal
           "ssh"
           (-navorski-profile-get '((:program-path . "ssh")) :program-path)))
  (should (not
           (-navorski-profile-get '((:hello . nil)) :hello))))

(ert-deftest navorski-profile-modify ()
  (let ((profile '((:hello . 1))))
    (should (equal
             '((:hello . 2))
             (-navorski-profile-modify profile
                                       :hello
                                       (lambda (x) (+ x 1)))))))

(ert-deftest navorski-remote-term-setup-program-args-test ()
  (should (equal
           '((:program-args . ("-t" "/bin/bash")))
           (-navorski-remote-term-setup-program-args '()))))

(ert-deftest navorski-remote-term-setup-tramp ()
  (let* ((profile '((:use-tramp . nil))))
    (should (null (-navorski-profile-get profile :use-tramp)))
    (should (equal
             '((:use-tramp . nil))
             (-navorski-remote-term-setup-tramp profile))))

  (let* ((input-profile '((:use-tramp . t)))
         (output-profile (-navorski-remote-term-setup-tramp input-profile)))

    (should (-navorski-profile-get output-profile :init-script))
    (should (listp (-navorski-profile-get output-profile :init-script)))
    (should (equal
             `((:use-tramp . t)
               (:init-script . ,(list (-navorski-remote-term-setup-tramp-string input-profile))))
             output-profile))))

(ert-deftest navorski-remote-term-to-local-term ()
  (let* ((input-profile '((:remote-host . "user@remote")))
         (output-profile (-navorski-remote-term-to-local-term input-profile)))

    (should (equal
             `((:remote-host . "user@remote")
               (:buffer-name . "remote-terminal")
               (:program-args . ("-t" "user@remote" "/bin/bash"))
               (:program-path . "ssh"))
             output-profile))))

(ert-deftest navorski-remote-term-setup-program-args-test ()
  (let ((profile '((:screen-session-name . "foo"))))
    (should (equal
             '((:screen-session-name . "foo")
               (:program-args . ("-x" "-R" "-S" "foo" "-s" "/bin/bash")))
             (-navorski-persistent-term-setup-program-args profile))))

  (let ((profile '((:screen-session-name . "foo")
                   (:screen-args . ("-e^Tt")))))
    (should (equal
             '((:screen-session-name . "foo")
               (:screen-args . ("-e^Tt"))
               (:program-args . ("-x" "-R" "-S" "foo" "-s" "/bin/bash" "-e^Tt")))
             (-navorski-persistent-term-setup-program-args profile)))))

(ert-deftest navorski-remote-term-to-local-term ()
  (let* ((profile '((:remote-host . "somehost")))
         (result (-navorski-remote-term-to-local-term profile)))
    (should (string-equal "ssh" (aget result :program-path)))
    (should (equal (list "-t" "somehost" "/bin/bash")
                   (aget result :program-args)))))

(ert-deftest navorski-get-buffer-name-test ()
  "Checks that buffer name is being returned correctly"
  (should (string-equal (-navorski-get-buffer-name '((:buffer-name . "other")))
                        "other"))
  (should (string-equal (-navorski-get-buffer-name (list))
                        "terminal")))

(ert-deftest navorski-get-buffer-test ()
  "Checks buffer is being created successfuly"
  (save-window-excursion
    ;; single name
    (let* ((profile '())
           (term-buffer (-navorski-get-buffer profile)))
      (should (string-equal (buffer-name term-buffer) "*terminal*"))

      (process-kill-without-query
       (get-buffer-process term-buffer))
      (kill-buffer term-buffer))

    ;; repeated multiple times
    (let* ((profile '())
           (term-buffer1 (-navorski-get-buffer profile))
           (term-buffer2 (-navorski-get-buffer profile)))

      (should (string-equal "*terminal*" (buffer-name term-buffer1)))
      (should (string-equal "*terminal<1>*" (buffer-name term-buffer2)))

      (process-kill-without-query
       (get-buffer-process term-buffer1))
      (kill-buffer term-buffer1)

      (process-kill-without-query
       (get-buffer-process term-buffer2))
      (kill-buffer term-buffer2))

    ;; repeated multiple times with unique option
    (let* ((profile '((:unique . t)))
           (term-buffer1 (-navorski-get-buffer profile))
           (term-buffer2 (-navorski-get-buffer profile)))

      (should (equal (get-buffer "*terminal*") term-buffer1))
      (should (equal term-buffer1  term-buffer2))

      (process-kill-without-query
       (get-buffer-process term-buffer1))
      (kill-buffer term-buffer1))))
