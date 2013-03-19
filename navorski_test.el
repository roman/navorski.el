(require 'ert)
(require 'navorski)

(ert-deftest navorski-merge-alist ()
  (should (equal '((:hello . "mundo"))
                 (-navorski-merge-alist
                  '((:hello . "world"))
                  '((:hello . "mundo"))))))

(ert-deftest navorski-remote-term-to-local-term ()
  (let* ((profile '((:remote-host . "somehost")))
         (result (-navorski-remote-term-to-local-term profile)))
    (should (string-equal "ssh" (aget result :program-path)))
    (should (equal (list "-t" "\"/bin/bash\"" "somehost")
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

;; (ert-deftest navorski-remote-term-test ()
;;   "Test nav/term command"
;;   (let ((profile ((:buffer-name . "some-buffer")
;;                   (:program-path . "/bin/bash")
;;                   (:program-args . (list)))))
;;     (nav/term profile)))
