;;; -*- lexical-binding: t -*-

(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
(load "/Users/mayman03/Code/acl2/emacs/emacs-acl2.el")

(setq acl2-doc-mode-hook nil)

(add-hook 'acl2-doc-mode-hook
          (lambda ()
            (set-face-attribute 'acl2-doc-link-face
                                nil :inherit 'link)
            (setq font-lock-defaults '(acl2-doc-keywords t))
            (set (make-local-variable 'font-lock-keyword-face)
                 'acl2-doc-link-face)))

(defun may/process-kill-buffer-query-function ()
  (let ((process (get-buffer-process (current-buffer))))
    (or (not process)
        (not (memq (process-status process) '(run stop open listen)))
        (if (equal "kill" (read-string
                           (format "Buffer %S has a running process; to kill it type 'kill' and return: "
                                   (buffer-name (current-buffer)))))
            t
          nil))))

(setq kill-buffer-query-functions (delete 'process-kill-buffer-query-function kill-buffer-query-functions))
(add-to-list 'kill-buffer-query-functions 'may/process-kill-buffer-query-function)

(defun acl2-highlight-until-prompt ()
  (acl2-beginning-of-def)
  (let* ((pt1 (point))
         (pt2 (progn (forward-sexp) (point)))
         (ovl (make-overlay pt1 pt2)))
    (overlay-put ovl 'face 'highlight)
    (letrec ((hookfun (lambda (val)
                        (when (string-match "^[A-Z0-9]* [pPs!]*>$" val)
                          (sit-for 0.1)
                          (delete-overlay ovl)
                          (remove-hook 'comint-output-filter-functions hookfun))
                        ;; (save-selected-window
                        ;;   (select-window (get-buffer-window (get-buffer *acl2-shell*) t))
                        ;;   (end-of-buffer))
                        val)))
      (add-hook 'comint-output-filter-functions hookfun))))

(defun acl2-admit-event-dont-move ()
  (interactive)
  (save-selected-window
    (enter-theorem-elsewhere)
    (comint-send-input))
  (save-excursion (acl2-highlight-until-prompt)))

(defun acl2-admit-event ()
  (interactive)
  (acl2-admit-event-dont-move)
  (acl2-beginning-of-def)
  (forward-sexp)
  (forward-sexp)
  (when (not (equal (point) (point-max)))
    (backward-sexp)))

(defvar may/lisp-defuns '("bvecthm" "bitthm" "skip-defthm"
                          "skip-defthmd" "local-skip-defthm"
                          "local-skip-defthmd" "defthm[d]?-nl"
                          "defthm[d]?"
                          "define"
                          "gl::.*" "defundd"
                          "def-gl-*"))

(defun may/quoted-or-kwd-use (levels)
  (or (save-excursion
        (catch 'quoted
          (dolist (pos levels)
            (goto-char pos)
            (when (eq (char-before) ?\')
              (throw 'quoted t)))))
      ;; Check for :instance keyword
      (save-excursion
        (skip-chars-backward "^[:alnum:]")
        (let ((pt (point)))
          (equal (buffer-substring (max (- pt 9) 1) pt) ":instance")))
      ;; Check for e/d macros at level -2
      (and (> (length levels) 1)
           (save-excursion
             (goto-char (cadr levels))
             (skip-chars-forward "^[:alnum:]")
             (looking-at-p "e/d")))
      ;; Check for :use keyword at level -1
      (and (> (length levels) 0)
           (save-excursion
             (goto-char (car levels))
             (skip-chars-backward "^[:alnum:]")
             (let ((pt (point)))
               (equal (buffer-substring (max (- pt 4) 1) pt) ":use"))))))

(defun may/lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (cond
     ((let ((indent)
            (i 0)
            (rgxp (mapconcat 'identity may/lisp-defuns "\\|")))
        (unless (equal rgxp "")
          (save-excursion
            (goto-char (1+ (elt state 1)))
            (when (looking-at rgxp)
              (lisp-indent-defform state indent-point))))))
     ((let ((levels (reverse (elt state 9))))
        (when (may/quoted-or-kwd-use levels)
          (save-excursion
            (goto-char (1+ (elt state 1)))
            (current-column)))))
     ((when (and (looking-at ":")
                 (save-excursion
                   (goto-char indent-point)
                   (looking-at " *:")))
        (lisp-indent-specform 0 state indent-point normal-indent)))
     ((lisp-indent-function indent-point state)))))

(define-key function-key-map [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map [(meta control shift iso-lefttab)] [(meta control shift tab)])

(defvar acl2-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    (define-key map (kbd "C-<tab>") 'acl2-admit-event)
    (define-key map (kbd "C-S-<tab>") 'acl2-admit-event-dont-move)
    map))

;; (define-derived-mode acl2-lisp-mode lisp-mode "ACL2 Lisp"
;;   "Major mode for editing Lisp files and interacting with ACL2."
;;   (setq-local lisp-indent-function #'may/lisp-indent-function)
;;   ;; (defun set-acl2-symbol-props ()
;;   ;;   (put 'local-defthm  'lisp-indent-function 'defun)
;;   ;;   (put 'local-defthmd 'lisp-indent-function 'defun)
;;   ;;   (put 'local-defund  'lisp-indent-function 'defun)
;;   ;;   (put 'local-defun  'lisp-indent-function 'defun)
;;   ;;   (put 'defund 'lisp-indent-function 'defun)
;;   ;;   (put 'case         'lisp-indent-function 'defun)
;;   ;;   (put 'CASE         'lisp-indent-function 'defun)
;;   ;;   (put 'case!        'lisp-indent-function 'defun)
;;   ;;   (put 'CASE!        'lisp-indent-function 'defun)
;;   ;;   (put 'case-match   'lisp-indent-function 'defun)
;;   ;;   (put 'CASE-MATCH   'lisp-indent-function 'defun)
;;   ;;   (put 'dolist       'lisp-indent-function 'defun)
;;   ;;   (put 'DOLIST       'lisp-indent-function 'defun)
;;   ;;   (put 'er@par       'lisp-indent-function 'defun)
;;   ;;   (put 'ER@PAR       'lisp-indent-function 'defun)
;;   ;;   (put 'warning$@par 'lisp-indent-function 'defun)
;;   ;;   (put 'WARNING$@PAR 'lisp-indent-function 'defun)

;;   ;;   ; Jared Davis has contributed the following.  It is tempting to
;;   ;;   ; comment out those that aren't part of ACL2, but rather, are defined
;;   ;;   ; in books, since for those, a given name might have different
;;   ;;   ; reasonable syntax for different books.  However, in practice is
;;   ;;   ; seems unlikely that these will cause problems; if that assumption
;;   ;;   ; turns out to be wrong, perhaps a new Emacs file should be created
;;   ;;   ; for the books, and book-specific forms below should be moved there.
;;   ;;   (put 'B* 'lisp-indent-function 1)
;;   ;;   (put 'b* 'lisp-indent-function 1)
;;   ;;   (put 'ENCAPSULATE       'lisp-indent-function 'defun)
;;   ;;   (put 'encapsulate       'lisp-indent-function 'defun)
;;   ;;   (put 'MV-LET       'lisp-indent-function 'defun)
;;   ;;   (put 'mv-let       'lisp-indent-function 'defun)
;;   ;;   (put 'PATTERN-MATCH       'lisp-indent-function 'defun)
;;   ;;   (put 'pattern-match       'lisp-indent-function 'defun)
;;   ;;   (put 'PATTERN-MATCH-LIST       'lisp-indent-function 'defun)
;;   ;;   (put 'pattern-match-list       'lisp-indent-function 'defun)
;;   ;;   (put 'VERIFY-GUARDS  'lisp-indent-function 'defun)
;;   ;;   (put 'verify-guards  'lisp-indent-function 'defun)
;;   ;;   (put 'VERIFY-TERMINATION  'lisp-indent-function 'defun)
;;   ;;   (put 'verify-termination  'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-ACL2-CHANNELS-BOUND 'lisp-indent-function 'defun)
;;   ;;   (put 'with-acl2-channels-bound 'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-FAST-ALIST      'lisp-indent-function 'defun)
;;   ;;   (put 'with-fast-alist      'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-FAST-ALISTS      'lisp-indent-function 'defun)
;;   ;;   (put 'with-fast-alists      'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-LOCAL-STOBJ      'lisp-indent-function 'defun)
;;   ;;   (put 'with-local-stobj      'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-OPEN-FILE 'lisp-indent-function 'defun)
;;   ;;   (put 'with-open-file 'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-OUTPUT 'lisp-indent-function 'defun)
;;   ;;   (put 'with-output 'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-OUTPUT! 'lisp-indent-function 'defun)
;;   ;;   (put 'with-output! 'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-OUTPUT-TO 'lisp-indent-function 'defun)
;;   ;;   (put 'with-output-to 'lisp-indent-function 'defun)
;;   ;;   (put 'WITH-STDOUT 'lisp-indent-function 'defun)
;;   ;;   (put 'with-stdout 'lisp-indent-function 'defun)
;;   ;;   ; Keshav Kini suggested special handling for er-let*; we add the
;;   ;;   ; following, long used by Matt K.
;;   ;;   (put 'er-let* 'lisp-indent-function 1)
;;   ;;   (put 'ER-LET* 'lisp-indent-function 1))
;;   ;; (defun clear-acl2-symbol-props ()
;;   ;;   (put 'local-defthm  'lisp-indent-function 'nil)
;;   ;;   (put 'local-defthmd 'lisp-indent-function 'nil)
;;   ;;   (put 'local-defund  'lisp-indent-function 'nil)
;;   ;;   (put 'local-defun  'lisp-indent-function 'nil)
;;   ;;   (put 'defund 'lisp-indent-function 'nil)
;;   ;;   (put 'case         'lisp-indent-function 'nil)
;;   ;;   (put 'CASE         'lisp-indent-function 'nil)
;;   ;;   (put 'case!        'lisp-indent-function 'nil)
;;   ;;   (put 'CASE!        'lisp-indent-function 'nil)
;;   ;;   (put 'case-match   'lisp-indent-function 'nil)
;;   ;;   (put 'CASE-MATCH   'lisp-indent-function 'nil)
;;   ;;   (put 'dolist       'lisp-indent-function 'nil)
;;   ;;   (put 'DOLIST       'lisp-indent-function 'nil)
;;   ;;   (put 'er@par       'lisp-indent-function 'nil)
;;   ;;   (put 'ER@PAR       'lisp-indent-function 'nil)
;;   ;;   (put 'warning$@par 'lisp-indent-function 'nil)
;;   ;;   (put 'WARNING$@PAR 'lisp-indent-function 'nil)
;;   ;;   ; Jared Davis has contributed the following.  It is tempting to
;;   ;;   ; comment out those that aren't part of ACL2, but rather, are defined
;;   ;;   ; in books, since for those, a given name might have different
;;   ;;   ; reasonable syntax for different books.  However, in practice is
;;   ;;   ; seems unlikely that these will cause problems; if that assumption
;;   ;;   ; turns out to be wrong, perhaps a new Emacs file should be created
;;   ;;   ; for the books, and book-specific forms below should be moved there.
;;   ;;   (put 'B* 'lisp-indent-function nil)
;;   ;;   (put 'b* 'lisp-indent-function nil)
;;   ;;   (put 'ENCAPSULATE       'lisp-indent-function 'nil)
;;   ;;   (put 'encapsulate       'lisp-indent-function 'nil)
;;   ;;   (put 'MV-LET       'lisp-indent-function 'nil)
;;   ;;   (put 'mv-let       'lisp-indent-function 'nil)
;;   ;;   (put 'PATTERN-MATCH       'lisp-indent-function 'nil)
;;   ;;   (put 'pattern-match       'lisp-indent-function 'nil)
;;   ;;   (put 'PATTERN-MATCH-LIST       'lisp-indent-function 'nil)
;;   ;;   (put 'pattern-match-list       'lisp-indent-function 'nil)
;;   ;;   (put 'VERIFY-GUARDS  'lisp-indent-function 'nil)
;;   ;;   (put 'verify-guards  'lisp-indent-function 'nil)
;;   ;;   (put 'VERIFY-TERMINATION  'lisp-indent-function 'nil)
;;   ;;   (put 'verify-termination  'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-ACL2-CHANNELS-BOUND 'lisp-indent-function 'nil)
;;   ;;   (put 'with-acl2-channels-bound 'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-FAST-ALIST      'lisp-indent-function 'nil)
;;   ;;   (put 'with-fast-alist      'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-FAST-ALISTS      'lisp-indent-function 'nil)
;;   ;;   (put 'with-fast-alists      'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-LOCAL-STOBJ      'lisp-indent-function 'nil)
;;   ;;   (put 'with-local-stobj      'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-OPEN-FILE 'lisp-indent-function 'nil)
;;   ;;   (put 'with-open-file 'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-OUTPUT 'lisp-indent-function 'nil)
;;   ;;   (put 'with-output 'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-OUTPUT! 'lisp-indent-function 'nil)
;;   ;;   (put 'with-output! 'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-OUTPUT-TO 'lisp-indent-function 'nil)
;;   ;;   (put 'with-output-to 'lisp-indent-function 'nil)
;;   ;;   (put 'WITH-STDOUT 'lisp-indent-function 'nil)
;;   ;;   (put 'with-stdout 'lisp-indent-function 'nil)
;;   ;;   ; Keshav Kini suggested special handling for er-let*; we add the
;;   ;;   ; following, long used by Matt K.
;;   ;;   (put 'er-let* 'lisp-indent-function nil)
;;   ;;   (put 'ER-LET* 'lisp-indent-function nil))
;;   ;; (set-acl2-symbol-props)
;;   ;; (defun clear-props-hook-fn ()
;;   ;;   (when (eq major-mode 'acl2-lisp-mode)
;;   ;;     (clear-acl2-symbol-props)))
;;   ;; (add-hook 'change-major-mode-hook #'clear-props-hook-fn)
;;   )




(defun set-acl2-symbol-props ()
  (put 'local-defthm  'lisp-indent-function 'defun)
  (put 'local-defthmd 'lisp-indent-function 'defun)
  (put 'local-defund  'lisp-indent-function 'defun)
  (put 'local-defun  'lisp-indent-function 'defun)
  (put 'defund 'lisp-indent-function 'defun)
  (put 'case         'lisp-indent-function 'defun)
  (put 'CASE         'lisp-indent-function 'defun)
  (put 'case!        'lisp-indent-function 'defun)
  (put 'CASE!        'lisp-indent-function 'defun)
  (put 'case-match   'lisp-indent-function 'defun)
  (put 'CASE-MATCH   'lisp-indent-function 'defun)
  (put 'dolist       'lisp-indent-function 'defun)
  (put 'DOLIST       'lisp-indent-function 'defun)
  (put 'er@par       'lisp-indent-function 'defun)
  (put 'ER@PAR       'lisp-indent-function 'defun)
  (put 'warning$@par 'lisp-indent-function 'defun)
  (put 'WARNING$@PAR 'lisp-indent-function 'defun)

                                        ; Jared Davis has contributed the following.  It is tempting to
                                        ; comment out those that aren't part of ACL2, but rather, are defined
                                        ; in books, since for those, a given name might have different
                                        ; reasonable syntax for different books.  However, in practice is
                                        ; seems unlikely that these will cause problems; if that assumption
                                        ; turns out to be wrong, perhaps a new Emacs file should be created
                                        ; for the books, and book-specific forms below should be moved there.
  (put 'B* 'lisp-indent-function 1)
  (put 'b* 'lisp-indent-function 1)
  (put 'ENCAPSULATE       'lisp-indent-function 'defun)
  (put 'encapsulate       'lisp-indent-function 'defun)
  (put 'MV-LET       'lisp-indent-function 'defun)
  (put 'mv-let       'lisp-indent-function 'defun)
  (put 'PATTERN-MATCH       'lisp-indent-function 'defun)
  (put 'pattern-match       'lisp-indent-function 'defun)
  (put 'PATTERN-MATCH-LIST       'lisp-indent-function 'defun)
  (put 'pattern-match-list       'lisp-indent-function 'defun)
  (put 'VERIFY-GUARDS  'lisp-indent-function 'defun)
  (put 'verify-guards  'lisp-indent-function 'defun)
  (put 'VERIFY-TERMINATION  'lisp-indent-function 'defun)
  (put 'verify-termination  'lisp-indent-function 'defun)
  (put 'WITH-ACL2-CHANNELS-BOUND 'lisp-indent-function 'defun)
  (put 'with-acl2-channels-bound 'lisp-indent-function 'defun)
  (put 'WITH-FAST-ALIST      'lisp-indent-function 'defun)
  (put 'with-fast-alist      'lisp-indent-function 'defun)
  (put 'WITH-FAST-ALISTS      'lisp-indent-function 'defun)
  (put 'with-fast-alists      'lisp-indent-function 'defun)
  (put 'WITH-LOCAL-STOBJ      'lisp-indent-function 'defun)
  (put 'with-local-stobj      'lisp-indent-function 'defun)
  (put 'WITH-OPEN-FILE 'lisp-indent-function 'defun)
  (put 'with-open-file 'lisp-indent-function 'defun)
  (put 'WITH-OUTPUT 'lisp-indent-function 'defun)
  (put 'with-output 'lisp-indent-function 'defun)
  (put 'WITH-OUTPUT! 'lisp-indent-function 'defun)
  (put 'with-output! 'lisp-indent-function 'defun)
  (put 'WITH-OUTPUT-TO 'lisp-indent-function 'defun)
  (put 'with-output-to 'lisp-indent-function 'defun)
  (put 'WITH-STDOUT 'lisp-indent-function 'defun)
  (put 'with-stdout 'lisp-indent-function 'defun)
                                        ; Keshav Kini suggested special handling for er-let*; we add the
                                        ; following, long used by Matt K.
  (put 'er-let* 'lisp-indent-function 1)
  (put 'ER-LET* 'lisp-indent-function 1))

(defun clear-acl2-symbol-props ()
  (put 'local-defthm  'lisp-indent-function 'nil)
  (put 'local-defthmd 'lisp-indent-function 'nil)
  (put 'local-defund  'lisp-indent-function 'nil)
  (put 'local-defun  'lisp-indent-function 'nil)
  (put 'defund 'lisp-indent-function 'nil)
  (put 'define 'lisp-indent-function 'nil)
  (put 'case         'lisp-indent-function 'nil)
  (put 'CASE         'lisp-indent-function 'nil)
  (put 'case!        'lisp-indent-function 'nil)
  (put 'CASE!        'lisp-indent-function 'nil)
  (put 'case-match   'lisp-indent-function 'nil)
  (put 'CASE-MATCH   'lisp-indent-function 'nil)
  (put 'dolist       'lisp-indent-function 'nil)
  (put 'DOLIST       'lisp-indent-function 'nil)
  (put 'er@par       'lisp-indent-function 'nil)
  (put 'ER@PAR       'lisp-indent-function 'nil)
  (put 'warning$@par 'lisp-indent-function 'nil)
  (put 'WARNING$@PAR 'lisp-indent-function 'nil)
                                        ; Jared Davis has contributed the following.  It is tempting to
                                        ; comment out those that aren't part of ACL2, but rather, are defined
                                        ; in books, since for those, a given name might have different
                                        ; reasonable syntax for different books.  However, in practice is
                                        ; seems unlikely that these will cause problems; if that assumption
                                        ; turns out to be wrong, perhaps a new Emacs file should be created
                                        ; for the books, and book-specific forms below should be moved there.
  (put 'B* 'lisp-indent-function nil)
  (put 'b* 'lisp-indent-function nil)
  (put 'ENCAPSULATE       'lisp-indent-function 'nil)
  (put 'encapsulate       'lisp-indent-function 'nil)
  (put 'MV-LET       'lisp-indent-function 'nil)
  (put 'mv-let       'lisp-indent-function 'nil)
  (put 'PATTERN-MATCH       'lisp-indent-function 'nil)
  (put 'pattern-match       'lisp-indent-function 'nil)
  (put 'PATTERN-MATCH-LIST       'lisp-indent-function 'nil)
  (put 'pattern-match-list       'lisp-indent-function 'nil)
  (put 'VERIFY-GUARDS  'lisp-indent-function 'nil)
  (put 'verify-guards  'lisp-indent-function 'nil)
  (put 'VERIFY-TERMINATION  'lisp-indent-function 'nil)
  (put 'verify-termination  'lisp-indent-function 'nil)
  (put 'WITH-ACL2-CHANNELS-BOUND 'lisp-indent-function 'nil)
  (put 'with-acl2-channels-bound 'lisp-indent-function 'nil)
  (put 'WITH-FAST-ALIST      'lisp-indent-function 'nil)
  (put 'with-fast-alist      'lisp-indent-function 'nil)
  (put 'WITH-FAST-ALISTS      'lisp-indent-function 'nil)
  (put 'with-fast-alists      'lisp-indent-function 'nil)
  (put 'WITH-LOCAL-STOBJ      'lisp-indent-function 'nil)
  (put 'with-local-stobj      'lisp-indent-function 'nil)
  (put 'WITH-OPEN-FILE 'lisp-indent-function 'nil)
  (put 'with-open-file 'lisp-indent-function 'nil)
  (put 'WITH-OUTPUT 'lisp-indent-function 'nil)
  (put 'with-output 'lisp-indent-function 'nil)
  (put 'WITH-OUTPUT! 'lisp-indent-function 'nil)
  (put 'with-output! 'lisp-indent-function 'nil)
  (put 'WITH-OUTPUT-TO 'lisp-indent-function 'nil)
  (put 'with-output-to 'lisp-indent-function 'nil)
  (put 'WITH-STDOUT 'lisp-indent-function 'nil)
  (put 'with-stdout 'lisp-indent-function 'nil)
                                        ; Keshav Kini suggested special handling for er-let*; we add the
                                        ; following, long used by Matt K.
  (put 'er-let* 'lisp-indent-function nil)
  (put 'ER-LET* 'lisp-indent-function nil))


(define-minor-mode acl2-lisp-mode
  "Toggles ACL2 editing and interactive features"
  :global nil
  :lighter " ACL2"
  :keymap acl2-lisp-mode-map
  (if acl2-lisp-mode
      (progn
        (setq-local lisp-indent-function #'may/lisp-indent-function)
        (set-acl2-symbol-props))
    (setq-local lisp-indent-function #'lisp-indent-function)
    (clear-acl2-symbol-props)))

;; ACL2:2 ends here

(provide 'init-acl2)

;; init-acl2.el ends here
