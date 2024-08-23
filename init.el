;; -*- lexical-binding: t -*-

;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(setq custom-file "~/.config/emacs/custom-settings.el")

(load custom-file t)

;;; Garbage collection

;; A large `gc-cons-threshold` may cause freezing and stuttering
;; during long-term interactive use.
;; If you experience freezing, decrease this amount, if you experience
;; stuttering, increase this amount.
(defvar better-gc-cons-threshold 134217728 ; 256mb
  "The default value to use for `gc-cons-threshold'.

;; If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;; Garbage Collect when Emacs is out of focus and avoid garbage
;; collection when using minibuffer.
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;;; Straight.el configuration
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

;;; Diminish minor modes
(use-package diminish)

;; Add Elisp path to load-path
;; As long as elisp file is in directory in a load-path, emacs can find it.

(add-to-list 'load-path "~/.config/emacs/elisp")
(setq use-package-verbose t)

;; TODO: Not sure if required
(require 'use-package)

;; Personal Information
(setq user-full-name "Mayank Manjrekar"
      user-mail-address "mayank.manjrekar2@arm.com")

;; System information
(defvar macos-p
  (eq system-type 'darwin))
(defvar linux-p
  (eq system-type 'gnu/linux))

;;; Exec path:
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize)
  :if macos-p
  :config
  (dolist (var '("SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(use-package emacs
  :preface
  ;; Function to reload configuration
  (defun my-reload-emacs-configuration ()
    (interactive)
    (load-file "~/.config/emacs/init.el"))
  :config
  ;; A buffer can get out of sync with respect to its visited file on
  ;; disk if that file is changed by another program. To keep it up to
  ;; date, you can enable Auto Revert mode
  (global-auto-revert-mode)

;;; Backups
  ;; By default, Emacs saves backup files in the current directory.
  (setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-backup-directory-alist
                 (cons tramp-file-name-regexp nil)))

;;; Save lots of backups!
  (setq delete-old-versions -1)
  (setq version-control t)
  (setq vc-make-backup-files t)
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;;; Disabling toolbar
  (tool-bar-mode -1)

;;; Change "yes or no" to "y or n"
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq use-dialog-box nil)

;;; Keyboard mapping on MacOs
  (when macos-p
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'meta))

  

;;; Minibuffer editing
  ;;Sometimes you want to be able to do fancy things with the text that
  ;;you're entering into the minibuffer. Sometimes you just want to be
  ;;able to read it, especially when it comes to lots of text. This
  ;;binds C-M-e in a minibuffer) so that you can edit the contents of
  ;;the minibuffer before submitting it.
  (use-package miniedit
    :commands minibuffer-edit
    :init (miniedit-install))

;;; Killing text
  (setq kill-ring-max 1000)
  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))
  
;;; Column number mode
  (column-number-mode 1)

  ;;; Scratch buffer
  (setq initial-scratch-message "")
  (setq initial-major-mode 'text-mode)
  ;;; Startup screen
  (setq inhibit-startup-screen t)
  ;;; top-join-lines
  ;; (defalias 'my/top-join-line
  ;;  (kmacro "<down> M-^ C-e"))
  ;; :bind
  ;; ("M-V" . #'my/top-join-line)
  ;; :bind
  ;; (
  ;;  ;; TODO:
  ;;  ;; ("C-c C-c r" . #'my-reload-emacs-configuration)
  ;;  )
  )


;; Zsh config
(use-package emacs
  :preface
  (defun my-shell-mode-hook ()
    (setq comint-input-ring-file-name "~/.zsh_history")
                                        ; Ignore timestamps in history file.  Assumes that zsh
                                        ; EXTENDED_HISTORY option is in use.
    (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
    (comint-read-input-ring t))
  :hook
  (shell-mode . my-shell-mode-hook)
  :config
  
   ; Remember lots of previous commands in shell-mode
  (setq comint-input-ring-size 100000))

;;; Emacs Lisp
;; Autocompile, but don't interrupt me with native compilation warnings. 
(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; TODO: see what this does
(use-package emacs
  :config
  (setq native-comp-async-report-warnings-errors nil)

  (setq eval-expression-print-length nil)
  (setq print-length nil)
  (setq edebug-print-length nil)
  (defun my-set-sentence-end-double-space ()
    (setq-local sentence-end-double-space t))
  (add-hook 'emacs-lisp-mode-hook
            'my-set-sentence-end-double-space)

;;; Override function
;;; TODO: try this
  (defun my-override-function (symbol)
    (interactive (list (completing-read
                        "Function: "
                        #'help--symbol-completion-table
                        #'fboundp
                        'confirm nil nil)))
    (let (function-body function-name)
      (save-window-excursion
        (find-function (intern symbol))
        (setq function-name (lisp-current-defun-name))
        (setq function-body (buffer-substring (point)
                                              (progn (forward-sexp) (point)))))
      (save-excursion
        (insert function-body (format "\n\n(advice-add '%s :around 'my-%s)\n" function-name function-name)))
      (save-excursion
        (forward-char 1)
        (forward-sexp 1)
        (skip-syntax-forward " ")
        (insert "my-")
        (forward-sexp 1)
        (skip-syntax-forward " ")
        (forward-char 1)
        (insert "_ ")))))

;;; TODO: See key-bindings
;;; Smartparens
(use-package smartparens
  :if macos-p
  :config
  (progn
                                        ;(require 'smartparens-config)
                                        ;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
                                        ;(add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management

    (define-key sp-keymap (kbd "C-c s r n") 'sp-narrow-to-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (define-key sp-keymap (kbd "C-c s t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "C-c s p") 'sp-prefix-pair-object)
    
    (define-key sp-keymap (kbd "C-c s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "C-c s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "C-c s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-c s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "C-c s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "C-c s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    
;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
    
;;; lisp modes
    (sp-with-modes sp--lisp-modes
		  (sp-local-pair "(" nil :bind "C-("))))

;;; Replace selection on insert
(use-package delsel
  :config
  (delete-selection-mode 1))

;;; When buffer is closed, saves the cursor location
(use-package saveplace
  :config
  (save-place-mode 1))

;;; Repeat mode
;; The command C-x z (repeat) provides another way to repeat an Emacs
;; command many times. This command repeats the previous Emacs
;; command, whatever that was. Repeating a command uses the same
;; arguments that were used before; it does not read new arguments
;; each time. To repeat the command more than once, type additional
;; z’s: each z repeats the command one more time. Repetition ends when
;; you type a character other than z or press a mouse button. For
;; example, suppose you type C-u 2 0 C-d to delete 20 characters. You
;; can repeat that command (including its argument) three additional
;; times, to delete a total of 80 characters, by typing C-x z z z. The
;; first C-x z repeats the command once, and each subsequent z repeats
;; it once again.
;; (use-package repeat
;;   :config
;;   (repeat-mode))

;;; Hydra keyboard shortcuts
;; TODO: Try this
(use-package hydra
  :commands defhydra
  :config
  (defhydra my-window-movement ()
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("b" consult-buffer "buffer")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
    ("q" nil))
  (defhydra my-shortcuts (:exit t)
    ("s" save-buffer "Save")
    ("+" text-scale-increase "Increase")
    ("-" text-scale-decrease "Decrease")
    (">" new-shell "New shell"))
  (defvar hydra-stack nil)

  (defun my-hydra-push (expr)
    (push `(lambda () ,expr) hydra-stack))

  (defun my-hydra-pop ()
    (interactive)
    (let ((x (pop hydra-stack)))
      (when x (funcall x))))

  (defun my-hydra-go-and-push (expr)
    (push hydra-curr-body-fn hydra-stack)
    (prin1 hydra-stack)
    (funcall expr))

  (defun my-hydra-format-head (h)
    (let ((key-binding (elt h 0))
          (hint (elt h 2))
          (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
      (if cmd
          (format "%s (%s) - %s" hint key-binding cmd)
        (format "%s (%s)" hint key-binding))))

  (defun my-hydra-heads-to-candidates (base)
    (mapcar (lambda (h)
              (cons (my-hydra-format-head h) (hydra--head-name h base)))
            (symbol-value (intern (concat (symbol-name base) "/heads")))))

  (defun my-hydra-execute-extended (&optional _ hydra-base)
    (interactive (list current-prefix-arg nil))
    (hydra-keyboard-quit)
    (let* ((candidates (my-hydra-heads-to-candidates
                        (or hydra-base
                            (intern
                             (replace-regexp-in-string "/body$" ""
                                                       (symbol-name hydra-curr-body-fn))))))
           (command-name (completing-read "Cmd: " candidates))
           (bind (assoc-default command-name candidates 'string=)))
      (cond
       ((null bind) nil)
       ((hydra--callablep bind) (call-interactively bind)))))
  (define-key hydra-base-map (kbd "<tab>") #'my-hydra-execute-extended)
  :bind
  (("M-W" . #'my-window-movement/body)
   ("C-M-;" . #'my-shortcuts/body)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (setq savehist-file "~/.config/emacs/savehist")
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
	      '(kill-ring
          search-ring
          regexp-search-ring))
  :init
  (savehist-mode 1))

(use-package which-key
  :diminish
  :after marginalia
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package vertico
  :init
  (vertico-mode))

;;; A few more useful vertico-related configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; `orderless' completion style.
(use-package orderless
  :config
  ;; TODO: Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; TODO: Prescient
;; (use-package prescient :config (prescient-persist-mode +1))
;; (use-package 'corfu-prescient)
;; (use-package 'vertico-prescient)

(use-package projectile)

(use-package consult
  :bind (("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s i" . consult-info)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ("M-g l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("C-x c o" . consult-multi-occur)
         ("C-x c SPC" . consult-mark)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :config
  (defun consult-isearch ()
    "Call `consult-line` with the search string from the last `isearch`."
    (interactive)
    (consult-line isearch-string))
      
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :custom
  consult-preview-key '(:debounce 0.2 any)
  consult-narrow-key "<"
  consult-project-root-function #'projectile-project-root)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; TODO:
;; (use-package marginalia
;;   :init
;;   (marginalia-mode)
;;   :bind (:map minibuffer-local-completion-map
;;               ("M-m" . marginalia-cycle))
;;   :config
;;   (add-to-list 'marginalia-prompt-categories '("sketch" . sketch))
;;   (add-to-list 'marginalia-censor-variables "-api-key")
;;   (cl-pushnew #'marginalia-annotate-symbol-with-alias
;;         (alist-get 'command marginalia-annotator-registry))
;;   (cl-pushnew #'marginalia-annotate-symbol-with-alias
;;         (alist-get 'function marginalia-annotator-registry))
;;   (cl-pushnew #'marginalia-annotate-symbol-with-alias
;;         (alist-get 'symbol marginalia-annotator-registry)))

;; (defun marginalia-annotate-alias (cand)
;;   "Annotate CAND with the function it aliases."
;;   (when-let ((sym (intern-soft cand))
;;              (alias (car (last (function-alias-p sym))))
;;              (name (and (symbolp alias) (symbol-name alias))))
;;     (format " (%s)" name)))

;; (defun marginalia-annotate-symbol-with-alias (cand)
;;   "Annotate symbol CAND with its documentation string.
;;     Similar to `marginalia-annotate-symbol'."
;;   (when-let (sym (intern-soft cand))
;;     (concat
;;      (marginalia-annotate-binding cand)
;;      (marginalia--fields
;;       ((marginalia-annotate-alias cand) :face 'marginalia-function)
;;       ((marginalia--symbol-class sym) :face 'marginalia-type)
;;       ((cond
;;         ((fboundp sym) (marginalia--function-doc sym))
;;         ((facep sym) (documentation-property sym 'face-documentation))
;;         (t (documentation-property sym 'variable-documentation)))
;;        :truncate 1.0 :face 'marginalia-documentation)))))

;;; Embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; TODO: See what these do:
;; (defun embark-which-key-indicator ()
;;   "An embark indicator that displays keymaps using which-key.
;; The which-key help message will show the type and value of the
;; current target followed by an ellipsis if there are further
;; targets."
;;   (lambda (&optional keymap targets prefix)
;;     (if (null keymap)
;;         (which-key--hide-popup-ignore-command)
;;       (which-key--show-keymap
;;        (if (eq (plist-get (car targets) :type) 'embark-become)
;;            "Become"
;;          (format "Act on %s '%s'%s"
;;                  (plist-get (car targets) :type)
;;                  (embark--truncate-target (plist-get (car targets) :target))
;;                  (if (cdr targets) "…" "")))
;;        (if prefix
;;            (pcase (lookup-key keymap prefix 'accept-default)
;;              ((and (pred keymapp) km) km)
;;              (_ (key-binding prefix 'accept-default)))
;;          keymap)
;;        nil nil t (lambda (binding)
;;                    (not (string-suffix-p "-argument" (cdr binding))))))))

;; (setq embark-indicators
;;   '(embark-which-key-indicator
;;     embark-highlight-indicator
;;     embark-isearch-highlight-indicator))

;; (defun embark-hide-which-key-indicator (fn &rest args)
;;   "Hide the which-key indicator immediately when using the completing-read prompter."
;;   (which-key--hide-popup-ignore-command)
;;   (let ((embark-indicators
;;          (remq #'embark-which-key-indicator embark-indicators)))
;;       (apply fn args)))

;; (advice-add #'embark-completing-read-prompter
;;             :around #'embark-hide-which-key-indicator)

;;; Modus-themes
(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tinted)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package modus-themes
  :preface
;;; Highlight the active modeline using colours from modus-themes
  (defun my-update-active-mode-line-colors ()
    (set-face-attribute
     'mode-line nil
     :foreground (modus-themes-get-color-value 'fg-mode-line-active)
     :background (modus-themes-get-color-value 'bg-blue-subtle)
     :box '(:line-width
            1
            :color
            (modus-themes-get-color-value 'border-mode-line-active))))
  :hook
  (modus-themes-after-load-theme . my-update-active-mode-line-colors))

(use-package emacs
  :config
;;; fonts
  (set-face-attribute 'default nil
                      :font "JetBrains Mono:pixelsize=14:weight=semibold:slant=normal:width=normal:spacing=100:scalable=true"))

;;; Session
(use-package session
  :preface
  ;;; Desktop save
  (defun my-save-shell-buffer (desktop-dirname)
    ;; we only need to save the current working directory
    default-directory)
  :custom
  (session-save-file (expand-file-name ".session" user-emacs-directory))
  (session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  :config
  (setq session-save-file-coding-system 'utf-8)
  :hook
  ((after-init . session-initialize)
   (;; save all shell-mode buffers
    (shell-mode
      . (lambda ()
          (setq-local desktop-save-buffer #'my-save-shell-buffer))))))

;; (use-package desktop
;;   :config
;;   (setq desktop-path (list (expand-file-name ".cache" user-emacs-directory)))
;;   (desktop-save-mode)
;;   (desktop-read))

(use-package emacs
  :config
  ;; Misc settings
  (setq-default
   blink-cursor-interval 0.4
   cursor-type '(bar . 3)
   bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
   buffers-menu-max-size 30
   case-fold-search t
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain
   auto-save-default nil
   make-backup-files nil
   mouse-yank-at-point t
   save-interprogram-paste-before-kill t
   scroll-preserve-screen-position 'always
   ;; Scroll step
   scroll-step 1
   set-mark-command-repeat-pop t
   tooltip-delay 1.5
   truncate-lines nil
   truncate-partial-width-windows nil
   ;; Use spaces instead of tabs for indentation
   indent-tabs-mode nil
   tab-width 2
   sentence-end-double-space nil))

;; (use-package emacs
;;   :bind
;;   (:map prog-mode-map
;;         ("RET" . #'default-indent-new-line)
;;         ("<return>" . #'default-indent-new-line)
;;         ("S-<return>" . #'newline)))

(use-package emacs
  :preface
  ;;; Kill back to indentation
  (defun my-kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the line."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))
  :bind
  (("C-M-<backspace>" . #'sanityinc/kill-back-to-indentation)))

;;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :if macos-p
  :hook (after-init . mode-line-bell-mode))

(use-package beacon
  :custom
  (beacon-lighter "")
  (beacon-color "DarkGoldenrod2")
  (beacon-size 10)
  (beacon-blink-when-window-scrolls nil)
  :hook (after-init . beacon-mode))

(use-package display-line-numbers
  :hook ((text-mode prog-mode) . display-line-numbers-mode))

;;; Iedit mode
(use-package iedit
  :bind ("C-c e" . iedit-mode)
  :diminish)

;;; Cut/copy the current line if no region is active
;;When no region is active, if you type C-w then the current line will
;;be copied. When yanked using C-y, it will be yanked before the
;;current line. If a region is active
;; (use-package whole-line-or-region
;;   :diminish (whole-line-or-region-local-mode whole-line-or-region-global-mode)
;;   :hook (after-init . whole-line-or-region-global-mode))

(use-package emacs
  :preface
  ;; Escape cancels all
  (defun may/keyboard-escape-quit ()
    "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers)."
    (interactive)
    (cond ((eq last-command 'mode-exited) nil)
	        ((region-active-p)
	         (deactivate-mark))
	        ((> (minibuffer-depth) 0)
	         (abort-recursive-edit))
	        (current-prefix-arg
	         nil)
	        ((> (recursion-depth) 0)
	         (exit-recursive-edit))
	        (buffer-quit-function
	         (funcall buffer-quit-function))
	        ((string-match "^ \\*" (buffer-name (current-buffer)))
	         (bury-buffer))))
  :bind
  (("<escape>" . #'may/keyboard-escape-quit)
   ("C-g" . #'may/keyboard-escape-quit)))

;;; Paren matching
;; Show Off-screen matches
(use-package paren
  :preface
  (defun display-line-overlay+ (pos str &optional face)
    "Display line at POS as STR with FACE.

  FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char (window-start))
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'window (get-buffer-window))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit default :inherit show-paren-match)))
      ol))
  :custom
  (show-paren-delay 0.3)
  :config
  (show-paren-mode 1)
  ;; we will call `blink-matching-open` ourselves...
  (remove-hook 'post-self-insert-hook
               #'blink-paren-post-self-insert-function)

  ;; this still needs to be set for `blink-matching-open` to work
  (setq blink-matching-paren t)
  (let ((ov nil))                       ; keep track of the overlay
    (advice-add
     #'show-paren-function
     :after
     (defun show-paren--off-screen+ (&rest _args)
       "Display matching line for off-screen paren."
       (when (overlayp ov)
         (delete-overlay ov))
       ;; check if it's appropriate to show match info,
       ;; see `blink-paren-post-self-insert-function'
       (when (and (overlay-buffer show-paren--overlay)
                  (not (or cursor-in-echo-area
                           executing-kbd-macro
                           noninteractive
                           (minibufferp)
                           this-command))
                  (and (not (bobp))
                       (memq (char-syntax (char-before)) '(?\) ?\$)))
                  (= 1 (logand 1 (- (point)
                                    (save-excursion
                                      (forward-char -1)
                                      (skip-syntax-backward "/\\")
                                      (point))))))
         ;; rebind `minibuffer-message' called by
         ;; `blink-matching-open' to handle the overlay display
         (cl-letf (((symbol-function #'minibuffer-message)
                    (lambda (msg &rest args)
                      (let ((msg (apply #'format-message msg args)))
                        (setq ov (display-line-overlay+
                                  (window-start) msg))))))
           (blink-matching-open)))))))

(use-package emacs
  :config
  (defun may/disable-features-during-macro-call (orig &rest args)
    "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
    (let (post-command-hook
          font-lock-mode
          (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
      (apply orig args)))

  (advice-add 'kmacro-call-macro :around 'may/disable-features-during-macro-call))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    ;; (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-auto-save-history nil)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/backups/undo-tree")))))

;;; Winner mode - undo and redo window configuration
(use-package winner
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode)
  )

(use-package emacs
  :preface
  (defun may/split-window-func-with-other-buffer (split-function)
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall split-function)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window)))))
  (fset 'my-split-function-vertically
        (may/split-window-func-with-other-buffer 'split-window-vertically))
  (fset 'my-split-function-horizontally
        (may/split-window-func-with-other-buffer 'split-window-horizontally))
  :bind
  (("C-x 2" . #'my-split-function-vertically)
   ("C-x 3" . #'my-split-function-horizontally)))

(use-package emacs
  :config
  ;; Emacs' default buffer placement algorithm is pretty disruptive if
  ;; you like setting up window layouts a certain way in your
  ;; workflow. The display-buffer-alist video controls this behavior and
  ;; you can customize it to prevent Emacs from popping up new windows
  ;; when you run commands.
  (setq display-buffer-base-action
        '(display-buffer-reuse-mode-window
          display-buffer-reuse-window
          display-buffer-same-window))

  ;; If a popup does happen, don't resize windows to be equal-sized
  (setq even-window-sizes nil))

;; (use-package winum
;;   :bind (:map winum-keymap
;; 	      ("C-`" . 'winum-select-window-by-number)
;; 	      ("C-²" . 'winum-select-window-by-number)
;; 	      ("M-0" . 'winum-select-window-0-or-10)
;; 	      ("M-1" . 'winum-select-window-1)
;; 	      ("M-2" . 'winum-select-window-2)
;; 	      ("M-3" . 'winum-select-window-3)
;; 	      ("M-4" . 'winum-select-window-4)
;; 	      ("M-5" . 'winum-select-window-5)
;; 	      ("M-6" . 'winum-select-window-6)
;; 	      ("M-7" . 'winum-select-window-7)
;; 	      ("M-8" . 'winum-select-window-8)
;; 	      ("M-9" . 'winum-select-window-9))
;;   :config
;;   (setq winum-auto-setup-mode-line nil)
;;   (winum-mode))

;; TODO: See if required
;; (defun zap-to-isearch (rbeg rend)
;;   "Kill the region between the mark and the closest portion of
;;       the isearch match string. The behaviour is meant to be analogous
;;       to zap-to-char; let's call it zap-to-isearch. The deleted region
;;       does not include the isearch word. This is meant to be bound only
;;       in isearch mode.  The point of this function is that oftentimes
;;       you want to delete some portion of text, one end of which happens
;;       to be an active isearch word. The observation to make is that if
;;       you use isearch a lot to move the cursor around (as you should,
;;       it is much more efficient than using the arrows), it happens a
;;       lot that you could just delete the active region between the mark
;;       and the point, not include the isearch word."
;;   (interactive "r")
;;   (when (not mark-active)
;;     (error "Mark is not active"))
;;   (let* ((isearch-bounds (list isearch-other-end (point)))
;;          (ismin (apply 'min isearch-bounds))
;;          (ismax (apply 'max isearch-bounds))
;;          )
;;     (if (< (mark) ismin)
;;         (kill-region (mark) ismin)
;;       (if (> (mark) ismax)
;;           (kill-region ismax (mark))
;;         (error "Internal error in isearch kill function.")))
;;     (isearch-exit)
;;     ))

;; (define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

;; TODO: see if required
(use-package transient
  :config
  (transient-define-prefix cc/isearch-menu ()
    "isearch Menu"
    [["Edit Search String"
      ("e"
       "Edit the search string (recursive)"
       isearch-edit-string
       :transient nil)
      ("w"
       "Pull next word or character word from buffer"
       isearch-yank-word-or-char
       :transient nil)
      ("s"
       "Pull next symbol or character from buffer"
       isearch-yank-symbol-or-char
       :transient nil)
      ("l"
       "Pull rest of line from buffer"
       isearch-yank-line
       :transient nil)
      ("y"
       "Pull string from kill ring"
       isearch-yank-kill
       :transient nil)
      ("t"
       "Pull thing from buffer"
       isearch-forward-thing-at-point
       :transient nil)]

     ["Replace"
      ("q"
       "Start ‘query-replace’"
       isearch-query-replace
       :if-nil buffer-read-only
       :transient nil)
      ("x"
       "Start ‘query-replace-regexp’"
       isearch-query-replace-regexp
       :if-nil buffer-read-only
       :transient nil)]]

    [["Toggle"
      ("X"
       "Regexp searching"
       isearch-toggle-regexp
       :transient nil)
      ("S"
       "Symbol searching"
       isearch-toggle-symbol
       :transient nil)
      ("W"
       "Word searching"
       isearch-toggle-word
       :transient nil)
      ("F"
       "Case fold"
       isearch-toggle-case-fold
       :transient nil)
      ("L"
       "Lax whitespace"
       isearch-toggle-lax-whitespace
       :transient nil)]

     ["Misc"
      ("o"
       "occur"
       isearch-occur
       :transient nil)
      ("h"
       "highlight"
       isearch-highlight-regexp
       :transient nil)
      ("H"
       "highlight lines"
       isearch-highlight-lines-matching-regexp
       :transient nil)]])
  :bind
  (("M-S" . #'cc/isearch-menu)))

(use-package replace
  :straight nil
  :after occur
  :bind
  (:map occur-mode-map
        ("C-x C-q" . #'occur-edit-mode)))

(use-package ediff
  :straight nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    "Store `current-window-configuration' in variable `my-ediff-last-windows'."
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    "Restore window configuration to stored value in `my-ediff-last-windows'."
    (set-window-configuration my-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig))

(use-package emacs
  :config
  (setq set-mark-command-repeat-pop t)
  :bind
  (("C-x p" . #'pop-to-mark-command)))

;;; Ace-window
;; package for selecting a window to switch to
(use-package ace-window
  :defer t)

;;; Window management
(use-package emacs
  :bind
  (("M-o" . #'ace-window))
  :config
  (defvar-keymap may/windmove-keys
    :repeat t
    "h" #'windmove-left
    "j" #'windmove-down
    "k" #'windmove-up
    "l" #'windmove-right
    "H" #'windmove-swap-states-left
    "J" #'windmove-swap-states-down
    "K" #'windmove-swap-states-up
    "L" #'windmove-swap-states-right)
  (keymap-global-set "C-c w" may/windmove-keys))

;; (use-package smartscan
;;   :config (global-smartscan-mode t))

;;; Dired
;; TODO: checkout find-dired
(use-package dired
  :straight nil
  :config
  ;; (setq dired-listing-switches "-altr")
  ;; (setq dired-dwim-target 'dired-dwim-target-next)
  )
(use-package find-dired
  :after dired
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package emacs
  :preface
  (defun my-smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

      Move point to the first non-whitespace character on this line.
      If point is already there, move to the beginning of the line.
      Effectively toggle between the first non-whitespace character and
      the beginning of the line.

      If ARG is not nil or 1, move forward ARG - 1 lines first.  If
      point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))


    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))
  :bind
  (("C-a" . #'my-smarter-move-beginning-of-line)))

(use-package recentf
  :straight nil
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (recentf-mode))


;;; UTF-8
(use-package emacs
  :config
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))


;;;; TODO: See what this does:

;;; Expand
(use-package emacs
  :preface
  (defun sanityinc/dabbrev-friend-buffer (other-buffer)
    (< (buffer-size other-buffer) (* 1 1024 1024)))
  :config
  (setq save-abbrevs 'silently)
  (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)
  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-expand-all-abbrevs
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
  :bind
  (("M-/" . #'hippie-expand)))

;;; Yasnippets
;;TODO
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (push '(yasnippet backquote-change) warning-suppress-types)
  (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (setq yas-installed-snippets-dir (expand-file-name "elisp/yasnippet-snippets" user-emacs-directory))
  (setq yas-snippet-dirs `(,(expand-file-name "elisp/yasnippet-snippets" user-emacs-directory)))
  (setq yas-expand-only-for-last-commands nil)
  (bind-key "\t" 'hippie-expand yas-minor-mode-map)
  ;; (yas-global-mode)
  (yas-reload-all)
  )


;;; Don't show whitespace in diff, but show context

(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

;;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :diminish
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (set-face-foreground 'git-gutter-fr:modified "orange1"))


;;;;;; Setup EVIL

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (setq evil-emacs-state-cursor `(,(modus-themes-get-color-value 'bg-graph-blue-0) (bar . 3))
;;         evil-normal-state-cursor `(,(modus-themes-get-color-value 'fg-lavender) (bar . 3))
;;         evil-visual-state-cursor `(,(modus-themes-get-color-value 'bg-lavender) (bar . 3))
;;         evil-insert-state-cursor `(,(modus-themes-get-color-value 'bg-graph-green-1) (bar . 3))
;;         evil-cross-lines t
;;         evil-want-fine-undo t
;;         ;; cursor-related `evil-mode' settings
;;         evil-move-cursor-back nil
;;         evil-move-beyond-eol t
;;         evil-highlight-closing-paren-at-point-states nil)
;;   (evil-set-undo-system 'undo-tree)
;;   (dolist (mode '(acl2-doc-mode
;;                   eshell-mode
;;                   shell-mode
;;                   term-mode))
;;     (evil-set-initial-state mode 'emacs))
;;   (dolist (mode '(message-buffer-mode))
;;     (evil-set-initial-state mode 'normal))

;;   (advice-add 'evil-paste-after :override #'my/evil-paste-after)
;;   (advice-add 'evil-paste-before :override #'my/evil-paste-before)

;;   (setq evil-mode-line-format '(before . mode-line-front-space))

;;   (setq evil-normal-state-tag
;;         (propertize "   N   " 'face `(:background ,(modus-themes-get-color-value 'bg-blue-subtle)
;;                                     :foreground ,(modus-themes-get-color-value 'fg-mode-line-active)))
;;         evil-emacs-state-tag
;;         (propertize "---E---"  'face `(:background ,(modus-themes-get-color-value 'bg-red-intense)
;;                                     :foreground ,(modus-themes-get-color-value 'fg-mode-line-active)))
;;         evil-insert-state-tag
;;         (propertize "***I***" 'face `(:background ,(modus-themes-get-color-value 'bg-graph-green-1)
;;                                     :foreground ,(modus-themes-get-color-value 'fg-mode-line-active)))
;;         evil-motion-state-tag
;;         (propertize "   M   " 'face `(:background ,(modus-themes-get-color-value 'bg-lavender)
;;                                     :foreground ,(modus-themes-get-color-value 'fg-mode-line-active)))
;;         evil-visual-state-tag
;;         (propertize "   V   " 'face `(:background ,(modus-themes-get-color-value 'bg-lavender)
;;                                     :foreground ,(modus-themes-get-color-value 'fg-mode-line-active)))
;;         evil-operator-state-tag
;;         (propertize "   O   " 'face `(:background ,(modus-themes-get-color-value 'bg-lavender)
;;                                     :foreground ,(modus-themes-get-color-value 'fg-mode-line-active))))


;;   (defun evil-mouse-start-end (start end mode)
;;     "Return a list of region bounds based on START and END according to MODE.
;; If MODE is not 1 then set point to (min START END), mark to (max
;; START END).  If MODE is 1 then set point to start of word at (min
;; START END), mark to end of word at (max START END)."
;;     (evil-sort start end)
;;     (setq mode (mod mode 4))
;;     (if (/= mode 1) (list start end)
;;       (list
;;        (save-excursion
;;          (goto-char (min (point-max) (1+ start)))
;;          (if (zerop (forward-thing evil-mouse-word -1))
;;              (let ((bpnt (point)))
;;                (forward-thing evil-mouse-word +1)
;;                (if (> (point) start) bpnt (point)))
;;            (point-min)))
;;        (save-excursion
;;          (goto-char end)
;;          (if (zerop (forward-thing evil-mouse-word +1))
;;              (let ((epnt (point)))
;;                (forward-thing evil-mouse-word -1)
;;                (if (<= (point) end) epnt (point)))
;;            (point-max))))))
  
;;   ;; make `evil-jump-item' move point just after matching delimeter if it jumps forward
;;   (evil-define-motion evil-jump-item-before (count)
;;     "Find the next item in this line immediately before
;; or somewhere after the cursor and jump to the corresponding one."
;;     :jump t
;;     :type inclusive
;;     (let ((pos (point)))
;;       (unless (or (bolp) (bobp)) (backward-char))
;;       (condition-case nil
;;           (evil-jump-item count)
;;         ('user-error (goto-char pos)))
;;       (unless (< (point) pos)
;;         (goto-char pos)
;;         (evil-jump-item count)
;;         (when (> (point) pos) (forward-char)))))
  
;;   (defun evil-yank-line-handler (text)
;;     "Insert the current text linewise."
;;     (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
;;           (opoint (point)))
;;       (evil-remove-yank-excluded-properties text)
;;       (cond
;;        ((eq this-command 'evil-paste-before)
;;         (let ((col (current-column)))
;;           (evil-move-beginning-of-line)
;;           (let ((beg (point)))
;;             (insert text)
;;             (setq evil-last-paste
;;                   (list 'evil-paste-before evil-paste-count opoint beg (point)))
;;             (evil-set-marker ?\[ beg)
;;             (evil-set-marker ?\] (1- (point)))
;;             (move-to-column col t))))
;;        ((eq this-command 'evil-paste-after)
;;         (evil-move-end-of-line)
;;         (let ((beg (point)))
;;           (insert "\n")
;;           (insert text)
;;           (delete-char -1)              ; delete the last newline
;;           (setq evil-last-paste
;;                 (list 'evil-paste-after evil-paste-count opoint beg (point)))
;;           (evil-set-marker ?\[ (1+ beg))
;;           (evil-set-marker ?\] (point))
;;           (unless evil--cursor-after
;;             (goto-char (1+ beg))))
;;         (back-to-indentation))
;;        (t (insert text)))))

;;   (evil-define-command my/evil-paste-before
;;     (count &optional register yank-handler)
;;     "Paste the latest yanked text behind point.
;; The return value is the yanked text."
;;     :suppress-operator t
;;     (interactive "*P<x>")
;;     (setq count (prefix-numeric-value count))
;;     (if (evil-visual-state-p)
;;         (evil-visual-paste count register)
;;       (evil-with-undo
;;         (let* ((text (copy-sequence
;;                       (if register
;;                           (evil-get-register register)
;;                         (current-kill 0))))
;;                (yank-handler (or yank-handler
;;                                  (when (stringp text)
;;                                    (car-safe (get-text-property
;;                                               0 'yank-handler text)))))
;;                (opoint (point)))
;;           (when text
;;             (if (functionp yank-handler)
;;                 (let* ((evil-paste-count count)
;;                        ;; for non-interactive use
;;                        (this-command #'evil-paste-before))
;;                   (insert-for-yank text))
;;               ;; no yank-handler, default
;;               (when (vectorp text)
;;                 (setq text (evil-vector-to-string text)))
;;               (set-text-properties 0 (length text) nil text)
;;               ;; (unless (eolp) (forward-char))
;;               (push-mark (point) t)
;;               ;; TODO: Perhaps it is better to collect a list of all
;;               ;; (point . mark) pairs to undo the yanking for COUNT > 1.
;;               ;; The reason is that this yanking could very well use
;;               ;; `yank-handler'.
;;               (let ((beg (point)))
;;                 (dotimes (_ (or count 1))
;;                   (insert-for-yank text))
;;                 (setq evil-last-paste
;;                       (list #'evil-paste-after
;;                             count
;;                             opoint
;;                             beg         ; beg
;;                             (point)))   ; end
;;                 (evil-set-marker ?\[ beg)
;;                 (evil-set-marker ?\] (1- (point)))
;;                 (when (evil-normal-state-p)
;;                   (evil-move-cursor-back)))))
;;           (when evil--cursor-after
;;             (if (eq 'evil-yank-line-handler yank-handler)
;;                 (ignore-errors (evil-next-line-first-non-blank 1))
;;               (evil-forward-char 1 nil t))
;;             (setq evil--cursor-after nil))
;;           (when register
;;             (setq evil-last-paste nil))
;;           (and (> (length text) 0) text)))))
  
;;   (evil-define-command my/evil-paste-before
;;     (count &optional register yank-handler)
;;     "Paste the latest yanked text behind point.
;; The return value is the yanked text."
;;     :suppress-operator t
;;     (interactive "*P<x>")
;;     (setq count (prefix-numeric-value count))
;;     (if (evil-visual-state-p)
;;         (evil-visual-paste count register)
;;       (evil-with-undo
;;         (let* ((text (copy-sequence
;;                       (if register
;;                           (evil-get-register register)
;;                         (current-kill 0))))
;;                (yank-handler (or yank-handler
;;                                  (when (stringp text)
;;                                    (car-safe (get-text-property
;;                                               0 'yank-handler text)))))
;;                (opoint (point)))
;;           (when text
;;             (if (functionp yank-handler)
;;                 (let* ((evil-paste-count count)
;;                        ;; for non-interactive use
;;                        (this-command #'evil-paste-before))
;;                   (insert-for-yank text))
;;               ;; no yank-handler, default
;;               (when (vectorp text)
;;                 (setq text (evil-vector-to-string text)))
;;               (set-text-properties 0 (length text) nil text)
;;               ;; (unless (eolp) (forward-char))
;;               (push-mark (point) t)
;;               ;; TODO: Perhaps it is better to collect a list of all
;;               ;; (point . mark) pairs to undo the yanking for COUNT > 1.
;;               ;; The reason is that this yanking could very well use
;;               ;; `yank-handler'.
;;               (let ((beg (point)))
;;                 (dotimes (_ (or count 1))
;;                   (insert-for-yank text))
;;                 (setq evil-last-paste
;;                       (list #'evil-paste-after
;;                             count
;;                             opoint
;;                             beg         ; beg
;;                             (point)))   ; end
;;                 (evil-set-marker ?\[ beg)
;;                 (evil-set-marker ?\] (1- (point)))
;;                 (when (evil-normal-state-p)
;;                   (evil-move-cursor-back)))))
;;           (when evil--cursor-after
;;             (if (eq 'evil-yank-line-handler yank-handler)
;;                 (ignore-errors (evil-next-line-first-non-blank 1))
;;               (evil-forward-char 1 nil t))
;;             (setq evil--cursor-after nil))
;;           (when register
;;             (setq evil-last-paste nil))
;;           (and (> (length text) 0) text)))))

;;   (evil-define-command my/evil-paste-after
;;     (count &optional register yank-handler)
;;     "Paste the latest yanked text before the cursor position.
;; The return value is the yanked text."
;;     :suppress-operator t
;;     (interactive "*P<x>")
;;     (setq count (prefix-numeric-value count))
;;     (if (evil-visual-state-p)
;;         ;; This is the only difference with evil-paste-after in visual-state
;;         (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
;;           (evil-visual-paste count register))
;;       (evil-with-undo
;;         (let* ((text (copy-sequence
;;                       (if register
;;                           (evil-get-register register)
;;                         (current-kill 0))))
;;                (yank-handler (or yank-handler
;;                                  (when (stringp text)
;;                                    (car-safe (get-text-property
;;                                               0 'yank-handler text)))))
;;                (opoint (point)))
;;           (when evil-paste-clear-minibuffer-first
;;             (delete-minibuffer-contents)
;;             (setq evil-paste-clear-minibuffer-first nil))
;;           (when text
;;             (if (functionp yank-handler)
;;                 (let ((evil-paste-count count)
;;                       ;; for non-interactive use
;;                       (this-command #'evil-paste-after))
;;                   (push-mark opoint t)
;;                   (insert-for-yank text))
;;               ;; no yank-handler, default
;;               (when (vectorp text)
;;                 (setq text (evil-vector-to-string text)))
;;               (set-text-properties 0 (length text) nil text)
;;               (push-mark opoint t)
;;               (dotimes (_ (or count 1))
;;                 (insert-for-yank text))
;;               (setq evil-last-paste
;;                     (list #'evil-paste-before
;;                           count
;;                           opoint
;;                           opoint        ; beg
;;                           (point)))     ; end
;;               (evil-set-marker ?\[ opoint)
;;               (evil-set-marker ?\] (1- (point)))
;;               (when (and evil-move-cursor-back
;;                          (> (length text) 0))
;;                 (backward-char))))
;;           (when evil--cursor-after
;;             (if (eq 'evil-yank-line-handler yank-handler)
;;                 (ignore-errors (evil-next-line-first-non-blank))
;;               (evil-forward-char 1 nil t))
;;             (setq evil--cursor-after nil))
;;           ;; no paste-pop after pasting from a register
;;           (when register
;;             (setq evil-last-paste nil))
;;           (goto-char opoint)
;;           (and (> (length text) 0) text)))))

;;   (evil-define-motion evil-find-char-after (count char)
;;     "Move point immediately after the next COUNT'th occurrence of CHAR.
;; Movement is restricted to the current line unless `evil-cross-lines' is non-nil."
;;     :type inclusive
;;     (interactive "<c><C>")
;;     (unless count (setq count 1))
;;     (if (< count 0)
;;         (evil-find-char-backward (- count) char)
;;       (when (= (char-after) char)
;;         (forward-char)
;;         (cl-decf count))
;;       (evil-find-char count char)
;;       (forward-char))
;;     (setq evil-last-find (list #'evil-find-char-after char (> count 0))))

;;   (defun evil-forward-after-end (thing &optional count)
;;     "Move forward to end of THING.
;; The motion is repeated COUNT times."
;;     (setq count (or count 1))
;;     (cond
;;      ((> count 0)
;;       (forward-thing thing count))
;;      (t
;;       (unless (bobp) (forward-char -1))
;;       (let ((bnd (bounds-of-thing-at-point thing))
;;             rest)
;;         (when bnd
;;           (cond
;;            ((< (point) (cdr bnd)) (goto-char (car bnd)))
;;            ((= (point) (cdr bnd)) (cl-incf count))))
;;         (condition-case nil
;;             (when (zerop (setq rest (forward-thing thing count)))
;;               (end-of-thing thing))
;;           (error))
;;         rest))))
   
;;   (defun evil-backward-after-end (thing &optional count)
;;     "Move backward to end of THING.
;; The motion is repeated COUNT times. This is the same as calling
;; `evil-forward-after-word-end' with -COUNT."
;;     (evil-forward-after-end thing (- (or count 1))))

;;   (evil-define-motion evil-forward-after-word-end (count &optional bigword)
;;     "Move the cursor to the end of the COUNT-th next word.
;; If BIGWORD is non-nil, move by WORDS."
;;     :type inclusive
;;     (let ((thing (if bigword 'evil-WORD 'evil-word))
;;           (count (or count 1)))
;;       (evil-signal-at-bob-or-eob count)
;;       (evil-forward-after-end thing count)))

;;   (evil-define-motion evil-forward-after-WORD-end (count)
;;     "Move the cursor to the end of the COUNT-th next WORD."
;;     :type inclusive
;;     (evil-forward-after-word-end count t))

;;   (evil-define-motion evil-backward-after-word-end (count &optional bigword)
;;     "Move the cursor to the end of the COUNT-th previous word.
;; If BIGWORD is non-nil, move by WORDS."
;;     :type inclusive
;;     (let ((thing (if bigword 'evil-WORD 'evil-word)))
;;       (evil-signal-at-bob-or-eob (- (or count 1)))
;;       (evil-backward-after-end thing count)))

;;   (evil-define-motion evil-backward-after-WORD-end (count)
;;     "Move the cursor to the end of the COUNT-th previous WORD."
;;     :type inclusive
;;     (evil-backward-after-word-end count t))


;;   ;; redefine `inclusive' motion type to not include character after point
;;   (evil-define-type inclusive
;;     "Return the positions unchanged, with some exceptions.
;; If the end position is at the beginning of a line, then:

;; * If the beginning position is at or before the first non-blank
;;   character on the line, return `line' (expanded)."
;;     :expand (lambda (beg end) (evil-range beg end))
;;     :contract (lambda (beg end) (evil-range beg end))
;;     :normalize (lambda (beg end)
;;                  (cond
;;                   ((progn
;;                      (goto-char end)
;;                      (and (/= beg end) (bolp)))
;;                    (setq end (max beg (1- end)))
;;                    (cond
;;                     ((progn
;;                        (goto-char beg)
;;                        (looking-back "^[ \f\t\v]*" (line-beginning-position)))
;;                      (evil-expand beg end 'line))
;;                     (t
;;                      (unless evil-cross-lines
;;                        (setq end (max beg (1- end))))
;;                      (evil-expand beg end 'inclusive))))
;;                   (t
;;                    (evil-range beg end))))
;;     :string (lambda (beg end)
;;               (let ((width (- end beg)))
;;                 (format "%s character%s" width
;;                         (if (= width 1) "" "s")))))

;;   (evil-define-operator evil-top-join (beg end)
;;     "Join the selected lines to top."
;;     :motion evil-line
;;     (let ((count (count-lines beg end))
;;           last-line-blank)
;;       (if (= count 1)
;;           (evil-previous-line)
;;         (setq count (1- count)))
;;       (dotimes (i count)
;;         (when (= (1+ i) count)       ; we're just before the last join
;;           (evil-move-beginning-of-line)
;;           (setq last-line-blank (looking-at "[ \t]*$")))
;;         (join-line 1))
;;       (and last-line-blank (indent-according-to-mode))))

;;   (evil-define-key '(normal insert) 'global (kbd "C-a") nil)
;;   (evil-define-key '(normal insert) 'global (kbd "C-e") nil)
;;   (evil-set-leader 'normal (kbd "<space>"))
;;   (evil-define-key 'normal 'global
;;     (kbd "<leader>fs") #'save-buffer
;;     (kbd "<leader>bb") #'consult-buffer)
;;   ;; Setup Evil, Emacs cursor model:
;;   ;; motion command rebindings
;;   (evil-define-key 'motion 'global
;;     "t"  #'evil-find-char
;;     "T"  #'evil-find-char-to-backward
;;     "f"  #'evil-find-char-after
;;     "F"  #'evil-find-char-backward
;;     "e"  #'evil-forward-after-word-end
;;     "E"  #'evil-forward-after-WORD-end
;;     "ge" #'evil-backward-after-word-end
;;     "gE" #'evil-backward-after-WORD-end
;;     "%"  #'evil-jump-item-before)
;;   (evil-define-key 'normal 'global
;;     "p" #'evil-paste-before
;;     "P" #'evil-paste-after
;;     (kbd "C-J") #'evil-top-join)
;;   (evil-define-key 'insert 'global
;;     (kbd "C-y") nil)
  
;;   (evil-mode 1))


;;;;;;; Programming languages
;;; Markdown
(use-package markdown-mode
  :if macos-p
  :mode ("\\.\\(njk\\|md\\)\\'" . markdown-mode))

;;; Org-mode
(use-package org
  :if macos-p
  :bind
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading))
  :custom
  (org-insert-heading-respect-content t)
  (org-export-with-sub-superscripts nil)
  (org-fold-catch-invisible-edits 'smart))

;;; LSP
(use-package lsp-mode
  :if macos-p
  :config
  (setq lsp-headerline-breadcrumb-enable t
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.5
        company-minimum-prefix-length 1
        create-lockfiles nil ;; lock files will kill `npm start'
        )
  :hook ((prog-mode-hook . lsp)
         (lsp-mode-hook . lsp-enable-which-key-integration)))
;; (use-package lsp-ui
;;   :if macos-p
;;   :commands lsp-ui-mode
;;   :after lsp-mode)
;; (use-package dap-mode
;;   :if macos-p
;;   :after lsp-mode)

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode))


;; OCaml configuration
;;  - better error and backtrace matching

(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
    2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'caml-mode-hook 'set-ocaml-error-regexp)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup)

(use-package tuareg
  :hook (tuareg-mode . lsp-deferred))

(use-package utop
  :config
  (setq utop-command "opam exec -- dune utop . -- -emacs"))

(use-package ocamlformat
  :bind ("<f6>" . ocamlformat))
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(use-package lsp-mode
  :after lsp
  :init
  ;; opam install ocaml-lsp-server
  (defcustom lsp-ocaml-lsp-server-command
    '("ocamllsp")
    "Command to start ocaml-language-server."
    :group 'lsp-ocaml
    :type '(choice
            (string :tag "Single string value")
            (repeat :tag "List of string values"
                    string)))
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (lambda () lsp-ocaml-lsp-server-command))
    :major-modes '(caml-mode tuareg-mode)
    :priority 0
    :server-id 'ocaml-lsp-server)))

;;; Isabelle setup
(use-package isar-mode
  :if macos-p
  :straight (:local-repo "~/Code/simp-isar-mode")
  :mode "\\.thy\\'"
  :config
  ;; (add-hook 'isar-mode-hook 'turn-on-highlight-indentation-mode)
  ;; (add-hook 'isar-mode-hook 'flycheck-mode)
  ;; (add-hook 'isar-mode-hook 'company-mode)
  ;; (add-hook 'isar-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'company-backends)
  ;;                  '((company-dabbrev-code company-yasnippet)))))
  (add-hook 'isar-mode-hook
            (lambda ()
              (set (make-local-variable 'indent-tabs-mode) nil)))
  (add-hook 'isar-mode-hook
            (lambda ()
              (yas-minor-mode)))
  )

(use-package isar-goal-mode
  :if macos-p
  :straight (:local-repo "~/Code/simp-isar-mode")
  :after isar-mode)

(use-package lsp-isar-parse-args
  :if macos-p
  :straight (:local-repo "~/Code/isabelle-emacs/src/Tools/emacs-lsp/lsp-isar/")
  :after isar-mode
  :custom
  (lsp-isar-parse-args-nollvm nil))

(use-package lsp-isar
  :if macos-p
  :after (isar-mode yasnippet)
  :straight (:local-repo "~/Code/isabelle-emacs/src/Tools/emacs-lsp/lsp-isar/")
  :commands lsp-isar-define-client-and-start
  :custom
  (lsp-isar-output-use-async t)
  (lsp-isar-output-time-before-printing-goal nil)
  (lsp-isar-experimental t)
  (lsp-isar-split-pattern 'lsp-isar-split-pattern-three-columns)
  (lsp-isar-decorations-delayed-printing t)
  :init
  (add-hook 'lsp-isar-init-hook 'lsp-isar-open-output-and-progress-right-spacemacs)
  (add-hook 'isar-mode-hook #'lsp-isar-define-client-and-start)

  (push (concat "~/Code/isabelle-emacs/src/Tools/emacs-lsp/yasnippet")
   yas-snippet-dirs)

  (setq lsp-isar-path-to-isabelle "~/Code/isabelle-emacs")
  )

(use-package session-async
  :if macos-p)

;;; YAML
(use-package yaml-mode
  :if macos-p
  :mode "\\.yml\\'")

;;; ACL2

(use-package init-acl2
  :if macos-p
  :straight nil
  :hook ((lisp-mode . acl2-lisp-mode))
  ;; :mode ("\\.lisp\\'" . lisp-mode)
  )

;;; HOL4
(use-package sml-mode
  :mode ("\\.sml\\'" . sml-mode)
  :config
  (load "~/Code/HOL/tools/hol-mode")
  (load "~/Code/HOL/tools/hol-unicode"))

;;; Verilog
;; (use-package verilog-ts-mode
;;   :mode "\\.s?vh?\\'")
(use-package verilog-ext
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET verilog-ext':
  ;; Comment out/remove the ones you do not need
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          ;; capf
          hierarchy
          ;; eglot
          lsp
          ;; lsp-bridge
          ;; lspce
          flycheck
          beautify
          navigation
          template
          formatter
          ;; compilation
          ;; imenu
          ;; which-func
          ;; hideshow
          ;; typedefs
          ;; time-stamp
          block-end-comments
          ports))
  :config
  (verilog-ext-mode-setup))


;; Set initial frame to full-screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
