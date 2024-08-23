;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

;; PERF: Garbage collection is a big contributor to startup
;;   times. This fends it off, but will be reset later by
;;   `gcmh-mode'. Not resetting it later causes stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; ;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
;; ;;   Ensuring correctness is 'doom sync's job, not the interactive session's.
;; ;;   Still, stale byte-code will cause *heavy* losses in startup efficiency, but
;; ;;   performance is unimportant when Emacs is in an error state.
(setq load-prefer-newer noninteractive)



;; DeferGC
(setq gc-cons-threshold 100000000)
;; -DeferGC

;; UnsetPES
(setq package-enable-at-startup nil)
;; -UnsetPES

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; -DisableUnnecessaryInterface

(defvar my-native-comp-reserved-cpus 2
  "Number of CPUs to reserve and not use for `native-compile'.")

(defun my-calculate-native-comp-async-jobs ()
  "Set `native-comp-async-jobs-number' based on the available CPUs."
  ;; The `num-processors' function is only available in Emacs >= 28.1
  (max 1 (- (num-processors) my-native-comp-reserved-cpus)))

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-async-jobs-number (my-calculate-native-comp-async-jobs)
          native-comp-deferred-compilation t
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;;; early-init.el ends here
