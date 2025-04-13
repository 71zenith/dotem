;;; early-init.el -*- lexical-binding: t -*-

;;; Garbage Collection
(defvar file-name-handler-alist-original file-name-handler-alist)
(defvar vc-handled-backends-original vc-handled-backends)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      vc-handled-backends nil
      file-name-handler-alist nil)

;;; Performance
(setq site-run-file nil
      use-dialog-box nil
      use-file-dialog nil
      x-gtk-use-system-tooltips nil
      tooltip-delay 0.1
      read-process-output-max (* 8 1024 1024)
      inhibit-compacting-font-caches t
      package-enable-at-startup nil
      x-underline-at-descent-line t
      redisplay-skip-fontification-on-input t
      kill-ring-max 500
      ring-bell-function #'ignore
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      fast-but-imprecise-scrolling t
      bidi-inhibit-bpa t
      bidi-display-reordering nil
      bidi-paragraph-direction 'left-to-right
      debug-on-error t
      idle-update-delay 1.0
      load-prefer-newer t)


;;; Minimal Frame
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)

(tooltip-mode -1)


;;; Font Config
(let ((mono-font "Aporetic Serif Mono")
      (serif-font "Aporetic Serif"))
  (set-face-attribute 'default nil :family mono-font :height 155)
  (set-face-attribute 'fixed-pitch nil :family serif-font :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil :family serif-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family serif-font :height 1.0))


;; changes other symbols // needs to be evaluated
(set-fontset-font "fontset-default" 'japanese-jisx0208 "Natsume")


;;; Pesky Behaviour
(setq inhibit-startup-buffer-menu t
      inhibit-x-resources t
      initial-major-mode 'fundamental-mode
      inhibit-startup-screen t
      inhibit-default-init t
      initial-scratch-message nil)


;;; Native Comp
(when (featurep 'native-compile)
  (setq native-compile-prune-cache t
        native-comp-async-report-warnings-errors nil))


;;; UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;;; Startup Timer
(defun zen/display-startup-time ()
  (message "📑 loaded in %s with %d 🚮"
           (format "%.2f ⌛"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))


;;; Hooks
(add-hook 'emacs-startup-hook
          (lambda ()
            (zen/display-startup-time))
          (setq gc-cons-threshold (* 8 1024 1024)
                gc-cons-percentage 0.1
                vc-handled-backends vc-handled-backends-original
                file-name-handler-alist file-name-handler-alist-original))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(setq custom-file (locate-user-emacs-file "var/custom.el"))
(load custom-file :no-error-if-missing)

(provide 'early-init)
