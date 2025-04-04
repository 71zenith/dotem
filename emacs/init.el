;; init.el -*- lexical-binding: t -*-

;;; Package Initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(setq package-native-compile t
      package-install-upgrade-built-in t)

(use-package no-littering :ensure t :demand t)
(use-package minions :ensure t :demand t
  :custom (minions-mode-line-lighter "zZ")
  :hook (after-init . minions-mode))

;;; Undo System
(use-package undo-fu :ensure t :defer t)
(use-package undo-fu-session :ensure t :defer t :hook (after-init . global-undo-fu-session-mode))

;;; Evil Mode
(use-package evil :ensure t :defer 2
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-undo-system 'undo-fu)
  :hook (after-init . evil-mode))

(use-package evil-collection :ensure t :defer 2
  :hook ((after-init . evil-collection-init)))

(use-package evil-commentary :ensure t :defer 2
  :hook (after-init . evil-commentary-mode))

(use-package evil-snipe :ensure t :defer 2
  :hook (after-init . evil-snipe-override-mode))

;;; General.el
(use-package general :ensure t
  :demand t
  :config
  (general-evil-setup)
  (general-def
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    "C-0" (lambda () (interactive) (text-scale-set 0)))

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(execute-extended-command :which-key "M-x")
    ":" '(eval-expression :which-key "M-:")
    "r" '(restart-emacs :which-key "re:")
    "q" '(kill-emacs :which-key "exit")
    "p" '(popper-toggle :which-key "pop")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "init")

    "b" '(:ignore t :which-key "buffer")
    "b d" 'kill-current-buffer
    "b b" 'consult-buffer
    "b p" 'previous-buffer
    "b n" 'next-buffer

    "w" '(:ignore t :which-key "window")
    "w c" 'delete-window
    "w v" 'evil-window-vsplit
    "w s" 'evil-window-split
    "w n" 'evil-window-next
    "w p" 'evil-window-prev

    "h" '(:ignore t :which-key "help")
    "h f" 'helpful-callable
    "h k" 'helpful-key
    "h o" 'helpful-symbol

    "f" '(:ignore t :which-key "file")
    "f s" 'save-buffer
    "f f" 'find-file
    "f g" 'consult-find
    "f G" 'consult-ripgrep
    "f r" 'consult-recent-file
    "f p" 'project-find-file
    "f P" 'project-switch-project
    "f d" 'dired

    "e" 'eshell
    "g" '(magit-status :which-key "git")
    ))

(global-set-key (kbd "<escape>") 'keyboard-quit)

;;; Visual Elements
(use-package which-key :ensure t :defer t
  :hook (after-init . which-key-mode))

(use-package anzu :ensure t :defer t
  :hook (after-init . global-anzu-mode))

(use-package spacious-padding :ensure t :defer t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(
     :internal-border-width 8
     :mode-line-width 1
     :tab-width 2
     :fringe-width 6
     :right-divider-width 10)))

(use-package rainbow-delimiters :ensure t :defer t :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-numbers :ensure t :defer t :hook (prog-mode . highlight-numbers-mode))
(use-package highlight-operators :ensure t :defer t
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p
                                'lisp-mode 'emacs-lisp-mode 'common-lisp-mode 'clojure-mode)
                         (highlight-operators-mode 1)))))


(use-package popper :ensure t :defer t
  :hook (after-init . popper-mode)
  :custom (popper-reference-buffers '("\\*.*\\*")))

;;; Completion
(use-package cape :ensure t
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu :ensure t
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-history-mode)
         (after-init . corfu-popupinfo-mode))
  :custom
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 4)
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.5 . 0.25)))

;;; Minibuffer
(use-package vertico :ensure t :hook (after-init . vertico-mode))

(use-package marginalia :ensure t :hook (after-init . marginalia-mode))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless substring))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult :ensure t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap isearch-forward] 'consult-line))

(use-package helpful :ensure t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)))

;;; VC
(use-package magit :ensure t :defer t)
(use-package diff-hl :ensure t
  :defer t
  :hook (after-init . global-diff-hl-mode))

;;; Lang Support
(use-package nix-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t :custom (zig-format-on-save nil))
(use-package odin-mode :ensure t :defer t
  :vc (:url "https://github.com/mattt-b/odin-mode" :rev :newest :branch "main"))

(use-package cider :ensure t :defer t
  :hook (clojure-mode . cider-mode))

(use-package paredit :ensure t :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (clojure-mode . enable-paredit-mode)))

;;; Dired
(use-package dired :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-mouse-drag-files t))

(use-package dired-subtree :ensure t :defer t :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove))
  :custom (dired-subtree-use-backgrounds nil))

;;; Terminal
(use-package eat :ensure t :defer t
  :hook ((eshell-mode . eat-eshell-mode)))

(use-package eshell-syntax-highlighting :ensure t :defer t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell :ensure nil
  :hook (eshell-mode . (lambda () (eshell/alias "c" "clear-scrollback")))
  :custom
  (eshell-banner-message "")
  (eshell-prompt-function (lambda nil
                            (let ((dir-color (face-attribute 'font-lock-keyword-face :foreground))
                                  (prompt-color (face-attribute 'font-lock-builtin-face :foreground)))
                              (concat
                               (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,dir-color))
                               (propertize " λ" 'face `(:foreground ,prompt-color))
                               (propertize " "))))
                          ))

;;; Global Modes
(dolist (mode '(global-hl-line-mode
                global-auto-revert-mode
                global-so-long-mode
                global-prettify-symbols-mode
                electric-pair-mode
                recentf-mode
                size-indication-mode
                column-number-mode
                pixel-scroll-precision-mode
                savehist-mode
                save-place-mode
                delete-selection-mode))
  (funcall mode 1))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers 'relative)
            (when (> (buffer-size) 100000)
              (display-line-numbers-mode -1))))

;;; Defaults
(setq-default
 ;; Emacs behavior
 confirm-kill-emacs nil
 confirm-kill-processes nil
 use-short-answers t

 ;; Editing behavior
 indent-tabs-mode nil
 tab-width 4
 require-final-newline nil
 backward-delete-char-untabify-method 'hungry
 truncate-lines t
 word-wrap t
 line-move-visual nil

 ;; Display settings
 display-time-default-load-average nil
 fringes-outside-margins nil
 fringe-indicator-alist nil
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 line-spacing 0.08
 cursor-type 'bar
 cursor-in-non-selected-windows nil

 ;; File handling
 create-lockfiles nil
 delete-by-moving-to-trash t
 make-backup-files nil
 auto-save-default nil
 version-control nil
 vc-make-backup-files nil
 vc-follow-symlinks t
 find-file-visit-truename nil

 ;; Auto-revert settings
 auto-revert-verbose nil
 auto-revert-interval 1
 auto-save-no-message t
 global-auto-revert-non-file-buffers t

 ;; Completion settings
 completion-ignore-case t

 ;; Scrolling behavior
 scroll-margin 3
 scroll-conservatively 100000
 scroll-preserve-screen-position t
 scroll-step 5
 auto-window-vscroll nil

 ;; Miscellaneous
 ad-redefinition-action 'accept)

;;; Personal Info
(setq user-full-name "Mori Zen"
      user-mail-address "71zenith@proton.me"
      default-input-method "japanese"
      display-time-format "%a %d %b %H:%M"
      calendar-week-start-day 1)

(use-package server
  :ensure nil :defer t
  :config (unless (server-running-p) (server-start)))

(load-file (concat user-emacs-directory "themes/oxocarbon-theme.el"))
(enable-theme 'oxocarbon)
