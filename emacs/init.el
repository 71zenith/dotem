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

(use-package paredit :ensure t :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (clojure-mode . enable-paredit-mode)))

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

    "a" '(embark-act :which-key "act")
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

;; only for non-ts langs
(use-package highlight-numbers :ensure t :defer t
  :hook (prog-mode . (lambda ()
                       (unless (string-suffix-p "-ts-mode" (symbol-name major-mode))
                         (highlight-numbers-mode)))))

;; only for non-lispy/non-ts langs
(use-package highlight-operators :ensure t :defer t
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p
                                'emacs-lisp-mode 'clojure-mode 'odin-ts-mode)
                         (highlight-operators-mode 1)))))


(use-package popper :ensure t :defer t
  :hook (after-init . popper-mode)
  :custom
  (popper-reference-buffers '("\\*.*\\*")))

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

(use-package consult :ensure t :defer t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap isearch-forward] 'consult-line))

(use-package embark :ensure t :defer t
  :config
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))


(use-package helpful :ensure t :defer t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)))

;;; VC
(use-package magit :ensure t :defer t)
(use-package diff-hl :ensure t
  :defer t
  :hook (after-init . global-diff-hl-mode))

;;; Lang Support
(use-package nix-ts-mode :ensure t :defer t
  :mode "\\.nix\\'")

(use-package odin-ts-mode :ensure t :defer t
  :vc (:url "https://github.com/Sampie159/odin-ts-mode" :rev :newest :branch "main")
  :mode "\\.odin\\'")

(use-package treesit-auto :ensure t :defer t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install t
        treesit-language-source-alist '((odin . ("https://github.com/tree-sitter-grammars/tree-sitter-odin"))
                                        (nix . ("https://github.com/nix-community/tree-sitter-nix"))))
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package cider :ensure t :defer t
  :hook (clojure-mode . cider-mode))

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
  (eshell-prompt-function (lambda ()
                            (concat
                             (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
                             (propertize " λ " 'face 'eshell-prompt)))))

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
 truncate-lines t
 word-wrap t
 line-move-visual nil

 ;; Display settings
 display-time-default-load-average nil
 fringes-outside-margins nil
 fringe-indicator-alist nil
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 cursor-in-non-selected-windows t

 ;; File handling
 create-lockfiles nil
 delete-by-moving-to-trash t
 make-backup-files nil
 auto-save-default nil
 version-control nil
 vc-make-backup-files nil
 vc-follow-symlinks t
 find-file-visit-truename nil

 ;; Completion settings
 completion-ignore-case t

 ;; Scrolling behavior
 scroll-margin 3
 scroll-conservatively 100000
 scroll-preserve-screen-position t
 auto-window-vscroll nil)

;;; Personal Info
(setq user-full-name "Mori Zen"
      user-mail-address "71zenith@proton.me"
      default-input-method "japanese"
      display-time-format "%a %d %b %H:%M")

(use-package server
  :ensure nil :defer 2
  :config (unless (server-running-p) (server-start)))

(load-file (concat user-emacs-directory "themes/oxocarbon-theme.el"))
(enable-theme 'oxocarbon)
