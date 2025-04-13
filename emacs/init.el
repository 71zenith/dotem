;;; init.el --- Config -*- lexical-binding: t -*-

;;; Commentary:
;;; Emacs config by mori.zen in 2025

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq package-native-compile t
      package-install-upgrade-built-in t)

(use-package no-littering :ensure t :demand t)
(use-package minions :ensure t :demand t
  :custom
  (minions-prominent-modes '(flymake-mode))
  (minions-mode-line-face 'match)
  (minions-mode-line-lighter "⮧")
  :hook (after-init . minions-mode))


;;; Undo
(use-package undo-fu :ensure t :defer t)

(use-package undo-fu-session :ensure t :defer t :hook (after-init . global-undo-fu-session-mode))


;;; Evil
(use-package evil :ensure t :defer t
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-echo-state nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-fu)
  :hook (after-init . evil-mode))

(use-package evil-collection :ensure t :defer t
  :hook (after-init . evil-collection-init))

(use-package evil-commentary :ensure t :defer t
  :hook (after-init . evil-commentary-mode))

(use-package evil-snipe :ensure t :defer t
  :hook (after-init . evil-snipe-override-mode))

(use-package paredit :ensure t :defer t
  :hook ((emacs-lisp-mode clojure-mode) . enable-paredit-mode))

;;; General
(use-package general :ensure t :demand t
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

  (general-define-key
   :states '(normal visual emacs)
   "[d" 'diff-hl-previous-hunk
   "]d" 'diff-hl-next-hunk
   "[f" 'flymake-goto-prev-error
   "]f" 'flymake-goto-next-error
   "[w" 'evil-window-prev
   "]w" 'evil-window-next)

  (general-define-key
   :keymaps 'dired-mode-map
   "<tab>" 'dired-subtree-toggle
   "TAB" 'dired-subtree-toggle
   "<backtab>" 'dired-subtree-remove
   "S-TAB" 'dired-subtree-remove)

  (leader-keys
    "x" '(execute-extended-command :wk "M-x")
    ":" '(eval-expression :wk "M-:")
    "r" '(restart-emacs :wk "re:")
    "a" '(embark :wk "act")
    "e" 'eshell
    "p" '(popper-toggle :wk "pop")
    "q" '(popper-toggle-type :wk "pop!")
    "i" '((lambda () (interactive) (find-file user-init-file)) :wk "init")

    "b" '(:ignore t :wk "buf")
    "b d" 'kill-current-buffer
    "b r" 'resize-window
    "b b" 'consult-buffer
    "b c" 'delete-window
    "b v" 'evil-window-vsplit
    "b s" 'evil-window-split

    "g" '(:ignore t :wk "git")
    "g g "'magit-status
    "g r" 'diff-hl-revert-hunk
    "g s" 'diff-hl-show-hunk
    "g a" 'diff-hl-stage-some

    "h" '(:ignore t :wk "help")
    "h f" 'describe-face
    "h v" 'helpful-variable
    "h c" 'helpful-callable
    "h o" 'helpful-symbol
    "h p" 'helpful-at-point

    "f" '(:ignore t :wk "file")
    "f f" 'find-file
    "f d" 'dired
    "f r" 'consult-recent-file
    "f g" 'consult-find
    "f G" 'consult-ripgrep
    "f P" 'project-switch-project
    "f p" 'project-find-file

    "c" '(:ignore t :wk "code")
    "c i" 'consult-imenu
    "c l" 'consult-flymake
    "c f" 'format-all-region-or-buffer
    "c h" 'display-local-help
    "c j" 'cider-jack-in-clj
    "c d" 'cider-eval-dwim
    "c e" 'cider-eval-last-sexp
    "c c" 'cider-repl-clear-buffer))

(global-set-key (kbd "<escape>") 'keyboard-quit)


;;; Visual
(use-package which-key :ensure t :defer t
  :hook (after-init . which-key-mode))

(use-package anzu :ensure t :defer t
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
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

(use-package resize-window :ensure t :defer t
  :custom (resize-window-fine-argument 5))

(use-package popper :ensure t
  :hook (after-init . popper-mode)
  :custom (popper-reference-buffers '("\\*.*\\*")))


;;; Highlight
(use-package rainbow-delimiters :ensure t :defer t :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :ensure t :defer t
  :hook (emacs-lisp-mode . (lambda () (when (string-suffix-p "-theme.el" (buffer-file-name))
                                   (rainbow-mode)))))

(use-package highlight-numbers :ensure t :defer t
  :hook (prog-mode . (lambda ()
                       (unless (string-suffix-p "-ts-mode" (symbol-name major-mode))
                         (highlight-numbers-mode)))))

(use-package highlight-operators :ensure t :defer t
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p
                                'emacs-lisp-mode 'clojure-mode)
                         (highlight-operators-mode)))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\_<\\(nil\\|t\\)\\_>" . font-lock-constant-face)))

;;; Completion
(use-package cape :ensure t
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu :ensure t
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-popupinfo-mode))
  :custom
  (corfu-preview-current nil)
  (corfu-min-width 4)
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.5 . 0.25)))


;;; Minibuffer
(use-package vertico :ensure t
  :hook ((after-init . vertico-reverse-mode)
         (after-init . vertico-mode)))

(use-package marginalia :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless :ensure t
  :custom
  (completion-styles '(substring orderless partial-completion))
  (completion-ignore-case t))

(use-package consult :ensure t :defer t
  :bind ("C-;" . consult-history)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (global-set-key [remap isearch-forward] 'consult-line))

(use-package embark :ensure t
  :bind ("C-." . embark-act)
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

(use-package embark-consult :ensure t :defer t)

(use-package helpful :ensure t :defer t)

;;; Git
(use-package magit :ensure t :defer t
  :custom
  (magit-section-visibility-indicator '("⮧"))
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package ediff :ensure nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl :ensure t
  :defer t
  :hook (after-init . global-diff-hl-mode))


;;; Langs
(use-package nix-ts-mode :ensure t :defer t
  :mode "\\.nix\\'")

(use-package clojure-ts-mode :ensure t :defer t
  :mode "\\.clj\\'")

(use-package zig-ts-mode :ensure t :defer t
  :mode "\\.zig\\'")

(use-package odin-ts-mode :ensure t :defer t
  :vc (:url "https://github.com/Sampie159/odin-ts-mode" :rev :newest :branch "main")
  :mode "\\.odin\\'")

(use-package css-ts-mode :ensure nil :defer t
  :mode "\\.rasi\\'")

(use-package treesit-auto :ensure t :defer t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install t
        treesit-language-source-alist '((odin . ("https://github.com/tree-sitter-grammars/tree-sitter-odin"))
                                        (nix . ("https://github.com/nix-community/tree-sitter-nix"))))
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package format-all :ensure t :defer t)

(use-package cider :ensure t :defer t
  :custom
  (cider-use-fringe-indicators nil)
  (cider-repl-display-help-banner nil)
  :hook (clojure-mode . cider-mode))


;;; Eglot
(use-package eglot :ensure nil :defer t
  :hook (odin-ts-mode . eglot-ensure)
  :custom
  (eglot-report-progress nil)
  (eglot-events-buffer-config '(:size 0))
  (eglot-send-changes-idle-time 0.05)
  (eglot-sync-connect nil)
  (eglot-autoshutdown t))

(use-package eglot-booster :ensure t :defer t
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest :branch "main")
  :config (eglot-booster-mode))

(use-package flymake :ensure nil :defer t
  :hook (emacs-lisp-mode . flymake-mode)
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-indicator-type 'margins))

(use-package eldoc :ensure nil :defer t
  :custom (eldoc-idle-delay 0.25))


;;; Dired
(use-package dired :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-open :ensure t :defer t :after dired)

(use-package dired-subtree :ensure t :defer t :after dired
  :custom (dired-subtree-use-backgrounds nil))


;;; Terminal
(use-package eat :ensure t :defer t
  :hook (eshell-mode . eat-eshell-mode))

(use-package eshell :ensure nil :defer t
  :hook (eshell-mode . (lambda () (eshell/alias "c" "clear-scrollback")))
  :custom
  (eshell-banner-message "")
  (eshell-prompt-function (lambda () (propertize (concat (abbreviate-file-name (eshell/pwd)) " λ ") 'face 'eshell-prompt)))
  :config
  (use-package eshell-syntax-highlighting :ensure t :defer t
    :hook (eshell-mode . eshell-syntax-highlighting-mode)))


;;; Modes
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
                winner-mode))
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
 tab-always-indent 'complete
 enable-recursive-minibuffers t
 text-mode-ispell-word-completion nil

 ;; Editing behavior
 indent-tabs-mode nil
 tab-width 4
 truncate-lines t
 word-wrap t
 line-move-visual nil
 require-final-newline t

 ;; Display settings
 display-time-default-load-average nil
 fringes-outside-margins nil
 fringe-indicator-alist nil
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 cursor-in-non-selected-windows t
 mode-line-end-spaces " "
 mode-line-front-space " "

 ;; Inferior files
 create-lockfiles nil
 delete-by-moving-to-trash t
 make-backup-files nil
 auto-save-default nil
 version-control nil
 vc-follow-symlinks t
 find-file-visit-truename nil
 recentf-max-saved-items 80

 ;; Scrolling behavior
 scroll-margin 3
 scroll-conservatively 10000
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 mouse-wheel-progressive-speed nil

 ;; Personal Info
 user-full-name "Mori Zen"
 user-mail-address "71zenith@proton.me"
 default-input-method "japanese"
 display-time-format "%a %d %b %H:%M")

(use-package server
  :ensure nil :defer t
  :config (unless (server-running-p) (server-start)))

(load-file (concat user-emacs-directory "themes/oxocarbon-theme.el"))
(enable-theme 'oxocarbon)

(provide 'init)

;;; init.el ends here
