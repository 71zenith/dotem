;;; oxocarbon-theme.el --- Theme

;; Copyright (C) 2025 , zen

;; Author: zen
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary: theme by shaunsingh
;;; oxocarbon theme created by zen in 2025

;;; Code:

(deftheme oxocarbon)
(let* ((class '((class color) (min-colors 89)))
       (base00 "#161616")
       (base01 "#262626")
       (base02 "#393939")
       (base03 "#525252")
       (base04 "#dde1e6")
       (base05 "#f2f4f8")
       (base06 "#ffffff")
       (base07 "#08bdba")
       (base08 "#3ddbd9")
       (base09 "#78a9ff")
       (base0A "#ee5396")
       (base0B "#33b1ff")
       (base0C "#ff7eb6")
       (base0D "#42be65")
       (base0E "#be95ff")
       (base0F "#82cfff")
       (fg1 base04)
       (fg2 base05)
       (fg3 base06)
       (bg1 base00)
       (bg2 base01)
       (bg3 base02)
       (bg4 base03)
       (builtin base09)
       (number  base0F)
       (keyword base09)
       (const   base07)
       (pre     base09)
       (comment base03)
       (func    base0C)
       (str     base0E)
       (type    base09)
       (var     base04)
       (selection base02)
       (warning   base0A)
       (warning2  base0D)
       (unspec   (when (>= emacs-major-version 29) 'unspecified)))
  (custom-theme-set-faces
   'oxocarbon
   ;;; emacs
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background ,bg2))))
   `(fringe ((,class (:background ,bg2 :foreground ,fg3))))
   `(cursor ((,class (:background ,fg3))))
   `(isearch ((,class (:weight bold :background ,warning :foreground ,fg2))))
   `(isearch-fail ((,class (:weight bold :foreground ,warning :background ,bg3))))
   `(minibuffer-prompt ((,class (:foreground ,keyword))))
   `(tooltip ((,class (:background ,bg1))))
   `(match ((,class (:foreground ,base0F))))
   `(italic ((,class (:italic t))))
   `(bold ((,class (:weight bold))))
   `(vertical-border ((,class (:background ,bg3))))
   `(link ((,class (:foreground ,const :underline t))))
   `(error ((,class (:foreground ,base0A))))
   `(success ((,class (:foreground ,base0D))))
   `(warning ((,class (:foreground ,base0C))))
   `(lazy-highlight ((,class (:foreground ,bg1 :background ,base08))))
   `(trailing-whitespace ((,class :foreground ,unspec :background ,warning)))


    ;;; font-lock-*
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-function-name-face ((,class (:foreground ,func :weight bold))))
   `(font-lock-function-call-face ((,class (:foreground ,func :weight bold))))
   `(font-lock-keyword-face ((,class :foreground ,keyword)))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-variable-call-face ((,class (:foreground ,var))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-operator-face ((,class (:foreground ,base0B))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(font-lock-escape-face ((,class (:foreground ,number))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-preprocessor-face ((,class :foreground ,pre)))
   `(font-lock-property-use-face ((,class :foreground ,warning)))
   `(font-lock-property-name-face ((,class :foreground ,warning)))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-string-face ((,class (:foreground ,str))))


   ;;; highlight-* (non ts-mode)
   `(highlight-numbers-number ((,class (:foreground ,number))))
   `(highlight-operators-face ((,class (:foreground ,base0B))))


   ;;; sh-mode
   `(sh-quoted-exec ((,class (:foreground ,unspec))))
   `(sh-heredoc ((,class (:foreground ,base0E))))


    ;;; mode-line-*
   `(mode-line ((,class (:box (:line-width 1 :color nil) :weight bold :foreground ,fg3 :background ,bg2))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,bg4 :background ,bg1))))
   `(mode-line-emphasis ((,class (:foreground ,base0C :slant italic))))
   `(mode-line-buffer-id ((,class (:foreground ,func :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
   `(anzu-mode-line ((,class (:foreground ,func))))


   ;;; ansi-color-*
   `(ansi-color-black ((,class (:foreground ,base00 :background ,base00))))
   `(ansi-color-red ((,class (:foreground ,base0A :background ,base0A))))
   `(ansi-color-green ((,class (:foreground ,base0D :background ,base0D))))
   `(ansi-color-yellow ((,class (:foreground ,base0D :background ,base0D))))
   `(ansi-color-blue ((,class (:foreground ,base0B :background ,base0B))))
   `(ansi-color-magenta ((,class (:foreground ,base09 :background ,base09))))
   `(ansi-color-cyan ((,class (:foreground ,base0F :background ,base0F))))
   `(ansi-color-white ((,class (:foreground ,base06 :background ,base06))))


   ;;; diff-hl
   `(diff-hl-insert ((,class (:foreground ,base07 :background ,bg2 :extend t))))
   `(diff-hl-delete ((,class (:foreground ,base0A :background ,bg2 :extend t))))
   `(diff-hl-change ((,class (:foreground ,base09 :background ,bg2 :extend t))))

   ;;; dired
   `(dired-directory ((,class (:foreground ,base08))))
   `(dired-marked ((,class (:foreground ,base0E))))
   `(dired-broken-symlink ((,class (:foreground ,base0A))))
   `(dired-warning ((,class (:foreground ,base0C))))
   `(dired-symlink ((,class (:foreground ,fg1 :slant italic))))


    ;;; compilation
   `(compilation-column-number ((,class (:foreground ,base03))))
   `(compilation-line-number ((,class (:foreground ,base03))))
   `(compilation-error ((,class (:foreground ,warning))))
   `(compilation-warning ((,class (:foreground ,base0D))))
   `(compilation-info ((,class (:foreground ,base0B))))
   `(compilation-mode-line-exit ((,class (:foreground ,base0D))))
   `(compilation-mode-line-fail ((,class (:foreground ,warning))))


   ;;; evil
   `(evil-ex-info ((,class (:foreground ,warning))))
   `(evil-ex-substitute-matches ((,class (:foreground ,base0B))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,base0C))))
   `(evil-ex-lazy-highlight ((,class (:background ,base08 :foreground ,bg1))))
   `(evil-snipe-matches-face ((,class (:background ,bg3))))


   ;;; which-key
   `(which-key-key-face ((,class (:foreground ,base08))))
   `(which-key-command-description-face ((,class (:foreground ,fg1))))
   `(which-key-local-map-description-face ((,class (:foreground ,fg1))))
   `(which-key-group-description-face ((,class (:foreground ,base0B))))


   ;;; magit
   `(magit-header-line ((,class (:foreground ,base0E :weight bold))))
   `(magit-section-heading ((,class (:foreground ,base0B :slant italic))))
   `(magit-branch-remote ((,class (:foreground ,base09))))
   `(magit-branch-local ((,class (:foreground ,base0C))))
   `(magit-hash ((,class (:foreground ,bg4))))
   `(magit-diff-file-heading ((,class (:foreground ,fg1))))
   `(magit-diff-removed ((,class (:foreground ,base0A))))
   `(magit-diffstat-removed ((,class (:foreground ,base0A))))
   `(magit-diff-removed-highlight ((,class (:foreground ,base0A :background ,bg2))))
   `(magit-diff-added ((,class (:foreground ,base0E))))
   `(magit-diffstat-added ((,class (:foreground ,base0E))))
   `(magit-diff-added-highlight ((,class (:foreground ,base0E :background ,bg2))))
   `(magit-log-author ((,class (:foreground ,fg1))))


   ;;; flymake
   `(flymake-warning ((,class (:underline (:color ,warning2 :style wave)))))
   `(flymake-info ((,class (:underline (:color ,base0D :style wave)))))
   `(flymake-note ((,class (:underline (:color ,base0B :style wave)))))


   ;;; vertico/marginalia/corfu/orderless
   `(vertico-current ((,class (:background ,bg2 :underline nil))))
   `(marginalia-documentation ((,class (:underline nil :foreground ,bg3))))
   `(corfu-default ((,class (:background ,bg1))))
   `(corfu-current ((,class (:background ,bg2))))
   `(corfu-annotations ((,class (:foreground ,func))))
   `(orderless-match-face-0 ((,class (:foreground ,base0B :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,base0C :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,base0D :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,base0E :weight bold))))

   ;;; paren
   `(show-paren-match ((,class (:inverse-video t))))
   `(show-paren-mismatch ((,class (:inverse-video t))))


   ;;; rainbow-delimiters-*
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,comment)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,func)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,comment)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,func)))


   ;;; term-color-*
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,base00 :background ,base00))))
   `(term-color-red ((,class (:foreground ,base0A :background ,base0A))))
   `(term-color-green ((,class (:foreground ,base0D :background ,base0D))))
   `(term-color-yellow ((,class (:foreground ,base0D :background ,base0D))))
   `(term-color-blue ((,class (:foreground ,base0B :background ,base0B))))
   `(term-color-magenta ((,class (:foreground ,base09 :background ,base09))))
   `(term-color-cyan ((,class (:foreground ,base0F :background ,base0F))))
   `(term-color-white ((,class (:foreground ,base06 :background ,base06)))))


  ;;; Legacy
  ;; emacs >= 27.1
  (when (>= emacs-major-version 27)
    (custom-theme-set-faces
     'oxocarbon
     `(tab-line              ((,class (:background ,bg1 :foreground ,fg1))))
     `(tab-line-tab          ((,class (:inherit tab-line))))
     `(tab-line-tab-inactive ((,class (:background ,bg1 :foreground ,bg4))))
     `(tab-line-tab-current  ((,class (:background ,bg1 :foreground ,fg3 :weight bold))))
     `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg1))))))
  ;; emacs >= 28
  (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'oxocarbon
     `(line-number ((t (:inherit fringe :foreground ,comment))))
     `(line-number-current-line ((t (:inherit fringe :foreground ,fg3))))))
  (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'oxocarbon
     `(tab-line-tab-modified ((,class (:foreground ,warning))))))
  (when (boundp 'font-lock-regexp-face)
    (custom-theme-set-faces
     'oxocarbon
     `(font-lock-regexp-face ((,class (:inherit font-lock-string-face :underline t)))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'oxocarbon)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; oxocarbon-theme.el ends here
