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

;;; Commentary:
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
       (bg4 base04)
       (builtin base09)
       (number  base0F)
       (keyword base09)
       (const   base07)
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
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background ,bg2))))
   `(fringe ((,class (:background ,bg2 :foreground ,fg3))))
   `(cursor ((,class (:background ,fg3))))
   `(isearch ((,class (:weight bold :foreground ,warning :background ,bg2))))
   `(isearch-fail ((,class (:weight bold :foreground ,warning2 :background ,bg3))))
   `(minibuffer-prompt ((,class (:foreground ,keyword))))
   `(tooltip ((,class (:background ,bg1)))l)
   `(match ((,class (:foreground ,base0F))))
   `(italic ((,class (:italic t))))
   `(bold ((,class (:weight bold))))
   `(vertical-border ((,class (:background ,bg3))))
   `(link ((,class (:foreground ,const :underline t))))
   `(error ((,class (:foreground ,base0A))))
   `(success ((,class (:foreground ,base0D))))
   `(warning ((,class (:foreground ,base0C))))

    ;;; font-lock-* faces
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-function-name-face ((,class (:foreground ,func :weight bold))))
   `(font-lock-keyword-face ((,class :foreground ,keyword)))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-preprocessor-face ((,class :foreground ,const)))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-string-face ((,class (:foreground ,str))))

    ;;; mode-line-* faces
   `(mode-line ((,class (:box (:line-width 1 :color nil) :weight bold :foreground ,fg3 :background ,bg2))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,bg4 :background ,bg1))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))
   `(mode-line-buffer-id ((,class (:foreground ,func))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
   `(anzu-mode-line ((,class (:foreground ,func))))

   `(ansi-color-black ((,class (:foreground ,base00 :background ,base00))))
   `(ansi-color-red ((,class (:foreground ,base0A :background ,base0A))))
   `(ansi-color-green ((,class (:foreground ,base0D :background ,base0D))))
   `(ansi-color-yellow ((,class (:foreground ,base0D :background ,base0D))))
   `(ansi-color-blue ((,class (:foreground ,base0B :background ,base0B))))
   `(ansi-color-magenta ((,class (:foreground ,base09 :background ,base09))))
   `(ansi-color-cyan ((,class (:foreground ,base0F :background ,base0F))))
   `(ansi-color-white ((,class (:foreground ,base06 :background ,base06))))

   `(corfu-default ((,class (:background ,base00))))

   `(diff-hl-insert ((,class (:foreground ,base0D :background ,base0D))))
   `(diff-hl-delete ((,class (:foreground ,base0A :background ,base0C))))
   `(diff-hl-change ((,class (:foreground ,base09 :background ,base09))))

   `(dired-directory ((,class (:foreground ,base08))))
   `(dired-marked ((,class (:foreground ,base0E))))
   `(dired-broken-symlink ((,class (:foreground ,base0A))))
   `(dired-warning ((,class (:foreground ,base0C))))
   `(dired-symlink ((,class (:foreground ,base0B))))

    ;;; compilation mode
   `(compilation-column-number ((,class (:foreground ,base03))))
   `(compilation-line-number ((,class (:foreground ,base03))))
   `(compilation-error ((,class (:foreground ,warning))))
   `(compilation-warning ((,class (:foreground ,base0B))))
   `(compilation-info ((,class (:foreground ,base0D))))
   `(compilation-mode-line-exit ((,class (:foreground ,base0D))))
   `(compilation-mode-line-fail ((,class (:foreground ,warning))))

   ;;; evil
   `(evil-ex-info ((,class (:foreground ,warning))))
   `(evil-ex-substitute-matches ((,class (:foreground ,base0B))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,base0C))))
   `(evil-snipe-matches-face ((,class (:background ,bg3))))


   `(magit-header-line ((,class (:foreground ,base0E))))
   `(magit-tag ((,class (:foreground ,base09))))
   `(magit-head ((,class (:foreground ,base0C))))

   ;;; flymake
   `(flymake-warning ((,class (:underline (:color ,warning2 :style wave)))))
   `(flymake-info ((,class (:underline (:color ,base0D :style wave)))))
   `(flymake-note ((,class (:underline (:color ,base0B :style wave)))))

   `(vertico-current ((,class (:background ,bg2 :underline nil))))
   `(marginalia-documentation ((,class (:underline nil :foreground ,bg3))))

   `(highlight-numbers-number ((,class (:foreground ,number))))
   `(highlight-operators-face ((,class (:foreground ,func))))
   `(term-color-oxocarbon ((,class (:foreground ,fg2 :background ,unspec))))

   `(trailing-whitespace ((,class :foreground ,unspec :background ,warning)))

   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,comment)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,func)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,comment)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,func)))

   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))

   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,base00 :background ,base00))))
   `(term-color-red ((,class (:foreground ,base0A :background ,base0A))))
   `(term-color-green ((,class (:foreground ,base0D :background ,base0D))))
   `(term-color-yellow ((,class (:foreground ,base0D :background ,base0D))))
   `(term-color-blue ((,class (:foreground ,base0B :background ,base0B))))
   `(term-color-magenta ((,class (:foreground ,base09 :background ,base09))))
   `(term-color-cyan ((,class (:foreground ,base0F :background ,base0F))))
   `(term-color-white ((,class (:foreground ,base06 :background ,base06)))))

  ;; Legacy
  ;; emacs >= 28
  (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'oxocarbon
     `(line-number ((t (:inherit fringe :foreground ,comment))))
     `(line-number-current-line ((t (:inherit fringe :foreground ,fg3))))))
  ;; emacs >= 27.1
  (when (>= emacs-major-version 27)
    (custom-theme-set-faces
     'oxocarbon
     `(tab-line              ((,class (:background ,bg2 :foreground ,fg3))))
     `(tab-line-tab          ((,class (:inherit tab-line))))
     `(tab-line-tab-inactive ((,class (:background ,bg2 :foreground ,fg3))))
     `(tab-line-tab-current  ((,class (:background ,bg1 :foreground ,fg1))))
     `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg2))))))
  (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'oxocarbon
     `(tab-line-tab-modified ((,class (:foreground ,warning2 :weight bold))))))
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
