;;; oxocarbon-theme.el --- -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29"))

;;; Commentary:
;; oxocarbon-alike theme by zen in 2025

;;; Code:

(deftheme oxocarbon)
(let* ((class '((class color) (min-colors 89)))
       (base00 "#121212")
       (base01 "#262626")
       (base02 "#3a3a3a")
       (base03 "#4e4e4e")
       (base04 "#e4e4e4")
       (base05 "#eeeeee")
       (base06 "#ffffff")
       (base07 "#00afaf")
       (base08 "#5fd7d7")
       (base09 "#87afff")
       (base0A "#ff5f87")
       (base0B "#5fafff")
       (base0C "#ff87af")
       (base0D "#5faf5f")
       (base0E "#af87ff")
       (base0F "#87d7ff")
       (bg1 base00)
       (bg2 base01)
       (bg3 base02)
       (bg4 base03)
       (fg1 base04)
       (fg2 base05)
       (fg3 base06)
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
       (oper    base0B)
       (selection base02)
       (warning   base0A)
       (warning2  base0D)
       (unspec   (when (>= emacs-major-version 29) 'unspecified)))
  (custom-theme-set-faces
   'oxocarbon
   ;;; emacs <built-in>
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background ,bg2))))
   `(fringe ((,class (:background ,bg2 :foreground ,fg3))))
   `(cursor ((,class (:background ,fg3))))
   `(isearch ((,class (:weight bold :background ,warning :foreground ,fg2))))
   `(isearch-fail ((,class (:weight bold :foreground ,warning :background ,bg3))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :inherit fixed-pitch-serif))))
   `(tooltip ((,class (:background ,bg1))))
   `(match ((,class (:foreground ,base0F))))
   `(italic ((,class (:italic t))))
   `(bold ((,class (:weight bold))))
   `(vertical-border ((,class (:background ,bg1))))
   `(window-divider ((,class (:inherit vertical-border))))
   `(link ((,class (:foreground ,const :underline t))))
   `(error ((,class (:foreground ,base0A))))
   `(success ((,class (:foreground ,base0D))))
   `(warning ((,class (:foreground ,base0C))))
   `(lazy-highlight ((,class (:foreground ,bg1 :background ,base08))))
   `(trailing-whitespace ((,class :background ,bg4)))
   `(line-number ((t (:inherit fringe :foreground ,comment))))
   `(line-number-current-line ((t (:inherit fringe :foreground ,fg3))))
   `(completions-common-part ((,class (:foreground ,fg3 :weight bold))))


    ;;; font-lock-* <built-in>
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-function-name-face ((,class (:foreground ,func :weight bold))))
   `(font-lock-function-call-face ((,class (:foreground ,func :weight bold))))
   `(font-lock-keyword-face ((,class :foreground ,keyword)))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,var :slant italic))))
   `(font-lock-variable-call-face ((,class (:foreground ,var :slant italic))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-operator-face ((,class (:foreground ,oper))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(font-lock-escape-face ((,class (:foreground ,number))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-preprocessor-face ((,class :foreground ,pre)))
   `(font-lock-property-use-face ((,class :foreground ,warning)))
   `(font-lock-property-name-face ((,class :foreground ,warning)))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-regexp-face ((,class (:foreground ,str :underline t))))


   ;;; highlight-* (non ts-mode)
   `(highlight-numbers-number ((,class (:foreground ,number))))
   `(highlight-operators-face ((,class (:foreground ,oper))))


   ;;; sh-mode <built-in>
   `(sh-quoted-exec ((,class (:foreground ,unspec))))
   `(sh-heredoc ((,class (:foreground ,base0E))))

   ;;; tty-menu <built-in>
   `(tty-menu-enabled-face ((,class (:foreground ,fg1))))
   `(tty-menu-selected-face ((,class (:foreground ,base0C :background ,bg2))))
   `(tty-menu-disabled-face ((,class (:foreground ,bg4))))
   `(menu ((,class (:background ,bg2))))


   ;;; mode-line-* <built-in>
   `(mode-line ((,class (:box (:line-width 1) :weight bold :foreground ,fg3 :background ,bg2))))
   `(mode-line-active ((,class (:inherit fixed-pitch-serif))))
   `(mode-line-inactive ((,class (:foreground ,bg4 :background ,bg1 :inherit fixed-pitch-serif))))
   `(mode-line-emphasis ((,class (:foreground ,base0C :slant italic))))
   `(mode-line-buffer-id ((,class (:foreground ,base0C :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,base09 :box nil :weight bold))))
   `(anzu-mode-line ((,class (:foreground ,base0C))))


   ;;; eshell <built-in>
   `(eshell-prompt ((,class (:foreground ,base0C))))


   ;;; widget <built-in>
   `(custom-button ((,class (:background ,bg3 :foreground ,fg3 :box (:line-width 2 :color ,bg2)))))
   `(custom-button-mouse ((,class (:inherit custom-button ))))
   `(custom-button-pressed ((,class (:inherit custom-button :box (:color ,fg1)))))
   `(custom-state ((,class (:inherit marginalia-documentation))))
   `(widget-field ((,class (:background ,bg3 :foreground ,fg3))))
   `(widget-inactive ((,class (:background ,bg3 :foreground ,bg4))))
   `(widget-single-line-field ((,class (:inherit widget-field))))


   ;;; eldoc <built-in>
   `(eldoc-highlight-function-argument ((,class (:foreground ,base0C :slant italic))))


   ;;; cider
   `(cider-error-overlay-face ((,class (:inherit error))))
   `(cider-result-overlay-face ((,class (:box (:line-width 1 :color ,bg3)) :backgrond ,bg2)))
   `(cider-warning-highlight-face ((,class (:underline (:color ,base0B :style wave)))))
   `(cider-error-highlight-face ((,class (:underline (:color ,base0C :style wave)))))


   ;;; outline-mode <built-in>
   `(outline-1 ((,class (:foreground ,base0C :weight bold))))
   `(outline-2 ((,class (:foreground ,base0B :weight bold))))
   `(outline-3 ((,class (:foreground ,base08 :weight bold))))
   `(outline-4 ((,class (:foreground ,base0D :weight bold))))
   `(outline-5 ((,class (:foreground ,base09 :weight bold))))
   `(outline-6 ((,class (:foreground ,base0F :weight bold))))


   ;;; org-mode <built-in>
   `(org-special-keyword ((,class (:foreground ,bg4))))
   `(org-document-info-keyword ((,class (:foreground ,bg4))))
   `(org-document-info ((,class (:foreground ,base0A))))
   `(org-document-title ((,class (:foreground ,base0A :weight bold))))
   `(org-block-begin-line ((,class (:foreground ,base0F :background ,bg2))))
   `(org-block-end-line ((,class (:foreground ,base0F :background ,bg2))))
   `(org-code ((,class (:foreground ,base0B :background ,bg2))))
   `(org-todo ((,class (:foreground ,base0A))))
   `(org-done ((,class (:foreground ,base0D))))
   `(org-date ((,class (:foreground ,base07))))
   `(org-verbatim ((,class (:foreground ,base0F))))
   `(org-checkbox ((,class (:foreground ,fg1 :weight bold))))


   ;;; ansi-color-* <built-in>
   `(ansi-color-black ((,class (:foreground ,base00 :background ,base00))))
   `(ansi-color-red ((,class (:foreground ,base0A :background ,base0A))))
   `(ansi-color-green ((,class (:foreground ,base0D :background ,base0D))))
   `(ansi-color-yellow ((,class (:foreground ,base0D :background ,base0D))))
   `(ansi-color-blue ((,class (:foreground ,base0B :background ,base0B))))
   `(ansi-color-magenta ((,class (:foreground ,base09 :background ,base09))))
   `(ansi-color-cyan ((,class (:foreground ,base0F :background ,base0F))))
   `(ansi-color-white ((,class (:foreground ,base06 :background ,base06))))


   ;;; eat-color-*
   `(eat-term-color-0 ((,class (:foreground ,base01 :background ,base01))))
   `(eat-term-color-1 ((,class (:foreground ,base0A :background ,base0A))))
   `(eat-term-color-2 ((,class (:foreground ,base0D :background ,base0D))))
   `(eat-term-color-3 ((,class (:foreground ,base0F :background ,base0F))))
   `(eat-term-color-4 ((,class (:foreground ,base0B :background ,base0B))))
   `(eat-term-color-5 ((,class (:foreground ,base0E :background ,base0E))))
   `(eat-term-color-6 ((,class (:foreground ,base07 :background ,base07))))
   `(eat-term-color-7 ((,class (:foreground ,base05 :background ,base05))))
   `(eat-term-color-8 ((,class (:foreground ,base02 :background ,base02))))
   `(eat-term-color-9 ((,class (:foreground ,base0C :background ,base0C))))
   `(eat-term-color-10 ((,class (:foreground ,base0D :background ,base0D))))
   `(eat-term-color-11 ((,class (:foreground ,base0F :background ,base0F))))
   `(eat-term-color-12 ((,class (:foreground ,base09 :background ,base09))))
   `(eat-term-color-13 ((,class (:foreground ,base0E :background ,base0E))))
   `(eat-term-color-14 ((,class (:foreground ,base08 :background ,base08))))
   `(eat-term-color-15 ((,class (:foreground ,base06 :background ,base06))))


   ;;; diff-hl
   `(diff-hl-insert ((,class (:foreground ,base0E :background ,bg2))))
   `(diff-hl-delete ((,class (:foreground ,base0A :background ,bg2))))
   `(diff-hl-change ((,class (:foreground ,base09 :background ,bg2))))


   ;;; diff <built-in>
   `(diff-changed ((,class (:foreground ,base09 :background ,bg2))))
   `(diff-added ((,class (:foreground ,base0E :background ,bg2))))
   `(diff-removed ((,class (:foreground ,base0A :background ,bg2))))
   `(diff-indicator-changed ((,class (:foreground ,base09))))
   `(diff-indicator-added ((,class (:foreground ,base0E))))
   `(diff-indicator-removed ((,class (:foreground ,base0A))))
   `(diff-header ((,class (:foreground ,fg1))))
   `(diff-file-header ((,class (:foreground ,fg1))))
   `(diff-hunk-header ((,class (:foreground ,base0D))))
   `(diff-refine-added ((,class (:background ,base0E :foreground ,fg2))))
   `(diff-refine-removed ((,class (:background ,base0A :foreground ,fg2))))
   `(diff-refine-changed ((,class (:background ,base09 :foreground ,fg2))))


   ;;; ediff <built-in>
   `(ediff-fine-diff-A ((,class (:foreground ,base0A))))
   `(ediff-fine-diff-B ((,class (:foreground ,base0E))))
   `(ediff-fine-diff-C ((,class (:foreground ,base09))))
   `(ediff-current-diff-A ((,class (:inherit ediff-fine-diff-A))))
   `(ediff-current-diff-B ((,class (:inherit ediff-fine-diff-B))))
   `(ediff-current-diff-C ((,class (:inherit ediff-fine-diff-C))))
   `(ediff-even-diff-A ((,class (:inherit ediff-fine-diff-A))))
   `(ediff-even-diff-B ((,class (:inherit ediff-fine-diff-B))))
   `(ediff-even-diff-C ((,class (:inherit ediff-fine-diff-C))))
   `(ediff-odd-diff-A ((,class (:inherit ediff-fine-diff-A))))
   `(ediff-odd-diff-B ((,class (:inherit ediff-fine-diff-B))))
   `(ediff-odd-diff-C ((,class (:inherit ediff-fine-diff-C))))


   ;;; dired <built-in>
   `(dired-directory ((,class (:foreground ,base08))))
   `(dired-marked ((,class (:foreground ,base0E))))
   `(dired-broken-symlink ((,class (:foreground ,base0A))))
   `(dired-warning ((,class (:foreground ,base0C))))
   `(dired-symlink ((,class (:foreground ,fg1 :slant italic))))


   ;;; compilation <built-in>
   `(compilation-column-number ((,class (:foreground ,base03))))
   `(compilation-line-number ((,class (:foreground ,base03))))
   `(compilation-error ((,class (:foreground ,base0C))))
   `(compilation-warning ((,class (:foreground ,base0B))))
   `(compilation-info ((,class (:foreground ,base0D))))
   `(compilation-mode-line-exit ((,class (:foreground ,base0D))))
   `(compilation-mode-line-fail ((,class (:foreground ,warning))))


   ;;; evil
   `(evil-ex-info ((,class (:foreground ,warning))))
   `(evil-ex-substitute-matches ((,class (:foreground ,bg1 :background ,base0B))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,bg1 :background ,base0C))))
   `(evil-ex-lazy-highlight ((,class (:background ,base08 :foreground ,bg1))))
   `(evil-snipe-matches-face ((,class (:background ,bg3))))


   ;;; anzu
   `(anzu-match-1 ((,class (:foreground ,base0B))))
   `(anzu-match-2 ((,class (:foreground ,base0E))))
   `(anzu-match-3 ((,class (:foreground ,base0F))))
   `(anzu-replace-highlight ((,class (:background ,base0B :foreground ,bg1))))
   `(anzu-replace-to ((,class (:foreground ,bg1 :background ,base0C))))


   ;;; which-key <built-in>
   `(which-key-key-face ((,class (:foreground ,base08))))
   `(which-key-command-description-face ((,class (:foreground ,fg1))))
   `(which-key-local-map-description-face ((,class (:foreground ,fg1))))
   `(which-key-group-description-face ((,class (:foreground ,base0B))))


   ;;; vc-mode <built-in>
   `(vc-state-base ((,class (:foreground ,base0F))))
   `(vc-edited-state ((,class (:foreground ,base07))))


   ;;; magit
   `(magit-header-line ((,class (:foreground ,base0E :weight bold))))
   `(magit-section-heading ((,class (:foreground ,base0B :slant italic :inherit fixed-pitch-serif))))
   `(magit-branch-remote ((,class (:foreground ,base09))))
   `(magit-branch-local ((,class (:foreground ,base0C))))
   `(magit-hash ((,class (:foreground ,bg4))))
   `(magit-log-author ((,class (:foreground ,fg1))))
   `(magit-diff-file-heading ((,class (:foreground ,fg1))))
   `(magit-diff-removed ((,class (:foreground ,base0A))))
   `(magit-diffstat-removed ((,class (:foreground ,base0A))))
   `(magit-diff-removed-highlight ((,class (:foreground ,base0A :background ,bg2))))
   `(magit-diff-added ((,class (:foreground ,base0E))))
   `(magit-diffstat-added ((,class (:foreground ,base0E))))
   `(magit-diff-added-highlight ((,class (:foreground ,base0E :background ,bg2))))
   `(magit-diff-hunk-heading ((,class (:background ,bg2))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg3 :foreground ,base0D))))


   ;;; transient
   `(transient-inactive-argument ((,class (:foreground ,bg3 :slant italic))))
   `(transient-argument ((,class (:foreground ,fg3 :slant italic))))
   `(transient-key-exit ((,class (:foreground ,base0B))))
   `(transient-key-stay ((,class (:foreground ,base0B))))
   `(transient-key-stack ((,class (:foreground ,base0B))))
   `(transient-heading ((,class (:foreground ,base0B :inherit fixed-pitch-serif))))


   ;;; flymake <built-in>
   `(flymake-warning ((,class (:underline (:color ,base0B :style wave)))))
   `(flymake-error ((,class (:underline (:color ,base0C :style wave)))))
   `(flymake-note ((,class (:underline (:color ,base0D :style wave)))))


   ;;; flycheck
   `(flycheck-warning ((,class (:underline (:color ,base0B :style wave)))))
   `(flycheck-error ((,class (:underline (:color ,base0C :style wave)))))
   `(flycheck-note ((,class (:underline (:color ,base0D :style wave)))))


   ;;; vertico/marginalia/corfu/orderless
   `(vertico-current ((,class (:background ,bg2 :underline nil))))
   `(marginalia-documentation ((,class (:underline nil :foreground ,bg3))))
   `(corfu-default ((,class (:background ,bg1 :foreground ,fg1))))
   `(corfu-popupinfo ((,class (:background ,bg1 :foreground ,fg1 :inherit fixed-pitch-serif))))
   `(corfu-current ((,class (:background ,bg2 :foreground ,fg3 :weight bold))))
   `(corfu-annotations ((,class (:foreground ,func :inherit fixed-pitch-serif))))
   `(orderless-match-face-0 ((,class (:foreground ,base08 :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,base0C :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,base0D :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,base0E :weight bold))))


   ;;; paren <built-in>
   `(show-paren-match ((,class (:inverse-video t))))
   `(show-paren-mismatch ((,class (:inverse-video t))))


   ;;; rainbow-delimiters-*
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,func)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,comment)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,func)))


   ;;; term-color-* <built-in>
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,base00 :background ,base00))))
   `(term-color-red ((,class (:foreground ,base0A :background ,base0A))))
   `(term-color-green ((,class (:foreground ,base0D :background ,base0D))))
   `(term-color-yellow ((,class (:foreground ,base0D :background ,base0D))))
   `(term-color-blue ((,class (:foreground ,base0B :background ,base0B))))
   `(term-color-magenta ((,class (:foreground ,base09 :background ,base09))))
   `(term-color-cyan ((,class (:foreground ,base0F :background ,base0F))))
   `(term-color-white ((,class (:foreground ,base06 :background ,base06))))


   ;;; tab-line-* <built-in>
   `(tab-line              ((,class (:background ,bg1 :foreground ,fg1 :inherit fixed-pitch-serif))))
   `(tab-line-tab          ((,class (:inherit tab-line))))
   `(tab-line-tab-inactive ((,class (:background ,bg1 :foreground ,bg4))))
   `(tab-line-tab-current  ((,class (:background ,bg1 :foreground ,fg3 :weight bold))))
   `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg1))))
   `(tab-line-tab-modified ((,class (:foreground ,warning))))))



;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'oxocarbon)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; oxocarbon-theme.el ends here
