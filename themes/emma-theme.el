;; emma-theme
;; Emacs theme based on Vim's gruvbox color scheme.

(require 'autothemer)

(autothemer-deftheme emma "This is Emma's theme"
  ((((class color) (min-colors 32000)) ((class color) (min-colors 90)) t)
   (em-dark0_hard "#1d2021" "#1c1c1c") ;234
   (em-dark0 "#282828" "#262626") ;235
   (em-dark0_soft "#32302f" "#303030") ;236
   (em-dark1 "#3c3836" "#3a3a3a") ;237
   (em-dark2 "#504945" "#4e4e4e") ;239
   (em-dark3 "#665c54" "#626262") ;241
   (em-dark4 "#7c6f64" "#767676") ;243

   (em-gray "#928374" "#8a8a8a") ;245

   (em-light0_hard "#f9f5d7" "#ffffd7") ;230
   (em-light0 "#fbf1c7" "#ffffaf") ;229
   (em-light0_soft "#f2e5bc" "#ffffaf") ;228
   (em-light1 "#ebdbb2" "#ffdfaf") ;223
   (em-light2 "#d5c4a1" "#bcbcbc") ;250
   (em-light3 "#bdae93" "#a8a8a8") ;248
   (em-light4 "#a89984" "#949494") ;246

   (em-bright_red "#fb4934" "#d75f5f") ;167
   (em-bright_green "#b8bb26" "#afaf00") ;142
   (em-bright_yellow "#fabd2f" "#ffaf00") ;214
   (em-bright_blue "#83a598" "#87afaf") ;109
   (em-bright_purple "#d3869b" "#d787af") ;175
   (em-bright_aqua "#8ec07c" "#87af87") ;108
   (em-bright_orange "#fe8019" "#ff8700") ;208

   (em-neutral_red "#cc241d" "#d75f5f") ;124
   (em-neutral_green "#98971a" "#afaf00") ;106
   (em-neutral_yellow "#d79921" "#ffaf00") ;172
   (em-neutral_blue "#458588" "#87afaf") ;66
   (em-neutral_purple "#b16286" "#d787af") ;132
   (em-neutral_aqua "#689d6a" "#87af87") ;72
   (em-neutral_orange "#d65d0e" "#ff8700") ;166

   (em-faded_red "#9d0006" "#870000") ;88
   (em-faded_green "#79740e" "#878700") ;100
   (em-faded_yellow "#b57614" "#af8700") ;136
   (em-faded_blue "#076678" "#005f87") ;24
   (em-faded_purple "#8f3f71" "#875f87") ;96
   (em-faded_aqua "#427b58" "#5f8787") ;66
   (em-faded_orange "#af3a03" "#af5f00") ;130

   (em-bg em-dark0)

   (em-fg em-light0)

   (em-delimiter-one "#458588" "#008787")
   (em-delimiter-two "#b16286" "#d75f87")
   (em-delimiter-three "#8ec07c" "#87af87")
   (em-delimiter-four "#d65d0e" "#d75f00")
   (em-black "#000000" "#000000")
   (em-white "#ffffff" "#ffffff"))

  ((default (:background em-bg :foreground em-light0))
   (cursor (:background em-light0))
   (mode-line (:background em-dark2 :foreground em-light2 :box nil))
   (mode-line-inactive (:background em-dark1 :foreground em-light4 :box nil))
   (fringe (:background em-bg))
   (hl-line (:background em-dark1))
   (region (:background em-dark2))
   (secondary-selection (:background em-dark1))
   (minibuffer-prompt (:background em-bg :foreground em-neutral_green :bold t))
   (vertical-border (:foreground em-dark2))
   (link (:foreground em-faded_blue :underline t))
   (shadow (:foreground em-dark4))

   ;; Build-in syntax

   (font-lock-builtin-face (:foreground em-neutral_orange))
   (font-lock-constant-face (:foreground em-neutral_purple))
   (font-lock-comment-face (:foreground em-dark4 :slant 'italic :italic t))
   (font-lock-function-name-face (:foreground em-neutral_yellow))
   (font-lock-keyword-face (:foreground em-neutral_red))
   (font-lock-string-face (:foreground em-neutral_green))
   (font-lock-variable-name-face (:foreground em-neutral_blue))
   (font-lock-type-face (:foreground em-neutral_purple))
   (font-lock-warning-face (:foreground em-neutral_red :bold t))

   ;; Basic faces

   (error (:foreground em-bright_red :bold t))
   (success (:foreground em-bright_green :bold t))
   (warning (:foreground em-bright_yellow :bold t))
   (trailing_whitespace (:background em-neutral_red))
   (escape_glyph (:foreground em-bright_aqua))
   (header-line (:background em-dark0 :foreground em-light3 :box nil :inherit nil))
   (highlight (:background em-dark4 :foreground em-light0))
   (homoglyph (:foreground em-bright_yellow))
   (match (:foreground em-dark0 :background em-neutral_blue))

   ;; Customize faces

   (widget-field (:background em-dark3))
   (custom-group-tag (:foreground em-neutral_blue :weight 'bold))
   (custom-variable-tag (:foreground em-neutral_blue :weight 'bold))

   ;; whitespace-mode

   (whitespace-space (:background em-bg :foreground em-dark4))
   (whitespace-hspace (:background em-bg :foreground em-dark4))
   (whitespace-tab (:background em-bg :foreground em-dark4))
   (whitespace-newline (:background em-bg :foreground em-dark4))
   (whitespace-line (:background em-dark1 :foreground em-neutral_red))
   (whitespace-space-before-tab (:background em-bg :foreground em-dark4))
   (whitespace-indentation (:background em-bg :foreground em-dark4))
   (whitespace-empty (:background nil :foreground nil))
   (whitespace-space-after-tab (:background em-bg :foreground em-dark4))

   ;; RainbowDelimiters

   (rainbow-delimiters-depth-1-face (:foreground em-delimiter-one))
   (rainbow-delimiters-depth-2-face (:foreground em-delimiter-two))
   (rainbow-delimiters-depth-3-face (:foreground em-delimiter-three))
   (rainbow-delimiters-depth-4-face (:foreground em-delimiter-four))
   (rainbow-delimiters-depth-5-face (:foreground em-delimiter-one))
   (rainbow-delimiters-depth-6-face (:foreground em-delimiter-two))
   (rainbow-delimiters-depth-7-face (:foreground em-delimiter-three))
   (rainbow-delimiters-depth-8-face (:foreground em-delimiter-four))
   (rainbow-delimiters-depth-9-face (:foreground em-delimiter-one))
   (rainbow-delimiters-depth-10-face (:foreground em-delimiter-two))
   (rainbow-delimiters-depth-11-face (:foreground em-delimiter-three))
   (rainbow-delimiters-depth-12-face (:foreground em-delimiter-four))
   (rainbow-delimiters-unmatched-face (:background nil :foreground em-light0))

   ;; line numbers

   (line-number (:foreground em-dark2 :background em-dark0))
   (line-number-current-line (:foreground em-neutral_orange :background em-dark1))
   (linum (:foreground em-dark2 :background em-dark0))
   (linum-highlight-face (:foreground em-neutral_orange :background em-dark1))
   (linum-relative-current-face (:foreground em-neutral_orange :background em-dark1))

   ;; Highlight indentation mode

   (highlight-indentation-current-column-face (:background em-dark2))
   (highlight-indentation-face (:background em-dark1))

   ;; Smartparens

   (sp-pair-overlay-face (:background em-dark2))
   (sp-show-pair-match-face (:background em-dark2))
   (sp-show-pair-mismatch-face (:background em-neutral_red))

   ;; elscreen

   (elscreen-tab-background-face (:background em-bg :box nil))
   (elscreen-tab-control-face (:background em-dark2 :foreground em-neutral_red :underline nil :box nil))
   (elscreen-tab-current-screen-face (:background em-dark4 :foreground em-dark0 :box nil))
   (elscreen-tab-other-screen-face (:background em-dark2 :foreground em-light4 :underline nil :box nil))

   ;; ag (the-silver-searcher)

   (ag-hit-face (:foreground em-neutral_blue))
   (ag-match-face (:foreground em-neutral_red))

   ;; diffs

   (diff-changed (:background nil :foreground em-light1))
   (diff-added (:background nil :foreground em-neutral_green))
   (diff-removed (:background nil :foreground em-neutral_red))
   (diff-indicator-changed (:inherit 'diff-changed))
   (diff-indicator-added (:inherit 'diff-added))
   (diff-indicator-removed (:inherit 'diff-removed))

   ;; js2

   (js2-warning (:underline (:color em-bright_yellow :style 'wave)))
   (js2-error (:underline (:color em-bright_red :style 'wave)))
   (js2-external-variable (:underline (:color em-bright_aqua :style 'wave)))
   (js2-jsdoc-tab (:background nil :foreground em-gray))
   (js2-jsdoc-type (:background nil :foreground em-light4))
   (js2-jsdoc-value (:background nil :foreground em-light3))
   (js2-function-param (:background nil :foreground em-bright_aqua))
   (js2-function-call (:background nil :foreground em-bright_blue))
   (js2-instance-member (:background nil :foreground em-bright_orange))
   (js2-private-member (:background nil :foreground em-faded_yellow))
   (js2-private-function-call (:background nil :foreground em-faded_aqua))
   (js2-jsdoc-html-tag-name (:background nil :foreground em-light4))
   (js2-jsdoc-html-tag-delimiter (:background nil :foreground em-light3))

   ;; popup

   (popup-face (:underline nil :foreground em-light1 :background em-dark1))
   (popup-menu-mouse-face (:underline nil :foreground em-light0 :background em-faded_green))
   (popup-menu-selection-face (:underline nil :foreground em-light0 :background em-faded_green))
   (popup-tip-face (:underline nil :foreground em-light2 :background em-dark2))

   ;; company-mode

   (company-scrollbar-bg (:background em-dark1))
   (company-scrollbar-fg (:background em-dark0_soft))
   (company-tooltip (:background em-dark0_soft))
   (company-tooltip-annotation (:foreground em-neutral_green))
   (company-tooltip-annotation-selection (:inherit 'company-tooltip-annotation))
   (company-tooltip-selection (:foreground em-neutral_purple :background em-dark2))
   (company-tooltip-common (:foreground em-neutral_blue :underline t))
   (company-tooltip-common-selection (:foreground em-neutral_blue :underline t))
   (company-preview-common (:foreground em-light0))
   (company-preview (:background em-bright_blue))
   (company-preview-search (:background em-bright_aqua))
   (company-template-field (:foreground em-black :background em-neutral_yellow))
   (company-echo-common (:foreground em-faded_red))

   ;; tool tips

   (tooltip (:foreground em-dark2 :background em-dark1))

   ;; term

   (term-color-black (:foreground em-dark2 :background em-dark1))
   (term-color-blue (:foreground em-bright_blue :background em-neutral_blue))
   (term-color-cyan (:foreground em-bright_aqua :background em-neutral_aqua))
   (term-color-green (:foreground em-bright_green :background em-neutral_green))
   (term-color-magenta (:foreground em-bright_purple :background em-neutral_purple))
   (term-color-red (:foreground em-bright_red :background em-neutral_red))
   (term-color-white (:foreground em-light1 :background em-light1))
   (term-color-yellow (:foreground em-bright_yellow :background em-neutral_yellow))
   (term-default-fg-color (:foreground em-light0))
   (term-default-bg-color (:foreground em-bg))

   ;; message-mode

   (message-header-to (:inherit 'font-lock-variable-name-face))
   (message-header-cc (:inherit 'font-lock-variable-name-face))
   (message-header-subject (:foreground em-neutral_orange :weight 'bold))
   (message-header-newsgroups (:foreground em-neutral_yellow :weight 'bold))
   (message-header-other (:inherit 'font-lock-variable-name-face))
   (message-header-name (:inherit 'font-lock-keyword-face))
   (message-header-xheader (:foreground em-faded_blue))
   (message-separator (:inherit 'font-lock-comment-face))
   (message-cited-text (:inherit 'font-lock-comment-face))
   (message-mml (:foreground em-faded_green :weight 'bold))

   ;; org-mode

   (org-hide (:foreground em-dark0))
   (org-level-1 (:foreground em-neutral_blue))
   (org-level-2 (:foreground em-neutral_yellow))
   (org-level-3 (:foreground em-neutral_purple))
   (org-level-4 (:foreground em-neutral_red))
   (org-level-5 (:foreground em-neutral_green))
   (org-level-6 (:foreground em-neutral_aqua))
   (org-level-7 (:foreground em-faded_blue))
   (org-level-8 (:foreground em-neutral_orange))
   (org-special-keyword (:inherit 'font-lock-comment-face))
   (org-drawer (:inherit 'font-lock-function-face))
   (org-column (:background em-dark0))
   (org-column-title (:background em-dark0 :underline t :weight 'bold))
   (org-waraning (:foreground em-neutral_red :weight 'bold :underline nil :bold t))
   (org-archived (:foreground em-light0 :weight 'bold))
   (org-link (:foreground em-faded_aqua :underline t))
   (org-footnote (:foreground em-neutral_aqua :underline t))
   (org-ellipsis (:foreground em-light4 :underline t))
   (org-date (:foreground em-neutral_blue :underline t))
   (org-sexp-date (:foreground em-faded_blue :underline t))
   (org-tag (:bold t :weight 'bold))
   (org-list-dt (:bold t :weight 'bold))
   (org-todo (:foreground em-neutral_red :weight 'bold :bold t))
   (org-done (:foreground em-neutral_aqua :weight 'bold :bold t))
   (org-agenda-done (:foreground em-neutral_aqua))
   (org-headline-done (:foreground em-neutral_aqua))
   (org-table (:foreground em-neutral_blue))
   (org-formula (:foreground em-neutral_yellow))
   (org-document-title (:foreground em-faded_blue))
   (org-document-info (:foreground em-faded_blue))
   (org-agenda-structure (:inherit 'font-lock-comment-face))
   (org-agenda-date-today (:foreground em-light0 :slant 'italic :weight 'bold :italic t))
   (org-scheduled (:foreground em-neutral_yellow))
   (org-scheduled-today (:foreground em-neutral_blue))
   (org-scheduled-previously (:foreground em-faded_red))
   (org-upcoming-deadline (:inherit 'font-lock-keyword-face))
   (org-deadline-announce (:foreground em-faded_red))
   (org-time-grid (:foreground em-faded_orange))
   (org-latex-and-related (:foreground em-neutral_blue))
   (org-scheduled (:foreground em-neutral_yellow))
   (org-scheduled-today (:foreground em-neutral_blue))
   (org-scheduled-previously (:foreground em-faded_red))
   (org-upcoming-deadline (:inherit 'font-lock-keyword-face))
   (org-deadline-announce (:foreground em-faded_red))
   (org-time-grid (:foreground em-faded_orange))
   (org-latex-and-related (:foreground em-neutral_blue))

   ;; org-habit

   (org-habit-clear-face (:background em-faded_blue))
   (org-habit-clear-future-face (:background em-neutral_blue))
   (org-habit-ready_face (:background em-faded_green))
   (org-habit-ready-future-face (:background em-neutral_green))
   (org-habit-alert-face (:background em-faded_yellow))
   (org-habit-alert-future-face (:background em-neutral_yellow))
   (org-habit-overdue-face (:background em-faded_red))

   ;; elfeed

   (elfeed-search-title-face (:foreground em-gray))
   (elfeed-search-unreed-title-face (:foreground em-light0))
   (elfeed-search-date-face (:inherit 'font-lock-builtin-face :underline t))
   (elfeed-search-feed-face (:inherit 'font-lock-variable-name-face))
   (elfeed-search-tag-face (:inherit 'font-lock-keyword-face))
   (elfeed-search-last-update-face (:inherit 'font-lock-comment-face))
   (elfeed-search-unread-count-face (:inherit 'font-lock-comment-face))
   (elfeed-search-filter-face (:inherit 'font-lock-string-face))

   ;; smart-mode-line

   (sml/global (:foreground em-light3 :inverse-video nil))
   (sml/modes (:foreground em-bright_green))
   (sml/filename (:foreground em-bright_red :weight 'bold))
   (sml/prefix (:foreground em-light1))
   (sml/read-only (:foreground em-neutral_blue))
   (persp-selected-face (:foreground em-neutral_orange))

   ;; isearch

   (isearch (:foreground em-black :background em-neutral_orange))
   (lazy-highlight (:foreground em-black :background em-neutral_yellow))
   (isearch-fail (:foreground em-light0 :background em-bright_red))

   ;; markdown-mode

   (markdown-header-face-1 (:foreground em-neutral_blue))
   (markdown-header-face-2 (:foreground em-neutral_yellow))
   (markdown-header-face-3 (:foreground em-neutral_purple))
   (markdown-header-face-4 (:foreground em-neutral_red))
   (markdown-header-face-5 (:foreground em-neutral_green))
   (markdown-header-face-6 (:foreground em-neutral_aqua))

   ;; anzu-mode

   (anzu-mode-line (:foreground em-bright_yellow :weight 'bold))
   (anzu-match-1 (:background em-bright_green))
   (anzu-match-2 (:background em-faded_yellow))
   (anzu-match-3 (:background em-faded_aqua))
   (anzu-replace-to (:foreground em-bright_yellow))
   (anzu-replace-hightlight (:inherit 'isearch))

   ;; ace-jump-mode

   (ace-jump-face-background (:foreground em-light4 :background em-bg :inverse-video nil))
   (ace-jump-face-foreground (:foreground em-bright_red :background em-bg :inverse-video nil))

   ;; ace-window

   (aw-background-face (:foreground em-light1 :background em-bg :inverse-video nil))
   (aw-leading-char-face (:foreground em-bright_red :background em-bg :height 4.0))

   ;; show-paren

   (show-paren-match (:background em-dark3 :weight 'bold))
   (show-paren-mismatch (:background em-bright_red :foreground em-dark3 :weight 'bold))

   ;; ivy

   (ivy-current-match (:foreground em-light0_hard :weight 'bold :underline t))
   (ivy-minibuffer-match-face-1 (:foreground em-neutral_orange))
   (ivy-minibuffer-match-face-2 (:foreground em-neutral_yellow))
   (ivy-minibuffer-match-face-3 (:foreground em-faded_orange))
   (ivy-minibuffer-match-face-4 (:foreground em-faded_yellow))

   ;; magit

   (magit-bisect-bad (:foreground em-faded_red))
   (magit-bisect-good (:foreground em-faded_green))
   (magit-bisect-skip (:foreground em-faded_yellow))
   (magit-blame-heading (:foreground em-light0 :background em-dark2))
   (magit-branch-local (:foreground em-bright_blue))
   (magit-branch-current (:underline em-bright_blue :inherit 'magit-branch-local))
   (magit-branch-remote (:foreground em-bright_green))
   (magit-cherry-equivalent (:foreground em-bright_purple))
   (magit-cherry-unmatched (:foreground em-bright_aqua))
   (magit-diff-added (:foreground em-neutral_green))
   (magit-diff-added-highlight (:foreground em-bright_green :inherit 'magit-diff-context-highlight))
   (magit-diff-base (:background em-faded_yellow :foreground em-light2))
   (magit-diff-base-highlight (:background em-faded_yellow :inherit 'magit-diff-context-highlight))
   (magit-diff-context (:foreground em-dark1 :foreground em-light1))
   (magit-diff-context-highlight (:background em-dark1 :foreground em-light0))
   (magit-diff-hunk-heading (:background em-dark3 :foreground em-light2))
   (magit-diff-hunk-heading-highlight (:background em-dark2 :foreground em-light0))
   (magit-diff-hunk-heading-selection (:background em-dark2 :foreground em-neutral_orange))
   (magit-diff-lines-heading (:background em-faded_orange :foreground em-light0))
   (magit-diff-removed (:foreground em-neutral_red))
   (magit-diff-removed-highlight (:foreground em-bright_red :inherit 'magit-diff-context-highlight))
   (magit-diffstats-added (:foreground em-faded_green))
   (magit-diffstats-removed (:foreground em-faded_red))
   (magit-dimmed (:foreground em-dark4))
   (magit-hash (:foreground em-neutral_blue))
   (magit-log-author (:foreground em-neutral_red))
   (magit-log-date (:foreground em-neutral_aqua))
   (magit-log-graph (:foreground em-dark4))
   (magit-process-ng (:foreground em-bright_red :weight 'bold))
   (magit-process-ok (:foreground em-bright_green :weight 'bold))
   (magit-reflog-amend (:foreground em-bright_purple))
   (magit-reflog-checkout (:foreground em-bright_blue))
   (magit-reflog-cherry-pick (:foreground em-bright_green))
   (magit-reflog-commit (:foreground em-bright_green))
   (magit-reflog-merge (:foreground em-bright_green))
   (magit-reflog-other (:foreground em-bright_aqua))
   (magit-reflog-rebase (:foreground em-bright_purple))
   (magit-reflog-remote (:foreground em-bright_blue))
   (magit-reflog-reset (:foreground em-bright_red))
   (magit-refname (:foreground em-light4))
   (magit-section-heading (:foreground em-bright_yellow :weight 'bold))
   (magit-section-heading-selection (:foreground em-faded_yellow))
   (magit-section-highlight (:foreground em-dark1))
   (magit-sequence-drop (:foreground em-faded_yellow))
   (magit-sequence-head (:foreground em-bright_aqua))
   (magit-sequence-part (:foreground em-bright_yellow))
   (magit-sequence-stop (:foreground em-bright_green))
   (magit-signature-bad (:foreground em-bright_red :weight 'bold))
   (magit-signature-error (:foreground em-bright_red))
   (magit-signature-expired (:foreground em-bright_orange))
   (magit-signature-good (:foreground em-bright_green))
   (magit-signature-revoked (:foreground em-bright_purple))
   (magit-signature-untrusted (:foreground em-bright_blue))
   (magit-tag (:foreground em-bright_yellow))

   ;; flyspell

   (flyspell-duplicate (:underline (:color em-light4 :style 'line)))
   (flyspell-incorrect (:underline (:color em-bright_red :style 'line)))

   ;; mu4e

   (mu4e-header-key-face (:foreground em-bright_green :weight 'bold))
   (mu4e-unread-face (:foreground em-bright_blue :weight 'bold))
   (mu4e-highlight-face (:foreground em-neutral_green))

   ;; shell script faces

   (sh-quoted-exec (:foreground em-bright_purple))
   (sh-hereoc (:foreground em-bright_orange))

   ;; undl-tree

   (undo-tree-visualizer-active-branch-face (:foreground em-light0))
   (undo-tree-visualizer-current-face (:foreground em-bright_red))
   (undo-tree-visualizer-default-face (:foreground em-dark4))
   (undo-tree-visualizer-register-face (:foreground em-bright_yellow))
   (undo-tree-visualizer-unmodified-face (:foreground em-bright_aqua))

   ;; widget faces
   (widget-button-pressed-face (:foreground em-bright_red))
   (widget-documentation-face (:foreground em-faded_green))
   (widget-field (:foreground em-light0 :background em-dark2))
   (widget-single-line-field (:foreground em-light0 :background em-dark2))

   ;; dired+

   (diredp-file-name (:foreground em-light2))
   (diredp-file-suffix (:foreground em-light4))
   (diredp-compressed-file-suffix (:foreground em-faded_blue))
   (diredp-dir-name (:foreground em-faded_blue))
   (diredp-dir-heading (:foreground em-bright_blue))
   (diredp-symlink (:foreground em-bright_orange))
   (diredp-date-time (:foreground em-light3))
   (diredp-number (:foreground em-faded_blue))
   (diredp-no-priv (:foreground em-dark4))
   (diredp-other-priv (:foreground em-dark2))
   (diredp-rare-priv (:foreground em-dark4))
   (diredp-ignored-file-name (:foreground em-dark4))
   (diredp-dir-priv (:foreground em-faded_blue :background em-faded_blue))
   (diredp-exec-priv (:foreground em-faded_blue :background em-faded_blue))
   (diredp-link-priv (:foreground em-faded_aqua :background em-faded_aqua))
   (diredp-read-priv (:foreground em-bright_red :background em-faded_red))
   (diredp-write-priv (:foreground em-bright_aqua :background em-faded_aqua)))
  nil)

(provide-theme 'emma)
