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

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(scroll-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(global-hl-line-mode t)

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(global-display-line-numbers-mode t)

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "FantasqueSansM Nerd Font"))

(add-to-list 'default-frame-alist '(font . "FantasqueSansM Nerd Font-16"))

(use-package unicode-fonts
  :straight t
  :init
  (unicode-fonts-setup))

(dolist (char/ligature-re
         `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
           (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
           (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
           (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                               "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
                               "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
                               "</" "<*")
                           (+ "<"))))
           (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
           (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
           (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
           (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
           (?&  . ,(rx (+ "&")))
           (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
                               "|]" "|}" "|=")
                           (+ "|"))))
           (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
           (?+  . ,(rx (or "+>" (+ "+"))))
           (?\[ . ,(rx (or "[<" "[|")))
           (?\{ . ,(rx "{|"))
           (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                           (+ "#"))))
           (?\; . ,(rx (+ ";")))
           (?_  . ,(rx (or "_|_" "__")))
           (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  . ,(rx "$>"))
           (?^  . ,(rx "^="))
           (?\] . ,(rx "]#"))))
  (let ((char (car char/ligature-re))
        (ligature-re (cdr char/ligature-re)))
    (set-char-table-range composition-function-table char
                          `([,ligature-re 0 font-shape-gstring]))))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-material-dark t)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30))

(use-package vertico
  :straight t
  :custom
  (vertico-count 15)
  :config
  (vertico-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package which-key
  :straight t
  :config
  (which-key-mode t))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-default-cursor t)
  (setq evil-want-c-i-jump nil) ;; fixes indent in org mode
  (evil-mode t))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit calc))
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :init (evil-commentary-mode))

(use-package vimish-fold
  :straight t
  :after evil)

(use-package evil-vimish-fold
  :straight t
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(use-package evil-anzu
  :straight t
  :config
  (global-anzu-mode))

(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file)

(setq backup-directory-alist `((".*" . "~/emacs_backups")))

(setq ring-bell-function 'ignore)

(require 'dired)

(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'dired
  (setq dired-listing-switches "-Dhlv --group-directories-first"))

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode t))

(use-package peep-dired
  :straight t
  :config
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "M-p") 'peep-dired)
    (evil-define-key 'normal dired-mode-map (kbd "h")
      (lambda () (interactive) (find-alternate-file "..")))
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-alternate-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)))

(run-at-time (current-time) 300 'recentf-save-list)

(setq warning-minimum-level :emergency)

(set-language-environment 'utf-8)

(use-package org
  :defer
  :straight `(org
              :fork (:host nil
  			 :repo "https://git.tecosaur.net/tec/org-mode.git"
  			 :branch "dev"
  			 :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
  	      (require 'lisp-mnt)
  	      (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
  		      (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))

(use-package org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
  	   :zoom 1.2)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll
  		  scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

(require 'org)

(setq org-startup-folded t)
(setq org-hidden-keywords '(title))
(setq org-return-follows-link t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq-default org-enforce-todo-dependencies t)

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "indian red" :weight bold)
              ("NEXT" :foreground "light blue" :weight bold)
              ("DONE" :foreground "light green" :weight bold)
              ("WAITING" :foreground "chocolate" :weight bold)
              ("CANCELLED" :foreground "dim gray" :weight bold))))

(setq-default org-export-with-todo-keywords nil)

(with-eval-after-load 'org-superstar
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))

  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-headline-bullets-list
        '("◉" "◈" "○" "▷"))
  (org-superstar-restart))


(setq org-hide-leading-stars nil)
(setq org-indent-mode-turns-on-hiding-stars nil)

(setq org-ellipsis " ▼ ")

(setq org-hide-emphasis-markers t)

(defun my/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "FantasqueSansM Nerd Font"
                                        :height 160
                                        :width normal))
  (buffer-face-mode))

(defun my/set-faces-org ()
  (setq org-hidden-keywords '(title))
  (set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)

  (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)

  (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.2) ;\large
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.44) ;\Large
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.728) ;\LARGE

  (setq org-cycle-level-faces nil)
  (setq org-n-level-faces 4)

  (set-face-attribute 'org-document-title nil
                      :height 2.074
                      :foreground 'unspecified
                      :inherit 'org-level-8))

(defun my/set-keyword-faces-org ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(;; Syntax
          ("TODO" .     "")
          ("DONE" .     "")
          ("WAITING" .  "")
          ("HOLD" .     "")
          ("NEXT" .     "")
          ("CANCELLED" . "")
          ("#+begin_quote" . "“")
          ("#+end_quote" . "”")))
  )

(defun my/style-org ()
  (my/set-faces-org)
  (my/set-keyword-faces-org))

(add-hook 'org-mode-hook 'my/style-org)
(add-hook 'org-mode-hook 'org-indent-mode)

;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;; prevent org capture from saving bookmarks
(setq org-bookmark-names-plist nil)

(use-package org-auto-tangle
  :straight t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package evil-org
  :straight t
  :after org
  :config
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :straight t
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start nil))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(defadvice load-theme (after style-org activate)
  (my/style-org))

(defadvice next-buffer (after avoid-anoying-buffers activate)
  (when (or (string-match-p "^\*" (buffer-name))
  	  (string-match-p "^magit" (buffer-name)))
    (next-buffer)))

(defadvice previous-buffer (after avoid-anoying-buffers activate)
  (when (or (string-match-p "^\*" (buffer-name))
  	  (string-match-p "^magit" (buffer-name)))
    (previous-buffer)))

(use-package magit
  :straight t)

(use-package gptel
  :straight t
  :config
  (setq
   gptel-model 'llama3:8b
   gptel-backend (gptel-make-ollama "ollama"
  		 :host "localhost:11434"
  		 :stream t
  		 :models '(llama3:8b dolphin-mistral:7b)))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll))

(use-package embark
  :straight t
  :bind*
  (("C-;" . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package affe
  :straight t)

(use-package avy
  :bind*
  (("C-j" . avy-goto-char-timer))
  :straight t)

(electric-pair-mode t)
(electric-indent-mode t)

(global-auto-revert-mode)

(setq indent-tabs-mode nil)

(use-package eglot
  :straight t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-popupinfo-mode t)
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("RET" . nil)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  :straight t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package nerd-icons-corfu
  :straight t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(defun my/yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))

(add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(setq save-abbrevs 'silently)
;; (setq-default abbrev-mode t)

(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t)

(use-package treemacs-nerd-icons
  :straight t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treesit
  :custom
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (nix "https://github.com/nix-community/tree-sitter-nix")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (treesit-font-lock-level 4))

(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  :custom
  (eglot-workspace-configuration
   '(:rust-analyzer
     ( :procMacro ( :attributes (:enable t)
                    :enable t)
       :cargo (:buildScripts (:enable t))
       :diagnostics (:disabled ["unresolved-proc-macro"
                                "unresolved-macro-call"]))))
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . eglot-ensure)
       (rust-ts-mode . eglot-ensure)))

(use-package nix-mode
  :straight t
  :mode
  ("\\.nix\\'" . nix-mode))

(use-package nix-ts-mode
  :straight t)

(add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd")))
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

(use-package eglot
  :after nix-ts-mode
  :hook (nix-ts-mode . eglot-ensure))

(use-package web-mode
  :straight t
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.css\\'" . web-mode)))

(use-package emmet-mode
  :straight t
  :hook
  ((web-mode . emmet-mode)
   (tsx-mode . emmet-mode)))

(use-package auctex
  :straight t  
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  :config
  (add-to-list 'TeX-command-list '("LaTeXMkClean" "latexmk -c %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil
  				 (LaTeX-mode)
  				 :help "latexmk clean"))
  (add-to-list 'TeX-command-list '("LaTeXMkCompile" "latexmk -pdf %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil
  				 (LaTeX-mode)
  				 :help "latexmk compile"))
  :mode
  ("\\.tex\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode . prettify-symbols-mode))

(use-package cdlatex
  :straight t)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(use-package eglot
  :hook (python-ts-mode . eglot-ensure))

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(use-package eglot
  :hook (c-ts-mode . eglot-ensure))

(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(use-package eglot
  :hook (c++-ts-mode . eglot-ensure))

(use-package general
  :straight t
  :config
  (general-evil-setup t))

(nvmap :states '(normal visual motion emacs) :keymaps 'override :prefix "SPC"
  "a" '(:which-key "apps")
  "a d" '(dashboard-open :which-key "dashboard")
  "a f" '(affe-find :which-key "affe find")
  "a F" '(affe-grep :which-key "affe grep")
  "a g" '(magit-status-here :which-key "magit")
  "a i" '(ibuffer :which-key "ibuffer")
  "a t" '(treemacs :which-key "open treemacs")
  "a r" '(org-roam-ui-open :which-key "org roam ui")
  "a s" '(gptel-send :which-key "gptel send")
  "a S" '(gptel :which-key "gptel")

  "b" '(:which-key "buffer")
  "b d" '(kill-current-buffer :which-key "kill buffer")
  "b i" '(ibuffer :which-key "ibuffer")
  "b n" '(next-buffer :which-key "next buffer")
  "b p" '(previous-buffer :which-key "previous buffer")

  "d" '(:which-key "dired")
  "d d" '(dired-jump :which-key "open dired")
  "d p" '(peep-dired :which-key "peep dired")

  "e" '(:which-key "eval")
  "e b" '(eval-buffer :which-key "eval buffer")
  "e e" '(eval-expression :which-key "eval expression")
  "e l" '(eval-last-sexp :which-key "eval last expression")
  "e r" '(eval-region :which-key "eval region")

  "f" '(:which-key "file")
  "f b" '(:which-key "bookmark")
  "f b b" '(bookmark-jump :which-key "jump to bookmark")
  "f b d" '(bookmark-delete :which-key "delete bookmark")
  "f b n" '(bookmark-set :which-key "new bookmark")
  "f f" '(find-file :which-key "find file")
  "f s" '(save-buffer :which-key "save file")
  "f S" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "source init.el")

  "h" '(:which-key "help")
  "h f" '(describe-function :which-key "describe function")
  "h k" '(describe-key :which-key "describe key")
  "h m" '(describe-mode :which-key "describe mode")
  "h M" '(man :which-key "gnu manual")
  "h t" '(load-theme :which-key "load theme")
  "h v" '(describe-variable :which-key "describe variable")

  "i" '(:which-key "insert")
  "i a" '(add-global-abbrev :which-key "write new abbrev")
  "i f" '(yas-visit-snippet-file :which-key "visit snippet")
  "i n" '(yas-new-snippet :which-key "new snippet")
  "i s" '(yas-insert-snippet :which-key "insert snippet")

  "l" '(:which-key "LaTeX")
  "l c" '((lambda () (interactive) (TeX-command "LaTeXMkCompile" 'TeX-master-file -1)) :which-key "LaTeX compile")
  "l C" '((lambda () (interactive) (TeX-command "LaTeXMkClean" 'TeX-master-file -1)) :which-key "LaTeX clean")
  "l e" '(cdlatex-environment :which-key "LaTeX environment")

  "o" '(:which-key "org")
  "o c" '(org-latex-preview-clear-cache :which-key "clear LaTeX fragments")
  "o e" '(org-export-dispatch :which-key "org export dispatch")
  "o E" '(org-edit-special :which-key "org edit special")
  "o i" '(org-link-preview :which-key "toggle inline images")
  "o p" '(org-latex-preview :which-key "preview LaTeX fragments")
  "o r" '(:which-key "org roam")
  "o R" '(org-mode-restart :which-key "restart org")
  "o r f" '(org-roam-node-find :which-key "org roam node find")
  "o r i" '(org-roam-node-insert :which-key "org roam node insert")
  "o r s" '(org-roam-db-sync :which-key "org roam sync db")

  "q" '(:which-key "quit")
  "q f" '(delete-frame :which-key "quit emacsclient")
  "q q" '(save-buffers-kill-terminal :which-key "quit emacs")

  "w" '(:which-key "window")
  "w c" '(delete-window :which-key "close window")
  "w s" '(split-window-below :which-key "split window horizontally")
  "w v" '(split-window-right :which-key "split window vertically")
  "w w" '(other-window :which-key "switch window"))

(nvmap :states '(normal) :keymaps 'override
  "z a" '(org-cycle :which-key "org toggle fold"))

(use-package dashboard
  :straight t
  :custom
  (dashboard-banner-logo-title "Welcome to Emacs")
  (dashboard-startup-banner 'logo)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-show-shortcuts t)
  (dashboard-center-content t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (dashboard-setup-startup-hook)
  :init
  (dashboard-open))
