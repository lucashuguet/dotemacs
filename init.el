(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-expand-minimally t)

(setq inhibit-startup-message t)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq backup-directory-alist `((".*" . "~/emacs_backups")))

(run-at-time (current-time) 300 'recentf-save-list)

(add-to-list 'default-frame-alist '(font . "FantasqueSansM Nerd Font-16"))

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

(tool-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode t)
(electric-indent-mode t)

(scroll-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(global-hl-line-mode t)
(line-number-mode t)
(global-display-line-numbers-mode t)
(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil))
(global-auto-revert-mode)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-default-cursor t)
  (evil-mode t))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit))
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :after evil
  :init (evil-commentary-mode))

(use-package vimish-fold
  :ensure t
  :after evil)

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(use-package evil-anzu
  :ensure t
  :config
  (global-anzu-mode))

(use-package general
  :ensure t
  :config
  (general-evil-setup t))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "FantasqueSansM Nerd Font"))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-banner-logo-png "~/.emacs.d/logo.png")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-center-content t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook))

(require 'dired)

(with-eval-after-load 'dired
  (setq dired-listing-switches "-Dhlv --group-directories-first"))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode t))

(use-package peep-dired
  :ensure t
  :config
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "M-p") 'peep-dired)
    (evil-define-key 'normal dired-mode-map (kbd "h")
      (lambda () (interactive) (find-alternate-file "..")))
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-alternate-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)))

(use-package lsp-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package magit
  :ensure t)

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

(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-agenda-set-keys))

(use-package toc-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package treemacs
  :ensure t)

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package unicode-fonts
  :ensure t
  :init
  (unicode-fonts-setup))

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (setq vertico-count 15))

(use-package which-key
  :ensure t
  :config
  (which-key-mode t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package ellama
  :ensure t
  :after llm
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "deepseek-coder-v2:16b-lite-base-q4_0" :embedding-model "deepseek-coder-v2:16b-lite-instruct-q4_0")))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-ayu-dark t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :init
  (setq treesit-font-lock-level 3)
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode))))

(use-package dockerfile-mode
  :ensure t)

(use-package company :ensure t)

(use-package web-mode :ensure t)

(use-package flycheck :ensure t)

(use-package tide :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)

(nvmap :states '(normal visual motion emacs) :keymaps 'override :prefix "SPC"
  "f" '(:which-key "file")
  "f f" '(find-file :which-key "find file")
  "f s" '(save-buffer :which-key "save file")
  "f S" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "source init.el")
  "f b" '(:which-key "bookmark")
  "f b b" '(bookmark-jump :which-key "jump to bookmark")
  "f b s" '(bookmark-set :which-key "set bookmark")
  "f b d" '(bookmark-delete :which-key "delete bookmark")

  "w" '(:which-key "window")
  "w s" '(split-window-below :which-key "split window horizontally")
  "w v" '(split-window-right :which-key "split window vertically")
  "w c" '(delete-window :which-key "close window")
  "w w" '(next-window-any-frame :which-key "switch window")

  "b" '(:which-key "buffer")
  "b d" '(kill-current-buffer :which-key "kill buffer")
  "b n" '(next-buffer :which-key "next buffer")
  "b p" '(previous-buffer :which-key "previous buffer")
  "b i" '(ibuffer :which-key "ibuffer")

  "q" '(:which-key "quit")
  "q q" '(save-buffers-kill-terminal :which-key "quit emacs")
  "q f" '(delete-frame :which-key "quit emacsclient")

  "d" '(:which-key "dired")
  "d d" '(dired :which-key "open dired")
  "d j" '(dired-jump :which-key "dired jump")
  "d p" '(peep-dired :which-key "peep dired")

  "a" '(:which-key "apps")
  "a t" '(treemacs :which-key "open treemacs")
  "a g" '(magit-status-here :which-key "magit")
  "a i" '(ibuffer :which-key "ibuffer")

  "h" '(:which-key "help")
  "h t" '(load-theme :which-key "load theme")
  "h v" '(describe-variable :which-key "describe variable")
  "h f" '(describe-function :which-key "describe function")
  "h k" '(describe-key :which-key "describe key")
  "h m" '(modus-themes-toggle :which-key "toggle modus theme")

  "r" '(:which-key "region")
  "r c" '(to-cyrillic :which-key "translate region to cyrillic")

  "o" '(:which-key "org")
  "o p" '(org-latex-preview :which-key "preview latex fragments")
  "o R" '(org-mode-restart :which-key "restart org")
  "o e" '(org-export-dispatch :which-key "org export dispatch")
  "o r" '(:which-key "org roam")
  "o r f" '(org-roam-node-find :which-key "node find")
  "o r i" '(org-roam-node-insert :which-key "node insert")
  "o r g" '(org-roam-graph :which-key "nodes graph")

  "i" '(:which-key "insert")
  "i s" '(yas/insert-snippet :which-key "insert snippet")
  "i n" '(yas/new-snippet :which-key "new snippet")

  "e" '(:which-key "eval")
  "e b" '(eval-buffer :which-key "eval buffer")
  "e r" '(eval-region :which-key "eval region")
  "e e" '(eval-expression :which-key "eval expression")
  "e l" '(eval-last-sexp :which-key "eval last expression"))

(nvmap :states '(normal) :keymaps 'override
  "z a" '(org-cycle :which-key "org toggle fold"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
