;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Terry Fung"
      user-mail-address "terryyessfung@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono" :size 18 :weight 'normal))
(set-language-environment "UTF-8")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; Dark theme :
;;(setq doom-theme 'doom-xcode)
;;(setq doom-theme 'doom-ayu-light)
;;(setq doom-theme 'doom-material)
;;(setq doom-theme 'doom-horizon)
;;(setq doom-theme 'doom-monokai-pro)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-nord)
;;
;; Light theme
;;(setq doom-theme 'doom-one-light)
;;(setq doom-theme 'leuven)
(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-flatwhite)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(use-package emacs
  :preface
  (defvar ian/indent-width 2)
  :config
  (setq
    ring-bell-function 'ignore
    frame-resize-pixelwise t
    require-final-newline t
    default-directory "~/")

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (display-time)

  ;; increase line space for better readability
  (setq-default line-spacing 3))

;; dashboard image
;;(setq fancy-splash-image "~/.doom.d/logo.png")

(use-package recentf
  :defer t
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "06:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))


(use-package delsel
  :disabled
  :ensure nil
  :config (delete-selection-mode +1))
(setq delete-selection-mode t)
;; Set history-length longer
(setq-default history-length 500)

(use-package dired
  :defer t
  ;; :ensure nil
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil))

(map! :leader
    :desc "dired jump other window"
    :n
    "f o" #'dired-jump-other-window)

;; coompany config
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

(use-package! lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil)
  :commands
  lsp-ui-mode)

(add-hook! 'org-mode-hook (company-mode -1))
(add-hook! 'org-capture-mode-hook (company-mode -1))

(setq
 js-indent-level 2
 typescript-indent-level 2
 projectile-project-search-path '("~/codebox")
 )


;; Seoarate custom file
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))


(global-set-key "\C-s" 'swiper)

(defun open-my-inbox()
  (interactive)
  (find-file "~/Documents/org/inbox.org"))
(global-set-key (kbd "<f12>") 'open-my-inbox)

;; Org config
(setq my/org-agenda-directory "~/Documents/org/")
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-directory "~/Documents/org"
        org-hide-emphasis-markers t
        org-agenda-span 'day
        org-agenda-start-day "+0d"
        org-todo-keywords '((sequencep "TODO(t)" "NEXT(n)" "|" "DONE(d)" "HOLD(h)"))
        org-tag-alist (quote (("@home" . ?h)
                              ("@office" . ?o)
                              ("book" . ?b)
                              ("websit" . ?w)
                              ("video" . ?v)
                              ("repeat"  . ?r)
                              ("project" . ?p)))
        org-capture-templates
        '(("i" "Inbox" entry (file "~/Documents/org/inbox.org")
           "* TODO %?")
          ("q" "Quick Note" entry (file "~/Documents/org/drafts.org")
           "* %?\n %T\n ")
          ("p" "Post" plain
                (file create-blog-post)
                (file "~/.doom.d/post.orgcaptmpl")))
        org-agenda-custom-commands
        '(("p" "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)))
            (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files (list
                                   (concat my/org-agenda-directory "projects.org")
                                   (concat my/org-agenda-directory "next.org")))))
            (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files (list (concat my/org-agenda-directory "projects.org")))))
            (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files (list (concat my/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
            (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files (list (concat my/org-agenda-directory "inbox.org")))))
            nil)))
        ;; org-agenda-custom-commands
        ;; '(("A" "Agenda" my/org-agenda-view)
        ;;         ("p" "Plan today"
        ;;            ((agenda)
        ;;           ("j" . "Jobs")
        ;;             (tags-todo "PROJECT")))
        ;;           ("ja" "Important & Emergency" tags-todo "+PRIORITY=\"A\"")
        ;;           ("jb" "Important & Not Emergency" tags-todo "+PRIORITY=\"B\""))
        org-refile-targets '(("~/Documents/org/inbox.org" :level . 0)
                           ("~/Documents/org/next.org" :level . 0)
                           ("~/Documents/org/done.org" :level . 0))

        org-agenda-files (list (concat my/org-agenda-directory "next.org")
                               (concat my/org-agenda-directory "inbox.org")
                               (concat my/org-agenda-directory "repeaters.org")
                               (concat my/org-agenda-directory "project.org"))))
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

(defun my/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda"
  (interactive)
  (org-with-wide-buffer
   (org-agenda-todo)
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))

(define-key! org-agenda-mode-map "r" 'my/org-agenda-process-inbox-item)

(use-package! deft
  :init
  (setq deft-extensions '("txt" "org" "md")
        deft-directory "~/Documents/org/org_notes/"
        deft-recursive t))


(define-key evil-insert-state-map "\C-k" 'evil-delete-line)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\M-d" 'kill-word)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-o" 'evil-open-above)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)


(map! :leader
      :desc "lsp jump back"
      :n
      "j b" #'lsp-ui-peek-jump-backward)

(map! :leader
      :desc "Go to line"
      :n
      "j l" 'avy-goto-line)

(map! :leader
      :desc "Go to char(s)"
      :n
      "j c" 'evil-avy-goto-char-timer)

(setq avy-timeout-seconds 0.2)

(map! :desc "upcase char"
      :m
      "g s u" 'upcase-char)

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(setq-default evil-escape-key-sequence "jf")
(setq-default evil-escape-delay 0.2)

;; Web mode config
(use-package! web-mode
  ;;:hook (web-mode . prettier-js-mode)
  :config
  (setq web-mode-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))

;; Thanks magars
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key "\C-c\C-e" 'eval-and-replace)

(defun my/today ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun my/semicolon ()
  (interactive)
  (end-of-line)
  (insert ";"))
(global-set-key "\C-c\;" 'my/semicolon)

(defun create-blog-post ()
        "Create blog post"
        (interactive)
        (let ((name (read-string "Post title: ")))
          (expand-file-name
           (concat
            (org-read-date) "-" (format "%s.md" name))
           "~/Documents/myjekyllblog/_posts/")))

;; For emacs server
(server-start) 
