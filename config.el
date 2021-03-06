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
(setq doom-font (font-spec :family "Source Code Pro" :size 16 :weight 'normal))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; Dark theme :
(setq doom-theme 'doom-horizon)
;;(setq doom-theme 'doom-monokai-pro)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-nord)
;;
;;For org/writing mode
;;(setq doom-theme 'leuven)
;;(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-flatwhite)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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
(display-time)
(add-hook! 'org-mode-hook (company-mode -1))
(add-hook! 'org-capture-mode-hook (company-mode -1))

(setq
 js-indent-level 2
 typescript-indent-level 2
 projectile-project-search-path '("~/CodeBox")
 )

;; full-screen when open the emcas
;;(if (eq initial-window-system 'x)
;;    desktop file
;;    (toggle-frame-maximized)
;;    (toggle-frame-fullscreen))

;; Seoarate custom file
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(global-set-key "\C-s" 'swiper)

(defun open-my-inbox()
  (interactive)
  (find-file "~/Documents/org/Inbox.org"))
(global-set-key (kbd "<f12>") 'open-my-inbox)

(use-package! evil-multiedit
  :init
  :config
  (evil-multiedit-default-keybinds))

;; Org config
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-directory "~/Documents/org"
        org-hide-emphasis-markers t
        org-agenda-span 'day
        org-agenda-start-day "+0d"
        org-todo-keywords '((sequencep "TODO(t)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)"))
        org-capture-templates
        '(("i" "Todo Inbox" entry (file "~/Documents/org/Inbox.org")
           "* %?\n")
          ("q" "Quick Note" entry (file "~/Documents/org/Drafts.org")
           "* %?\n %T\n ")
          ("p" "Post" plain
                (file create-blog-post)
                (file "~/.doom.d/post.orgcaptmpl")))
        org-agenda-custom-commands
                '(("p" "Plan today"
                   ((agenda)
                    (tags-todo "PROJECT"))))
        org-refile-targets '(("~/Documents/org/TODO.org" :level . 1)
                           ("~/Documents/org/Inbox.org" :level . 1)
                           ("~/Documents/org/Done.org" :level . 1))
       
        org-agenda-files (list "~/Documents/org/TODO.org"
                               "~/Documents/org/Habit.org"
                               "~/Documents/org/Project.org")))

;; super-agende-mode
 (use-package! org-super-agenda
   :after org-agenda
   :init
   (setq org-super-agenda-groups
       '((:name "Today"
                :time-grid t
                :order 1
                :todo "TODAY")
         (:name "Important"
                :order 2
                :priority "A")
         (:name "Project"
                :order 3
                :tag "PROJECT")
         (:order-multi (4 (:name "To-Read"
                                 :tag ("EBOOK" "BOOK" "WEBPAGE"))
                          (:name "To-Learn"
                                 :tag "LEARN")
                          (:name "Habit"
                                 :tag "HABIT")
                          (:name "Personal"
                                 :tag "@ME")))
         (:todo "HOLD" :order 5)
         (:name "Overdue" :deadline past :order 6)))

       :config
       (org-super-agenda-mode))

;; Org-brain
;; (use-package! org-brain
;;   :init
;;   (setq org-brain-path "~/Documents/org/org_notes/org_brain/")
;; ;;  (with-eval-after-load 'evil (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (map! :ne "SPC n b" #'org-brain-visualize)
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/Documents/org/.org-id-locations")
;;   (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;;   (setq org-brain-visualize-default-choices 'all)
;;   )

(use-package! deft
  :init
  (setq deft-extensions '("txt" "org" "md")
        deft-directory "~/Documents/org/org_notes/"
        deft-recursive t))

(setq url-proxy-services
      '(("http"     . "127.0.0.1:10080")
	("https"     . "127.0.0.1:10080")))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(define-key evil-insert-state-map "\C-k" 'evil-delete-line)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\M-d" 'kill-word)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-o" 'split-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)

;; (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/.local/etc/plantuml.jar"))
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

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

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; org roam config
(use-package! org-roam
  :init
  (setq org-roam-directory "~/Documents/org/roam")

  :config
  (setq org-roam-capture-templates
      '(
        ("d" "default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n\n")
        ("r" "Read Note" plain (function org-roam-capture--get-point)
         "* What\n\n* Why"
         :file-name "Read-Note-${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n\n")
        ("a" "Annotation" plain (function org-roam-capture--get-point)
               "%U ${body}\n"
               :file-name "${slug}"
               :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
               :immediate-finish t
               :unnarrowed t)
        ("g" "group")
        ("ga" "Group A" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n\n")
        ("gb" "Group B" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n\n"))))

(add-to-list 'org-roam-capture-ref-templates
             '("a" "Annotation" plain (function org-roam-capture--get-point)
               "%U ${body}\n"
               :file-name "${slug}"
               :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
               :immediate-finish t
               :unnarrowed t))

(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 9090
      org-roam-server-export-inline-images t
      org-roam-server-authenticate nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20))
;;(org-roam-server-mode)
(require 'org-roam-protocol)

;; Web mode config
(use-package! web-mode
  ;;:hook (web-mode . prettier-js-mode)
  :config
  (setq web-mode-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(defun create-blog-post ()
        "Create blog post"
        (interactive)
        (let ((name (read-string "Post title: ")))
          (expand-file-name
           (concat
            (format-time-string "%Y-%m-%d") "-" (format "%s.md" name))
           "~/Documents/myjekyllblog/_posts/")))


(add-hook 'before-save-hook 'gofmt-before-save)
