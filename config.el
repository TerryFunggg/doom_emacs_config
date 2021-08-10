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
(setq doom-font (font-spec :family "Source Code Pro" :size 18 :weight 'normal))
(set-language-environment "UTF-8")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; Dark theme :
(setq doom-theme 'doom-material)
;;(setq doom-theme 'doom-horizon)
;;(setq doom-theme 'doom-monokai-pro)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-nord)
;;
;; Light theme
;;(setq doom-theme 'doom-one-light)
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
(use-package emacs
  :preface
  (defvar ian/indent-width 2)
  :config
  (setq
    ring-bell-function 'ignore
    frame-resize-pixelwise t
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
  :ensure nil
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

(use-package! lsp-mode
  :config
  (setq lsp-idle-delay 0.500
        lsp-enable-file-watchers nil))

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
                              ("NEXT" . ?n)
                              ("REPEAT" . ?r)
                              ("INBOX" . ?i)))
        org-capture-templates
        '(("i" "Inbox" entry (file "~/Documents/org/inbox.org")
           "* TODO %? :INBOX:")
          ("q" "Quick Note" entry (file "~/Documents/org/Drafts.org")
           "* %?\n %T\n ")
          ("p" "Post" plain
                (file create-blog-post)
                (file "~/.doom.d/post.orgcaptmpl")))
        org-agenda-custom-commands
                '(("p" "Plan today"
                   ((agenda)
                    (tags-todo "PROJECT")))
                  ("j" . "Jobs")
                  ("ja" "Important & Emergency" tags-todo "+PRIORITY=\"A\"")
                  ("jb" "Important & Not Emergency" tags-todo "+PRIORITY=\"B\""))
        org-refile-targets '(("~/Documents/org/inbox.org" :level . 0)
                           ("~/Documents/org/next.org" :level . 0)
                           ("~/Documents/org/done.org" :level . 0))

        org-agenda-files (list (concat my/org-agenda-directory "next.org")
                               (concat my/org-agenda-directory "inbox.org")
                               (concat my/org-agenda-directory "repeaters.org")
                               (concat my/org-agenda-directory "project.org"))))

;;super-agende-mode
 (use-package! org-super-agenda
   :after org-agenda
   :init
   (setq org-super-agenda-groups
       '((:name "Today"
                :time-grid t)
         (:name "In Progress"
                :todo "NEXT")
         (:name "Important"
                :priority "A")
         (:name "To Refile"
                :tag "INBOX")
         (:name "Repeat Event"
                :tag "REPEAT")))

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

(use-package! openwith
  :after-call (pre-command-hook after-find-file dired-before-readin-hook)
  :config
  (openwith-mode 1))
(after! openwith
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "vlc"
               '(file))
         (list (openwith-make-extension-regexp
                '("png" "gif" "jpeg" "jpg"))
               "feh"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file))))
)
;; (setq url-proxy-services
;;        '(("http"     . "127.0.0.1:8001")
;;  	("https"     . "127.0.0.1:8001")))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(define-key evil-insert-state-map "\C-k" 'evil-delete-line)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\M-d" 'kill-word)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-o" 'evil-open-above)
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

(setq avy-timeout-seconds 0.2)

(map! :desc "upcase char"
      :m
      "g s u" 'upcase-char)

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(setq-default evil-escape-key-sequence "jf")
(setq-default evil-escape-delay 0.2)
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
        ;; ("g" "group")
        ;; ("ga" "Group A" plain (function org-roam-capture--get-point)
        ;;  "%?"
        ;;  :file-name "%<%Y%m%d%H%M%S>-${slug}"
        ;;  :head "#+title: ${title}\n#+roam_alias:\n\n")
        ;; ("gb" "Group B" plain (function org-roam-capture--get-point)
        ;;  "%?"
        ;;  :file-name "%<%Y%m%d%H%M%S>-${slug}"
        ;;  :head "#+title: ${title}\n#+roam_alias:\n\n")
        )))

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
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))



(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
(setq-local indent-line-function 'js-jsx-indent-line)
;;(add-hook 'rjsx-mode-hook 'prettier-js-mode)
;; (use-package! snails :load-path "lisp/snails")
(add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Hydra config
(defhydra hydra-web-mode (:color blue :quit-key "q" :hint nil)
"
Navigate^^^^                 Element^^                    Attribute^^     Other
[_j_/_k_] next/prev element  [_c_] clone                  [_@_] insert    [_p_] xpath (display path)
[_J_/_K_] next/prev sibling  [_d_] vanish (keep content)  [_!_] delete    [_q_] quit
[_h_/_l_] parent/child       [_D_] kill (inkl. content)   [_[_] next
[_a_/_e_] head/end element   [_r_] rename                 [_]_] previous
[_f_] fold/unfold^^          [_w_] wrap
^^^^                         [_i_] insert element
^^^^                         [_s_] select(tag,content)
"
  ("j" web-mode-element-next :exit nil)
  ("k" web-mode-element-previous :exit nil)
  ("J"  web-mode-element-sibling-next :exit nil)
  ("K"  web-mode-element-sibling-previous :exit nil)
  ("h"  web-mode-element-parent :exit nil)
  ("l"  web-mode-element-child :exit nil)
  ("a" web-mode-element-beginning :exit nil)
  ("e" web-mode-element-end :exit nil)
  ("c" web-mode-element-clone :exit nil)
  ("f" web-mode-element-children-fold-or-unfold :exit nil)
  ("i" web-mode-element-insert :exit nil)
  ("D" web-mode-element-kill :exit nil)
  ("d" web-mode-element-vanish :exit nil)
  ;;("m" web-mode-element-mute-blanks)
  ("w" web-mode-element-wrap :exit nil)
  ("r" web-mode-element-rename :exit nil)
  ("s" web-mode-element-select :exit nil)
  ;; attribute
  ("@" web-mode-attribute-insert :exit nil)
  ("!" web-mode-attribute-kill :exit nil)
  ("]" web-mode-attribute-next :exit nil)
  ("[" web-mode-attribute-previous :exit nil)
  ; other
  ("p" web-mode-dom-xpath :exit nil)
  ("q" nil "quit" :exit t))
(define-key web-mode-map (kbd "C-c .") 'hydra-web-mode/body)
(eval-after-load "web-mode"
  '(set-face-background 'web-mode-current-element-highlight-face "#98f5ff"))

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
(map! :leader
      :desc "Dumb jump"
      :n
      "j d" 'dumb-jump-hydra/body)

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

;; (use-package! ruby-mode
;;   :init
;;   (progn
;;     (add-hook 'before-save-hook #'lsp-format-buffer t)))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;; (eval-after-load 'js
;;   '(add-hook 'js-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;; (eval-after-load 'json-mode
;;   '(add-hook 'json-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;; (eval-after-load 'sgml-mode
;;   '(add-hook 'html-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

;; (eval-after-load 'web-mode
;;   '(add-hook 'web-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

;; (eval-after-load 'css-mode
;;   '(add-hook 'css-mode-hook
;;              (lambda ()
;;                (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))
(setq inferior-lisp-program "sbcl");
(add-to-list 'slime-contribs 'slime-repl)
(toggle-frame-fullscreen)
