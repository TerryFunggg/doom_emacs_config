;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")

(setq user-full-name "Terry Fung"
      user-mail-address "terryyessfung@gmail.com")

;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)


(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

(define-key evil-insert-state-map "\C-k" 'evil-delete-line)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\M-d" 'kill-word)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-o" 'evil-open-above)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)

(map! :leader
      :desc "lsp jump back"
      :n
      "j b" #'lsp-ui-peek-jump-backward)

(map! :leader
      :desc "Go to char(s)"
      :n
      "j c" 'evil-avy-goto-char-timer)

(setq avy-timeout-seconds 0.2)

(map! :desc "upcase char"
      :m
      "g s u" 'upcase-char)

(setq-default evil-escape-key-sequence "jf")
(setq-default evil-escape-delay 0.2)


;; Org config
(global-set-key (kbd "C-c a") #'org-agenda)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/gtd/"))
(setq org-agenda-diary-file "~/org/diary")
(setq diary-file "~/org/diary")
(setq org-capture-templates nil)
(setq org-agenda-custom-commands nil)
(setq org-tag-alist
       '(("@work" . ?w)
         ("@home" . ?h)
         ("@project" . ?p)
         ("@meeting" . ?e)
         ("@me" . ?m)
         ("@book" . ?b)
         ("@website" . ?i)
         ("@cs" . ?c)
         ("@ee" . ?s)
         ("@video" . ?v)))


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; Capture
(after! org (add-to-list
 'org-capture-templates
 '("t" "Todo" entry
   (file"~/org/gtd/inbox.org")
   "* TODO %i%?^G \n%T")))

(add-to-list
 'org-capture-templates
 '("n" "Quick Note" entry
   (file "~/org/notes/quick_notes.org")
   "* %^{heading} %t %^g\n  %?\n"))


;; Query
(add-to-list 'org-agenda-custom-commands '("Q"."Custom Queries"))
(add-to-list 'org-agenda-custom-commands
             '("Qn" "Search Quick Notes" tags ""
               ((org-agenda-files (file-expand-wildcards "~/org/notes/quick_notes.org"))
                (org-agenda-prefix-format " "))))

(add-to-list 'org-agenda-custom-commands
             '("Qb" "Search Bookmarks" tags ""
               ((org-agenda-files (file-expand-wildcards "~/org/gtd/read.org"))
                (org-agenda-prefix-format " "))))

(add-to-list 'org-agenda-custom-commands
              '("g" "Get Things Done (GTD)"
                (
                 (tags "inbox+TODO=\"TODO\"+PRIORITY=\"A\"|project+TODO=\"TODO\"+PRIORITY=\"A\"|read+TODO=\"TODO\"+PRIORITY=\"A\""
                            (
                             (org-agenda-prefix-format "  %?-12t% s [%e] ")
                             (org-agenda-overriding-header "\nGet thing Done\n")))
                 (tags "inbox+TODO=\"TODO\"-PRIORITY=\"A\""
                            (
                             (org-agenda-prefix-format "  %?-12t% s ")
                             (org-agenda-overriding-header "\nInbox\n")))
                 (tags "project+TODO=\"TODO\"-PRIORITY=\"A\""
                            ((org-agenda-prefix-format "  %?-12t% s [%e] ")
                             (org-agenda-overriding-header "\nProjects\n")))
                 (tags "read-PRIORITY=\"A\""
                            ((org-agenda-prefix-format "  %?-12t% s")
                             (org-agenda-overriding-header "\nTo Read\n")))
                 ;; (tags "+DEADLINE>=\"<today>\""
                 ;; (tags "CLOSED>=\"<today>\""
                 ;;       ((org-agenda-overriding-header "\nCompleted today\n")))
                 (todo "HOLD"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %?-12t% s ")
                        (org-agenda-overriding-header "\nHold\n"))))
                nil
                ("~/org/export/agenda.html")))

(add-hook 'org-agenda-mode-hook 'delete-other-windows)


(defun my/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda"
  (interactive)
  (org-with-wide-buffer
   (org-agenda-todo)
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))
(define-key! org-agenda-mode-map "r" 'my/org-agenda-process-inbox-item)
