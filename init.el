   ;;; -*- lexical-binding: t; -*-

 (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
 (bootstrap-version 3))
 (unless (file-exists-p bootstrap-file)
 (with-current-buffer
 (url-retrieve-synchronously
 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
 'silent 'inhibit-cookies)
 (goto-char (point-max))
 (eval-print-last-sexp)))
 (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(use-package git) ;; ensure we can install from git sources

;; Get current system's name
(defun insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )

;; Get current system type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )
(message "Completed OS Level variables load")

;; Get current system's name
(defun insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )

;; Get current system type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )

(setq epa-file-encrypt-to "shreyas@fastmail.com")

(require 'auth-source)
(setq auth-sources
      '((:source "~/.authinfo.gpg"
		 "~/.bitly-access.token.gpg")))

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package magit
:demand t
:config
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-revert-buffers 'silent)
(setq magit-process-find-password-functions '(magit-process-password-auth-source)))

(use-package git-timemachine
  :ensure t)

;;______________________________________________________________________
;;;;  Installing Org with straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; (straight-use-package 'org) ; or org-plus-contrib if desired

(use-package org-plus-contrib
   :mode (("\\.org$" . org-mode))
   :bind
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda))

(use-package org-bullets
:after (org)
:config
(add-hook 'org-mode-hook 'org-bullets-mode))

   (with-eval-after-load 'org
   (add-hook 'org-mode-hook #'org-indent-mode))

(use-package ox-pandoc
  :ensure t
  :defer nil)

(setq org-agenda-start-on-weekday 1)

(setq org-agenda-tags-column -150)

(setq
 org-directory "~/my_org/"
 org-agenda-files '("~/my_org/")
 )

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "recurr"
		((org-agenda-overriding-header "Recurring Tasks")))
          (agenda "")
          (todo "")))
        ("o" agenda "Office mode" ((org-agenda-tag-filter-preset '("-course" "-habit" "-someday" "-book" "-emacs"))))
        ("qc" tags "+commandment")
	("e" tags "+org")
	("w" agenda "Today" ((org-agenda-tag-filter-preset '("+work"))))
	("W" todo-tree "WAITING")
	("q" . "Custom queries") ;; gives label to "q"
	("d" . "ds related")	 ;; gives label to "d"
	("ds" agenda "Datascience" ((org-agenda-tag-filter-preset '("+datascience"))))
	("qw" agenda "MRPS" ((org-agenda-tag-filter-preset '("+canjs"))))
	("qa" "Archive tags search" org-tags-view ""
         ((org-agenda-files (file-expand-wildcards "~/my_org/*.org*"))))
        ("j" "Journal Search" search ""
         ''((org-agenda-text-search-extra-files (file-expand-wildcards "~/my_org/journal/"))))
        ("S" search ""
	 ((org-agenda-files '("~/my_org/"))
	  (org-agenda-text-search-extra-files )))
	)
      )

(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq org-agenda-search-view-always-boolean t)

(require 'org-habit)
(setq org-habit-graph-column 90)

(setq org-datetree-add-timestamp nil)

(setq org-capture-templates
      '(("t" "Task entry")
        ("tt" "Todo - Fast" entry (file+headline "~/my_org/todo-global.org" "@Inbox")
	 "** TODO %?")
        ("tj" "Todo -Job journal" entry (file+olp+datetree "~/my_org/ds-jobs.org" "Job Search Journal")
	 "** TODO %?")
        ("te" "Todo - Emacs" entry (file+headline "~/my_org/todo-global.org" "@Emacs notes and tasks")
         "** TODO %?")
        ("td" "Datascience inbox" entry (file+headline "~/my_org/datascience.org" "@Datascience @Inbox")
         "** TODO %?")
	("tm" "Mail Link Todo" entry (file+headline "~/my_org/todo-global.org" "@Inbox")
	 "** TODO Mail: %a ")
        ("l" "Link/Snippet" entry (file+headline "~/my_org/link_database.org" ".UL Unfiled Links")
         "** %? %a ")
        ("e" "Protocol info" entry ;; 'w' for 'org-protocol'
         (file+headline "~/my_org/link_database.org" ".UL Unfiled Links")
         "*** %a, \n %:initial")
        ("n" "Notes")
        ("ne" "Emacs note" entry (file+headline "~/my_org/todo-global.org" "@Emacs notes and tasks")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("nn" "General note" entry (file+headline "~/my_org/notes.org" "@NOTES")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("nd" "Datascience note" entry (file+headline "~/my_org/datascience.org" "@Datascience @Notes")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("g" "BGR stuff")
        ("gi" "Inventory project")
        ("gil" "Daily log" entry (file+olp+datetree "~/my_org/bgr.org" "Inventory management Project") "** %? %i")
        ("C" "Commandment" entry (file+datetree "~/my_org/lifebook.org" "")
         "** %? %i :commandment:")
        ("J" "Job search" entry (file+headline "~/my_org/mrps_canjs.org" "MRPS #CANJS")
         "** TODO %? %i ")
        ("w" "Website" plain
         (function org-website-clipper)
         "* %a %T\n" :immediate-finish t)
        ("j" "Journal entry" entry (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format) %?")
        ("i" "Whole article capture" entry
         (file+headline "~/my_org/full_article_archive.org" "" :empty-lines 1)
         "** %a, %T\n %:initial" :empty-lines 1)
        ("c" "Clocking capture")
        ("ct" "Clock TODO" entry (clock) "** TODO %?")
        ("cn" "Clock Note" entry (clock) "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("r" "Review note" entry (file+weektree "~/my_org/lifebook.org" "#Personal #Reviews")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
         ))

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))))

(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead in the mac
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install)
)

(use-package org-noter
  :ensure t
  :defer t
  :config
  (setq org-noter-set-auto-save-last-location t)
  )

(use-package winum
  :defer nil
  :init
  ;; ;;(define-key map (kbd "C-`") 'winum-select-window-by-number)
  ;; (define-key winum-keymap (kbd "C-0") 'winum-select-window-0-or-10)
  ;; (define-key winum-keymap (kbd "C-1") 'winum-select-window-1)
  ;; (define-key winum-keymap (kbd "C-2") 'winum-select-window-2)
  ;; (define-key winum-keymap (kbd "C-3") 'winum-select-window-3)
  ;; (define-key winum-keymap (kbd "C-4") 'winum-select-window-4)
  ;; (define-key winum-keymap (kbd "C-5") 'winum-select-window-5)
  ;; (define-key winum-keymap (kbd "C-6") 'winum-select-window-6)
  ;; (define-key winum-keymap (kbd "C-7") 'winum-select-window-7)
  ;; (define-key winum-keymap (kbd "C-8") 'winum-select-window-8)
  :ensure t
  :config
  ;;(winum-set-keymap-prefix (kbd "C-"))'
  (global-set-key (kbd "C-0") 'winum-select-window-0-or-10)
  (global-set-key (kbd "C-1") 'winum-select-window-1)
  (global-set-key (kbd "C-2") 'winum-select-window-2)
  (global-set-key (kbd "C-3") 'winum-select-window-3)
  (global-set-key (kbd "C-4") 'winum-select-window-4)
  (global-set-key (kbd "C-5") 'winum-select-window-5)
  (global-set-key (kbd "C-6") 'winum-select-window-6)
  (global-set-key (kbd "C-7") 'winum-select-window-7)
  (global-set-key (kbd "C-8") 'winum-select-window-8)
  (setq
   window-numbering-scope            'global
   winum-ignored-buffers             '(" *which-key*")
   winum-ignored-buffers-regexp      '(" \\*Treemacs-.*"))
  (winum-mode))

(winner-mode)

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode 1)
  )

(use-package which-key
  :defer 5
  :ensure t
  :config
  (which-key-mode))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :init (setq projectile-cache-file
	      (expand-file-name "emacs_meta/projectile.cache" org-directory)
	      projectile-known-projects-file
	      (expand-file-name "emacs_meta/projectile-bookmarks.eld" org-directory))
  :bind
  ("C-c pp" . projectile-switch-project)
  ("C-c pb" . projectile-switch-to-buffer)
  ("C-c pf" . projectile-find-file)
  ("C-c pg" . projectile-grep)
  ("C-c pk" . projectile-kill-buffers)
  ;; nothing good in the modeline to keep.
  :diminish ""
  :config
  (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
  (setq projectile-sort-order 'recently-active)
  (projectile-global-mode))

(use-package org-projectile
  :ensure t
  :after org projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (setq org-projectile-projects-file
        "~/my_org/project-tasks.org")
  ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))) ;; Not necessary as my task projects are a part of the main org folder
  (push (org-projectile-project-todo-entry) org-capture-templates))

  (use-package org-brain
    :after org
    :ensure t
    :bind  ("M-s v" . org-brain-visualize)
    :init
    (setq org-brain-path "~/my_org/brain/")
    ;; ;; For Evil users
    ;; (with-eval-after-load 'evil
   ;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
    :config
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/my_org/emacs_meta/.org-id-locations")
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12)
    (add-hook 'org-brain-refile 'org-id-get-create))
;; (global-set-key (kbd "M-s v") #'org-brain-visualize)

(use-package org-web-tools
:defer 5
:ensure nil
:config
(global-set-key (kbd "H-y") 'org-web-tools-insert-link-for-url)
)

(use-package org-download
  :defer nil
  :ensure t
  ;;:after org
  :config
    ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  ;; For some reason this still seems required, despite using defer nil
  (require 'org-download)
  )



(use-package treemacs
  :ensure t

  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs
          (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          ttreemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  "~/my_org/emacs_meta/.treemacs-persist"
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    ;;(treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("M-s t t" . treemacs)
        ("M-s t w" . treemacs-switch-workspace)
        ;; ("C-x t 1"   . treemacs-delete-other-windows)
        ;; ("C-x t t"   . treemacs)
        ;; ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag)
        )
  )

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (scheme . t)
     (sqlite . t)
     (R . t)
     (lisp . t)
     (sql .  t)
     (shell . t)
     ;; (ipython . t)
  ;;   (jupyter . t)
  ;;   (ein . t)
     )
   )

(with-eval-after-load 'org
(setq org-babel-default-header-args
'((:session . "none")
(:results . "silent")
(:exports . "code")
(:cache . "no")
(:noweb . "no")
(:hlines . "no")
(:tangle . "no"))))

(with-eval-after-load 'org
  (setq org-confirm-babel-evaluate nil)
  (setq org-confirm-shell-link-function nil)
  (setq org-confirm-elisp-link-function nil))

;; Superior lisp editing
(use-package lispy
  :config
  (dolist (hook '(emacs-lisp-mode-hook
		  hy-mode-hook))
    (add-hook hook
	      (lambda ()
		(lispy-mode)
		(eldoc-mode)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(message "Loaded easier selection")

(global-set-key (kbd "M-/") (make-hippie-expand-function
			     '(try-expand-dabbrev-visible
			       try-expand-dabbrev
			       try-expand-dabbrev-all-buffers) t))

(use-package browse-kill-ring
  :ensure t
)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(message "Loaded MC")

;; Allow tree-semantics for undo operations.
(use-package undo-tree
  :diminish                       ;; Don't show an icon in the modeline
  :config
    ;; Always have it on
    (global-undo-tree-mode)

    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)

    ;; Show a diff window displaying changes between undo nodes.
;; Execute (undo-tree-visualize) then navigate along the tree to witness
;; changes being made to your file live!
)

  (use-package yasnippet)
  (use-package ivy-yasnippet
      :bind ("M-s i" . ivy-yasnippet))

(use-package swiper
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("H-s" . swiper-all)
  :diminish ivy-mode
  :config
  (ivy-mode))

(use-package avy)

(use-package counsel
  :init
  (require 'ivy)
  (setq projectile-completion-system 'ivy)
  (setq ivy-use-virtual-buffers t)
  (define-prefix-command 'counsel-prefix-map)
  (global-set-key (kbd "H-c") 'counsel-prefix-map)

  ;; default pattern ignores order.
  (setf (cdr (assoc t ivy-re-builders-alist))
	'ivy--regex-ignore-order)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-info-lookup-symbol)
   ("H-c r" . ivy-resume)
   ("H-c l" . counsel-load-library)
   ("H-c f" . counsel-git)
   ("H-c g" . counsel-git-grep)
   ("H-c a" . counsel-ag)
   ("H-c p" . counsel-pt))
  :diminish ""
  :config
  (progn
    (counsel-mode)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    (define-key ivy-minibuffer-map (kbd "M-<SPC>") 'ivy-dispatching-done)

    ;; C-RET call and go to next
    (define-key ivy-minibuffer-map (kbd "C-<return>")
      (lambda ()
	"Apply action and move to next/previous candidate."
	(interactive)
	(ivy-call)
	(ivy-next-line)))

    ;; M-RET calls action on all candidates to end.
    (define-key ivy-minibuffer-map (kbd "M-<return>")
      (lambda ()
	"Apply default action to all candidates."
	(interactive)
	(ivy-beginning-of-buffer)
	(loop for i from 0 to (- ivy--length 1)
	      do
	      (ivy-call)
	      (ivy-next-line)
	      (ivy--exhibit))
	(exit-minibuffer)))

    ;; s-RET to quit
    (define-key ivy-minibuffer-map (kbd "s-<return>")
      (lambda ()
	"Exit with no action."
	(interactive)
	(ivy-exit-with-action
	 (lambda (x) nil))))

    ;; Show keys
    (define-key ivy-minibuffer-map (kbd "?")
      (lambda ()
	(interactive)
	(describe-keymap ivy-minibuffer-map)))

    (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
    (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-backward-delete-char)))

(if (system-type-is-darwin)
    (progn
      (setq mac-left-command-modifier 'super)
      (setq mac-right-option-modifier 'hyper)))

(if (system-type-is-darwin)
    (progn
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
      (require 'mu4e)
      (require 'mu4e-contrib)
      (require 'org-mu4e)

      (setq
       mail-user-agent 'mu4e-user-agent
       mue4e-headers-skip-duplicates  t
       mu4e-view-show-images t
       mu4e-view-show-addresses 't
       mu4e-compose-format-flowed t
       ;;mu4e-update-interval 200
       message-ignored-cited-headers 'nil
       mu4e-date-format "%y/%m/%d"
       mu4e-headers-date-format "%Y/%m/%d"
       mu4e-change-filenames-when-moving t
       mu4e-attachments-dir "~/Downloads/Mail-Attachments/"
       mu4e-maildir (expand-file-name "~/my_mail/fmail")
       message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote..."
       mu4e-index-lazy-check t
       ;; After Years. I've finally found you.
       mu4e-compose-dont-reply-to-self t
       mu4e-headers-auto-update t
       )

      ;; mu4e email refiling loations
      (setq
       mu4e-refile-folder "/Archive"
       mu4e-trash-folder  "/Trash"
       mu4e-sent-folder   "/Sent"
       mu4e-drafts-folder "/Drafts"
       )

      ;; setup some handy shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/INBOX"   . ?i)
	      ("/Sent"    . ?s)
	      ("/Archive" . ?a)
	      ("/Trash"   . ?t)))

      ;;store link to message if in header view, not to header query
      (setq org-mu4e-link-query-in-headers-mode nil
            org-mu4e-convert-to-html t) ;; org -> html


      (autoload 'mu4e "mu4e" "mu for Emacs." t)

      ;; Earlier Config for sending email
      ;; (setq
      ;;  message-send-mail-function 'message-send-mail-with-sendmail
      ;;  send-mail-function 'sendmail-send-it
      ;;  message-kill-buffer-on-exit t
      ;;  )

      ;; allow for updating mail using 'U' in the main view:
      (setq mu4e-get-mail-command  "mbsync -q fins")

      ;; Stolen from https://github.com/djcb/mu/issues/1431 and found thanks to parsnip in #emacs
      (defun my-mu4e-mbsync-current-maildir (msg)
	(interactive)
	(let* ((maildir (downcase (substring (plist-get msg :maildir) 1)))
	       (mu4e-get-mail-command (format "mbsync %s" maildir)))
	  (mu4e-update-mail-and-index t)))

      ;; Enabling view in browser for HTML heavy emails that don't render well
      (add-to-list 'mu4e-view-actions
	           '("ViewInBrowser" . mu4e-action-view-in-browser) t)
      (add-to-list 'mu4e-view-actions
		   '("mbsync maildir of mail at point" . my-mu4e-mbsync-current-maildir) t)

      (add-hook 'mu4e-headers-mode-hook
		(defun my/mu4e-change-headers ()
		  (interactive)
		  (setq mu4e-headers-fields
			`((:human-date . 12)
			  (:flags . 4)
			  (:from-or-to . 15)
			  (:subject . ,(- (window-body-width) 47))
			  (:size . 7)))))

      ;; Don't keep asking for confirmation for every action
      (defun my-mu4e-mark-execute-all-no-confirm ()
	"Execute all marks without confirmation."
	(interactive)
	(mu4e-mark-execute-all 'no-confirm))
      ;; mapping x to above function
      (define-key mu4e-headers-mode-map "x" #'my-mu4e-mark-execute-all-no-confirm)

      ;; source: http://matt.hackinghistory.ca/2016/11/18/sending-html-mail-with-mu4e/

      ;; this is stolen from John but it didn't work for me until I
      ;; made those changes to mu4e-compose.el
      (defun htmlize-and-send ()
	"When in an org-mu4e-compose-org-mode message, htmlize and send it."
	(interactive)
	(when
	    (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
	  (org-mime-htmlize)
	  (org-mu4e-compose-org-mode)
	  (mu4e-compose-mode)
	  (message-send-and-exit)))

      ;; This overloads the amazing C-c C-c commands in org-mode with one more function
      ;; namely the htmlize-and-send, above.
      (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

      (setq mu4e-update-interval 300)
      (setq message-kill-buffer-on-exit t)

      ;; Config for queued sending of emails
      ;; Reference  :https://vxlabs.com/2017/02/07/mu4e-0-9-18-e-mailing-with-emacs-now-even-better/https://vxlabs.com/2017/02/07/mu4e-0-9-18-e-mailing-with-emacs-now-even-better/

      ;; when switch off queue mode, I still prefer async sending
      (use-package async
	:ensure t
	:defer nil
	:config (require 'smtpmail-async))

      (setq
       send-mail-function 'async-smtpmail-send-it
       message-send-mail-function 'async-smtpmail-send-it
       ;; replace with your email provider's settings
       smtpmail-smtp-server "smtp.fastmail.com"
       smtpmail-smtp-service 465
       smtpmail-stream-type 'ssl

       ;; if you need offline mode, set to true -- and create the queue dir
       ;; with 'mu mkdir', i.e:
       ;; mu mkdir /home/user/Mail/queue && touch ~/Maildir/queue/.noindex
       ;; https://www.djcbsoftware.nl/code/mu/mu4e/Queuing-mail.html
       smtpmail-queue-mail  nil
       smtpmail-queue-dir  (expand-file-name "~/my_mail/fmail/Queue/cur"))

      ))

(use-package org-msg
 ;; :disabled nil
  :ensure t
  :defer 5
  :config

  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi %s,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-signature "

 Regards,

 #+begin_signature
 -- *Shreyas Ragavan* \\\\
 E: shreyas@fastmail.com \\\\
 W: https://shreyas.ragavan.co \\\\
 M: +1 647-671-1851 \\\\
 #+end_signature")
  (org-msg-mode))
;; Attempt to solve the problem of forwarding emails especailly with attachments.
;(advice-add '(org-msg-mode) :after #'mu4e-compose-forward))

 (use-package org-beautify-theme
 :after (org)
 :config
 (setq org-fontify-whole-heading-line t)
 (setq org-fontify-quote-and-verse-blocks t)
 (setq org-hide-emphasis-markers t))

;; For Linux
(if (system-type-is-gnu)
    (set-face-attribute 'default nil :family "ttf-iosevka" :height 130 ))

;; For Mac OS
(if (system-type-is-darwin)
    (set-face-attribute 'default nil :family "Iosevka Type" :height 160 ))

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (disable-theme 'smart-mode-line-light)
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-position-off)
)

(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "DarkGreen"
			     :weight normal
			     :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))

(set-face-attribute 'org-todo nil
                    :box '(:line-width 2
                           :color "black"
                           :style released-button)
                    :inverse-video t
                    )
(set-face-attribute 'org-done nil
                    :box '(:line-width 2
                           :color "black"
                           :style released-button)
                    :inverse-video t
                    )
(set-face-attribute 'org-priority nil
                    :inherit font-lock-keyword-face
                    :inverse-video t
                    :box '(:line-width 2
                           :color "black"
                           :style released-button)
                    )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t initial-scratch-message nil)

(use-package visual-fill-column
:config (global-visual-fill-column-mode))

(setq-default fill-column 79)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; (defconst scimax-dir (file-name-directory (or load-file-name (buffer-file-name)))

  (defconst scimax-dir "./straight/repos/scimax")

  (use-package helm-bibtex)

  (use-package helm-projectile)

  ;; (use-package help-fns+
  ;;     :straight (help-fns+ :host github :repo "jkitchin/scimax"))


  ;; Functions for working with hash tables
  (use-package ht)

  (use-package htmlize)

  (use-package hy-mode)

  (use-package hydra
    :init
    (setq hydra-is-helpful t)

    :config
    (require 'hydra-ox))

  (use-package ivy-hydra)

  (use-package jedi)

  (use-package jedi-direx)

  (use-package diminish)

  (use-package avy)

(use-package pydoc)

(use-package pyvenv
:config
(require 'pyvenv))

(use-package rainbow-mode)

(use-package elpy
  :config
  (elpy-enable))

(use-package esup)

;; Provides functions for working with files
(use-package f)

  (use-package org-ref
    :straight (org-ref :host github :repo "jkitchin/org-ref")
:config
(require 'doi-utils)
(require 'org-ref-wos)
(require 'org-ref-pubmed)
(require 'org-ref-arxiv)
(require 'org-ref-bibtex)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)
(require 'org-ref-helm)
(require 'org-ref-isbn)

(setq org-ref-completion-library 'org-ref-ivy-cite)
;; note and bib location

(setq org-ref-bibliography-notes "~/my_org/references/references.org"
      org-ref-bibliography-notes "~/my_org/references/research_notes.org"
      org-ref-default-bibliography '("~/my_org/references/references.bib")
      org-ref-pdf-directory "~/my_org/references/pdfs/")

;; setting up helm-bibtex
(setq helm-bibtex-bibliography "~/my_org/references/references.bib"
      helm-bibtex-library-path "~/my_org/org/references/pdfs"
      helm-bibtex-notes-path "~/my_org/references/research_notes.org")

(setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5
	org-ref-bibtex-hydra-key-binding (kbd "H-b")))

(straight-use-package 'emacsql-sqlite)
(straight-use-package 'gitter)

(use-package scimax-org
  :straight (scimax-org :host github :repo "jkitchin/scimax")
  :bind
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)
     (matlab . t)
     (sqlite . t)
     (ruby . t)
     (perl . t)
     (org . t)
     (dot . t)
     (plantuml . t)
     (R . t)
     (fortran . nil)
     (C . t)))
  )

  (use-package scimax-hydra
    :straight (scimax-hydra :host github :repo "jkitchin/scimax")
    :bind ("<f12>" . scimax/body))

    (use-package scimax-journal
      :after scimax-org
      :init (setq scimax-journal-root-dir "~/my_org/journal/")
      :bind ("H-j" . scimax-journal/body)
      :straight (scimax-journal :host github :repo "jkitchin/scimax"))

  (use-package scimax-yas
    :after scimax-org
    :straight (scimax-yas :host github :repo "jkitchin/scimax"))

  (use-package scimax-ivy
    :after scimax-org
    :straight (scimax-ivy :host github :repo "jkitchin/scimax"))

    (use-package ob-ipython
      :straight (ob-ipython :host github :repo "gregsexton/ob-ipython"))

  ;;; * Applying John's customisations and monkeypatches 
  ;;; These are related to ipython kernel management
    (use-package scimax-org-babel-ipython-upstream
          :straight (scimax-org-babel-ipython-upstream :host github :repo "jkitchin/scimax"))

  ;;; * These  are related mostly to org-babel customisations
    (use-package scimax-ob
      :straight (scimax-ob :host github :repo "jkitchin/scimax"))


(setq python-shell-interpreter "python3")
(setq org-babel-python-command "python3")
(setq flycheck-python-pycompile-executable "python3")

;;; Apparently the ob-ipython build process does not symlink the client.py file which is necessary to start the client. 
;;; THis is unlikely to work on a windows machine and perhaps some conditional has to be built in
;;; It would also be nice ot have a clear method to take care of the path expansion

(call-process "/bin/bash" nil t nil "-c" "ln -s ~/.emacs.d/straight/repos/ob-ipython/client.py ~/.emacs.d/straight/build/ob-ipython/")

(straight-use-package 'beacon)
(use-package scimax-org-babel-python
      :straight (scimax-org-babel-python :host github :repo "jkitchin/scimax"))

;;; scimax-notebook.el ---    -*- lexical-binding: t -*-

;;; Commentary:
;; This is an experiment in using scimax and org-mode for scientific notebook
;; purposes. The idea is you have a "project" that is a set of org and other
;; files under version control (git). There is a "master" file that is the
;; starting point, e.g. the README.org file. You can use `projectile' to switch
;; between projects easily, or search/find files within a project.
;;
;; `nb-new' is command to create a new project, it is just a thin wrapper that
;; creates the directories, registers them with projectile, and opens the master
;; file.
;;
;; `nb-open' is a command to open an existing project. It is a thin wrapper
;; around the projectile-switch-project command that opens the master file.
;;
;; `nb-agenda' to see the TODO items within a project, or do other org-agenda
;; things within the scope of the project, e.g. search by tag/property.
;;
;; `nb-archive' creates a zip-archive of the project.
;;
;; Note there is a projectile hydra defined: `hydra-projectile/body' that may be
;; useful for scimax-notebooks.

;;; Code:

;; * Setup
(projectile-mode +1)

(use-package ggtags)
(use-package ibuffer-projectile)
(when (executable-find "ag")
  (use-package ag))

(require 'scimax-apps)

(defcustom nb-notebook-directory
  "~/vc/projects/"
  "Directory where projects are stored."
  :group 'scimax-notebook
  :type '(directory))


(unless (file-directory-p nb-notebook-directory)
  (make-directory nb-notebook-directory t))


(defcustom nb-master-file (lambda (&optional name)
			    "Return the master file name for the project."
			    "README.org")
  "A function that returns the master file in each project.
The function must take one optional argument that is a project
name. This function will be run in the root directory of the
project. The function should return a string of the master file
name. See `nb-example-master' for an example of a computed master
file."
  :group 'scimax-notebook)

(defcustom nb-project-type 'git
  "Symbol for what type of project to make.
projectile will just put a .projectile file in the directory
git will initialize the directory as a git repo."
  :group 'scimax-notebook)


(defun nb-example-master (&optional name)
  "Return the master filename for the project of NAME.
NAME is optional, and if it is nil, compute the filename from the
current directory. In this example the master file is an org-file
with the name of the root directory, with a @ prefix so it sorts
to the top of the directory with ls."
  (concat "@"
	  (file-name-base (directory-file-name default-directory))
	  ".org"))

(defcustom nb-switch-project-action
  (lambda ()
    (find-file (read-file-name "File: " "." (funcall nb-master-file))))
  "Function to run after switching projects with `nb-open'."
  :group 'scimax-notebook)

;;;###autoload
(defun nb-new (name)
  "Create a new project of NAME in `nb-notebook-directory'."
  (interactive (list (read-directory-name "New project name: " nb-notebook-directory)))
  (when (file-directory-p name)
    (user-error "%s already exists." name))
  (let ((dir (file-name-as-directory (expand-file-name name nb-notebook-directory)))
	(nb-master-file-name (funcall nb-master-file name)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (cond ((eq 'git nb-project-type)
	     (let ((default-directory dir))
	       (shell-command "git init")))
	    ((eq 'projectile nb-project-type)
	     (let ((default-directory dir))
	       (shell-command "touch .projectile")))
	    (t
	     (error "Unknown kind of project: %s" nb-project-type))))
    (projectile-add-known-project dir)
    (projectile-save-known-projects)
    (find-file (expand-file-name nb-master-file-name dir))))

;;;###autoload
(defun nb-open ()
  "Switch to a project and open the main file.
This is a thin wrapper on `projectile-switch-project' that opens the master file."
  (interactive)
  (let ((projectile-switch-project-action nb-switch-project-action))
    (projectile-switch-project)))

;;;###autoload
(defun nb-git-clone (url path)
  "Clone a git repo at URL as a project at PATH in `nb-notebook-directory'.
The URL and PATH should work in a command like: git clone URL
PATH. You need to specify the path you want the file to be in. A
default name based on the url is suggested."
  (interactive (list (read-string "git url: ") nil))
  (setq path (read-directory-name "Path: " nb-notebook-directory
				  nil nil
				  (replace-regexp-in-string
				   "\\.git\\'" ""
				   (car (last (f-split url))))))
  (let ((default-directory nb-notebook-directory))
    (when (file-exists-p path)
      (error "%S already exists" path))
    (make-directory path t)
    (shell-command-to-string (format "git clone %s \"%s\"" urlg path))
    (dired path)))

;;;###autoload
(defun nb-clone ()
  "Create a clone (by a recursive copy) of the current notebook."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (dir-one-up (file-name-directory (directory-file-name project-root)))
	 (name (file-name-base (directory-file-name project-root)))
	 (clone-base-name (read-directory-name
			   "Clone name: "
			   dir-one-up  nil nil
			   (concat name "-clone"))))
    (let ((default-directory dir-one-up))
      (shell-command (format "cp -R %s %s" name clone-base-name))
      (projectile-add-known-project clone-base-name)
      (projectile-save-known-projects)
      (projectile-switch-project-by-name clone-base-name))))

(defcustom nb-agenda-files nil
  "A file, a list of files or function to generate a list of org-files to make an agenda from.
The function should return a string filepath or list of absolute
file paths. The function will be run in the root project
directory. You may want to make this a directory local variable."

  :group 'scimax-notebook
  :type '(string list function))


;;;###autoload
(defun nb-agenda (project &optional all-org-files)
  "Show org-agenda for org-files in the notebook."
  (interactive (list (completing-read "Project: "
				      (mapcar 'expand-file-name
					      (append
					       (list (projectile-project-root
						      (projectile-project-name)))
					       (projectile-relevant-known-projects)))
				      nil t
				      (projectile-project-root (projectile-project-name)))
		     current-prefix-arg))
  (let ((org-agenda-files (if (or all-org-files
				  (null nb-agenda-files))
			      (mapcar
			       (lambda (f) (expand-file-name
					    f (projectile-project-root)))
			       (-filter (lambda (f)
					  (and
					   (f-ext? f "org")
					   (not (s-contains? "#" f))))
					(projectile-current-project-files)))
			    (cond
			     ((listp nb-agenda-files)
			      nb-agenda-files)
			     ((functionp nb-agenda-files)
			      (let ((default-directory (projectile-project-root
							(projectile-project-name))))
				(funcall nb-agenda-files)))
			     (t
			      nb-agenda-files)))))
    (org-agenda)))

;;;###autoload
(defun nb-git-archive ()
  "Create an archive of the current notebook.
This uses git archive to create an archive of the current state
of the notebook. The zip file will be timestamped in the root
project directory. If your repo contains untracked files or
uncommitted changes, you will be prompted to continue."
  (let ((output (shell-command-to-string "git status --porcelain")))
    (unless (string= "" output)
      (when
	  (y-or-n-p
	   (format
	    "Your notebook contains uncommitted changes or files:\n%s\n Continue? " output))
	(shell-command
	 (format
	  "git archive --format zip HEAD -o \"%s-%s.zip\""
	  (f-join (projectile-project-root)
		  (car (last (f-split (projectile-project-root)))))
	  (format-time-string "%Y-%m-%d-%H:%M%p")))))))

(defcustom nb-archive-command "zip"
  "Command to make archives.
An alternative is tar."
  :group 'scimax-notebook)


(defcustom nb-archive-command-options "-r"
  "Command options to make archives.
For tar you want -czf for a tar.gz
or tar -cjf for a bzipped file
For bzip2 you want "
  :group 'scimax-notebook)


(defcustom nb-archive-extension ".zip"
  "Default extension for the archive.
For tar with -czf I recommend .tar.gz
For tar with -cjf I recommend .tbz2"
  :group 'scimax-notebook)


(defun nb-archive (zip-file project)
  "Create an archive file of the project.
The type of archive is determined by `nb-archive-command'."
  (interactive (list (read-string
		      "Archive name: "
		      (concat (projectile-project-name) nb-archive-extension))
		     (projectile-completing-read "Project: "
						 (projectile-relevant-known-projects))))
  (let ((default-directory project))
    (message
     (format "%s %s %s \"%s\""
	     nb-archive-command nb-archive-command-options
	     zip-file project))))

(defun nb-list-tags ()
  "Get a list of tags in the notebook."
  (interactive)
  (let ((tags '())
	(already-open nil)
	(org-files (mapcar
		    (lambda (f) (expand-file-name
				 f (projectile-project-root)))
		    (-filter (lambda (f)
			       (and
				(f-ext? f "org")
				(not (s-contains? "#" f))))
			     (projectile-current-project-files))))
	(inhibit-local-variables-regexps))
    ;; Ignore local variables for this.
    (push "\\.org\\'" inhibit-local-variables-regexps)
    (cl-loop for org-file in org-files do
	     (setq already-open (find-buffer-visiting org-file))
	     (with-current-buffer (find-file-noselect org-file)
	       (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (while (re-search-forward org-heading-regexp nil t)
		     (setq tags (append tags (org-get-tags)))))))
	     (unless already-open
	       (kill-buffer already-open)
	       (setq already-open nil)))
    (-uniq tags)))

(use-package ivy-xref
  :ensure t
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))


(defun nb-search (file-pattern regexp)
  "Search files matching FILE-PATTERN for REGEXP and show matches."
  (interactive "sfile pattern: \nsSearch for: ")
  (let* ((files (projectile-project-files (projectile-project-root)))
	 (ignores (nconc (mapcar
                          (lambda (s) (concat s "/"))
                          grep-find-ignored-directories)
                         grep-find-ignored-files))
	 (xrefs (cl-mapcan
                 (lambda (file)
		   (when (file-exists-p file)
                     (xref-collect-matches regexp "*" file
                                           (and (file-directory-p file)
						ignores))))
                 files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil t)))


(defun nb-search-all (regexp)
  "Search files for REGEXP and show matches."
  (interactive "sSearch for: ")
  (let* ((files (projectile-project-files (projectile-project-root)))
	 (ignores (nconc (mapcar
                          (lambda (s) (concat s "/"))
                          grep-find-ignored-directories)
                         (append '("*.pdf") grep-find-ignored-files)))
	 ;; This was a little surprising, I had to add -a in a few places to treat binary files like text
	 (grep-host-defaults-alist '((localhost
				      (grep-command "grep  -nH --null -e -a ")
				      (grep-template "grep <X> <C> -nH --null -e <R> <F>")
				      (grep-use-null-device nil)
				      (grep-find-command ("find . -type f -exec grep  -nH --null -e -a \\{\\} +" . 42))
				      (grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> -a \\{\\} +")
				      (grep-use-null-filename-separator t) (grep-find-use-xargs exec-plus)
				      (grep-highlight-matches nil))))

	 (xrefs (cl-mapcan
                 (lambda (file)
		   (when (file-exists-p file)
                     (xref-collect-matches regexp "*" file
                                           (and (file-directory-p file)
						ignores))))
                 files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil t)))

(defun nb-search-title ()
  "Select a notebook file by title, date or filename."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (org-files (mapcar (lambda (f)
			      (expand-file-name f project-root))
			    (-filter (lambda (f) (f-ext? f "org"))
				     (projectile-project-files project-root))))
	 data
	 candidates
	 format-string
	 title
	 date)

    ;; Get title, date and filename for each org-file. If there is no date, we
    ;; use last modified time. It doesn't seem possible to get the file creation
    ;; time. I am not sure this is robust for all things you might put into the
    ;; DATE field. I assume it is something org can read and convert to a time.
    (setq data (mapcar
		(lambda (f)
		  (when (file-exists-p f)
		    (with-temp-buffer
		      (insert-file-contents f)
		      (setq title (if (re-search-forward "#\\+TITLE:\\(.*\\)" nil t)
				      (match-string 1)
				    "No title"))


		      (goto-char (point-min))
		      (setq date (if (re-search-forward "#\\+DATE:\\(.*\\)" nil t)
				     (format-time-string "%Y-%m-%d"
							 (org-read-date nil t (match-string 1)))
				   (format-time-string
				    "mod-%Y-%m-%d"
				    (file-attribute-modification-time
				     (file-attributes f)))))

		      (list title date f))))
		org-files))

    ;; Sort by date, more recent things will be first
    (setq data (cl-sort (copy-sequence data)
			(lambda (a b) (org-time> (nth 1 a) (nth 1 b)))))

    ;; Now create a format string so the longest title fits and is aligned.
    (setq format-string (format "%%%ss | %%16s | %%s"
				(apply 'max (mapcar (lambda (e) (length (car e))) data))))
    ;; These are the candidates we will choose from.
    (setq candidates (cl-loop for (title date f) in data
			      collect
			      (list (format format-string title date f) f)))

    ;; I use completing-read here so you can use your own backend. The only
    ;; downside is I can't put many actions like in a dedicated ivy command.
    (find-file (cadr (assoc
		      (completing-read "Open: " candidates)
		      candidates)))))

(defun nb-help ()
  "Open the org-file describing the notebook."
  (interactive)
  (find-file (expand-file-name "scimax-notebook.org" scimax-dir)))

(defun nb-parse-path (path)
  "Parse PATH into parts.
PATH is a :: separated string with up to 3 parts.
Returns a list of (project fpath link-target).
The link target is optional, and defaults to line 1."
  ;; Somehow split-string must change match-data. This messes up
  ;; fontification...
  (save-match-data
    (let* ((parts (split-string path "::")))
      (when (> (length parts) 3)
	(error "There should only be 3 parts separated by ::"))
      (list
       (nth 0 parts)
       (nth 1 parts)
       (or (nth 2 parts) "1")))))

(defun nb-follow (path)
  "Open the project at PATH."
  (interactive (list (org-element-property :path (org-element-context))))
  (destructuring-bind (project fpath link-target) (nb-parse-path path)
    (let* ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath p)))
				project-candidates)))
      (cond
       ((null project-candidates)
	(error "%s is not a known project" project))
       ((null candidates)
	(error "%s was not found in %s\nproject-candidates: %S\ncandidates: %s" fpath project project-candidates candidates))
       ;; one project, and the file exists
       ((and (= 1 (length candidates))
	     (file-exists-p (expand-file-name fpath (car candidates))))
	(org-mark-ring-push)
	(find-file (expand-file-name fpath (car candidates))))
       ;; multiple matches, select project interactively
       (t
	(org-mark-ring-push)
	(find-file (expand-file-name fpath (completing-read "Project: " candidates)))))
      ;; If we get here, we have not errored and should have opened a file. Now,
      ;; link-target the end link.
      (cond
       ((eq major-mode 'org-mode)
	(when (not (or (null link-target) (string= "" link-target)))
	  (cond
	   ((string-match "\\<[0-9]+\\>" link-target)
	    (forward-line (- (string-to-number link-target) 1)))
	   ((string-match "\\<c\\([0-9]+\\)\\>" link-target)
	    (goto-char (string-to-number (match-string 1 link-target))))
	   (t
	    (org-open-link-from-string (format "[[%s]]" link-target)))))
	(org-show-entry))
       ;; everything else
       (t
	(cond
	 ;; if it is just a number it is a line number
	 ((string-match "\\<[0-9]*\\>" link-target)
	  (forward-line (- (string-to-number link-target) 1)))
	 ;; a pattern like c23 means go to char 23
	 ((string-match "^c\\([0-9]*\\)" link-target)
	  (goto-char (string-to-number (match-string 1 link-target))))
	 (t
	  (goto-char (point-min))
	  (goto-char (re-search-forward (regexp-quote link-target) nil t)))))))))

(defun nb-follow-other (path &optional new-frame)
  "Open the project at PATH in other window."
  (interactive (list (org-element-property :path (org-element-context))
		     current-prefix-arg))
  (destructuring-bind (project fpath link-target) (nb-parse-path path)
    (let* ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath p)))
				project-candidates)))
      (cond
       ((null project-candidates)
	(error "%s is not a known project" project))
       ((null candidates)
	(error "%s was not found in %s\nproject-candidates: %S\ncandidates: %s" fpath project project-candidates candidates))
       ;; one project, and the file exists
       ((and (= 1 (length candidates))
	     (file-exists-p (expand-file-name fpath (car candidates))))
	(org-mark-ring-push)
	(if new-frame
	    (find-file-other-frame (expand-file-name fpath (car candidates)))
	  (find-file-other-window (expand-file-name fpath (car candidates)))))
       ;; multiple matches, select project interactively
       (t
	(org-mark-ring-push)
	(if new-frame
	    (find-file-other-frame (expand-file-name fpath (completing-read "Project: " candidates)))
	  (find-file-other-window (expand-file-name fpath (completing-read "Project: " candidates))))))
      ;; If we get here, we have not errored and should have opened a file. Now,
      ;; link-target the end link.
      (cond
       ((eq major-mode 'org-mode)
	(when (not (or (null link-target) (string= "" link-target)))
	  (cond
	   ((string-match "\\<[0-9]+\\>" link-target)
	    (forward-line (- (string-to-number link-target) 1)))
	   ((string-match "\\<c\\([0-9]+\\)\\>" link-target)
	    (goto-char (string-to-number (match-string 1 link-target))))
	   (t
	    (org-open-link-from-string (format "[[%s]]" link-target)))))
	(org-show-entry))
       ;; everything else
       (t
	(cond
	 ;; if it is just a number it is a line number
	 ((string-match "\\<[0-9]*\\>" link-target)
	  (forward-line (- (string-to-number link-target) 1)))
	 ;; a pattern like c23 means go to char 23
	 ((string-match "c\\([0-9]*\\)" link-target)
	  (goto-char (string-to-number (match-string 1 link-target))))
	 ;; Everything else means search for it. I don't know why I have to
	 ;; use goto-char here. I thought it should just go.
	 (t
	  (goto-char (re-search-forward link-target nil 'mv)))))))))


(defun nb-follow-other-frame (path)
  "Follow path in other frame."
  (interactive (list (org-element-property :path (org-element-context))))
  (nb-follow-other path t))

(defun nb-follow-sys (path)
  "Open the project at PATH with a system program."
  (interactive (list (org-element-property :path (org-element-context))))
  (destructuring-bind (project fpath link-target) (nb-parse-path path)
    (let* ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath p)))
				project-candidates)))
      (cond
       ((null project-candidates)
	(error "%s is not a known project" project))
       ((null candidates)
	(error "%s was not found in %s\nproject-candidates: %S\ncandidates: %s" fpath project project-candidates candidates))
       ;; one project, and the file exists
       ((and (= 1 (length candidates))
	     (file-exists-p (expand-file-name fpath (car candidates))))
	(org-mark-ring-push)
	(org-open-file-with-system (expand-file-name fpath (car candidates))))
       ;; multiple matches, select project interactively
       (t
	(error "no match found"))))))

(defun nb-store-link ()
  "Store a project link to a file in a project."
  (if (or (null (buffer-file-name)) (not (projectile-project-p)))
      nil
    (let* ((root (projectile-project-root))
	   (current-file (buffer-file-name))
	   (project (car (last (f-split (projectile-project-root)))))
	   (relpath (file-relative-name current-file root))
	   (link-target (format "c%s" (point))))

      (org-store-link-props
       :type "nb"
       ;; Note I use the concat here just to avoid fontifying errors in the link
       ;; in the org file.
       :link (format (concat "nb:" "%s::%s::%s") project relpath link-target)
       :description "")
      (format (concat "nb:" "%s::%s::%s") project relpath link-target))))

(defun nb-complete-link ()
  "Create a link with completion."
  ;; Pick a project
  (let* ((project-root  (projectile-completing-read
			 "Project: "
			 (projectile-relevant-known-projects)
			 :initial-input (projectile-project-root (projectile-project-name))))
	 (project (projectile-project-name project-root))
	 (file (completing-read "File: " (projectile-project-files project-root))))
    (format "nb:%s::%s" project file)))


(defun nb-insert-link ()
  "Insert a link with completion."
  (insert (nb-complete-link)))

(defun nb-link-face (path)
  "Compute a face for the link.
If everything is in order it is an 'org-link.
If there are multiple projects it will be orange.
If we can't find a project or file, it will be red."
  ;; Something in here modifies the match-data which will mess up fontification.
  ;; We save it to avoid that.
  (save-match-data
    (let* ((parts (nb-parse-path path))
	   (project (nth 0 parts))
	   (fpath (nth 1 parts))
	   (follow (nth 2 parts))
	   (projects (append (projectile-relevant-known-projects)
			     (list (projectile-project-root))))
	   (project-candidates (-filter (lambda (p)
					  (string-match project (or p "")))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath (or p ""))))
				project-candidates)))
      (cond
       ;; No project
       ((null candidates)
        '(:foreground "red"))
       ;; one project, and the file exists
       ((= 1 (length candidates))
	'(:foreground "darkviolet"))
       ;; Multiple projects seem to match.
       ((> (length candidates) 1)
        '(:foreground "orange"))))))

(defun nb-link-tooltip (_win _obj position)
  "A tooltip for the nb links."
  (save-match-data
    (save-excursion
      (goto-char position)
      (let ((path (org-element-property :path (org-element-context))))
  	(destructuring-bind (project fpath follow) (nb-parse-path path)
  	  (let* ((projects (append (projectile-relevant-known-projects)
				   (list (projectile-project-root))))
  		 (project-candidates (-filter (lambda (p)
  						(string-match project p))
  					      projects))
  		 ;; These are projects that match the spec, and that have the file we want.
  		 (candidates (-filter (lambda (p)
  					(file-exists-p (expand-file-name fpath p)))
  				      project-candidates)))
  	    (cond
  	     ((null project-candidates)
  	      (format "%s is not a known project." project))

  	     ((null candidates)
  	      (format "%s not found in %s." fpath project))

  	     ;; There is one project, and the file is in it.
  	     ((= 1 (length candidates))
  	      ;; Show the path
  	      (expand-file-name fpath (car candidates)))

  	     ;; Multiple projects. We don't check for file existence
  	     ((> (length candidates) 1)
  	      (format "Multiple projects have %s: %S" fpath candidates))

  	     (t
  	      "Not sure what is going on with this one."))))))))

(defun nb-activate-link (start end path bracketp)
  "Activate a project link.
This is used to put image overlays on links.
START and END are the positions of the link.
PATH is the link PATH.
BRACKETP is non-nil for bracketed links."
  (destructuring-bind (project fpath link-target) (nb-parse-path path)
    (if (and (string-match (org-image-file-name-regexp) fpath)
  	     (not (ov-at start)))
  	;; Find the image
  	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
  					     (list
  					      (when (projectile-project-p)
  						(projectile-project-root))))))
  	       ;; These are projects that match the project spec
  	       (project-candidates (-filter (lambda (p)
  					      (string-match (concat project "/\\'") p))
  					    projects))
  	       ;; These are projects that match the spec, and that have the file we want.
  	       (candidates (-filter (lambda (p)
  				      (file-exists-p (expand-file-name fpath p)))
  				    project-candidates))
  	       (img-file (when (and (= 1 (length candidates))
  				    (file-exists-p (expand-file-name fpath (car candidates))))
  			   (expand-file-name fpath (car candidates)))))
  	  (when img-file
  	    (let* ((ov (make-overlay start end))
  		   (lnk (org-element-context))
  		   (parent (org-element-property :parent lnk))
  		   (ao (when parent (org-element-property :attr_org parent)))
  		   (width (when ao
  		   	    (plist-get
  		   	     (org-export-read-attribute :attr_org  parent) :width)))
  		   (img-file (if width
  		   		 (funcall  org-inline-image-resize-function img-file width)
  		   	       img-file))
  		   (img (create-image (or img-file )
  		   		      nil
  		   		      nil
  		   		      :width width)))

  	      (overlay-put ov 'display img)
  	      (overlay-put ov 'help-echo (expand-file-name fpath (car candidates)))
  	      (overlay-put ov 'face 'default)
  	      (overlay-put ov 'org-image-overlay t)
  	      (overlay-put ov 'modification-hooks
  	      		   (list 'org-display-inline-remove-overlay))
  	      (push ov org-inline-image-overlays)))))))

(defun nb-link-bash ()
  "Open the nb link at point in bash."
  (interactive)
  (let* ((link (org-element-context))
	 (path (org-element-property :path link)))
    (when (and (eq 'link (car link))
	       (string= "nb" (org-element-property :type link)))
      (destructuring-bind (project fpath link-target) (nb-parse-path path)
	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
					     (list
					      (when (projectile-project-p)
						(projectile-project-root))))))
	       ;; These are projects that match the project spec
	       (project-candidates (-filter (lambda (p)
					      (string-match (concat project "/\\'") p))
					    projects))
	       ;; These are projects that match the spec, and that have the file we want.
	       (candidates (-filter (lambda (p)
				      (file-exists-p (expand-file-name fpath p)))
				    project-candidates)))
	  (if (= 1 (length candidates))
	      (bash (expand-file-name (car candidates)))
	    (bash (read-string "Project: " candidates))))))))

(defun nb-link-explorer ()
  "Open the nb link at point in explorer/finder."
  (interactive)
  (let* ((link (org-element-context))
	 (path (org-element-property :path link)))
    (when (and (eq 'link (car link))
	       (string= "nb" (org-element-property :type link)))
      (destructuring-bind (project fpath link-target) (nb-parse-path path)
	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
					     (list
					      (when (projectile-project-p)
						(projectile-project-root))))))
	       ;; These are projects that match the project spec
	       (project-candidates (-filter (lambda (p)
					      (string-match (concat project "/\\'") p))
					    projects))
	       ;; These are projects that match the spec, and that have the file we want.
	       (candidates (-filter (lambda (p)
				      (file-exists-p (expand-file-name fpath p)))
				    project-candidates)))
	  (if (= 1 (length candidates))
	      (explorer (expand-file-name (car candidates)))
	    (explorer (read-string "Project: " candidates))))))))

(defun nb-link-projectile-find-file ()
  "Open the nb link at point with projectile."
  (interactive)
  (let* ((link (org-element-context))
	 (path (org-element-property :path link)))
    (when (and (eq 'link (car link))
	       (string= "nb" (org-element-property :type link)))
      (destructuring-bind (project fpath link-target) (nb-parse-path path)
	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
					     (list
					      (when (projectile-project-p)
						(projectile-project-root))))))
	       ;; These are projects that match the project spec
	       (project-candidates (-filter (lambda (p)
					      (string-match (concat project "/\\'") p))
					    projects))
	       ;; These are projects that match the spec, and that have the file we want.
	       (candidates (-filter (lambda (p)
				      (file-exists-p (expand-file-name fpath p)))
				    project-candidates)))
	  (if (= 1 (length candidates))
	      (let ((default-directory (expand-file-name (car candidates))))
		(projectile-completing-read "Find file: "
                                            (projectile-project-files
					     (projectile-project-root))
					    :initial-input fpath))))))))

(defun nb-event (event)
  "EVENT is from a mouse click.
We use this with C-mouse-1 on a link."
  (interactive "e")
  (with-selected-window (nth 0 (cadr event))
    (goto-char (nth 1 (cadr event)))
    (nb-hydra/body)))

(defvar nb-link-map (let ((map (copy-keymap org-mouse-map)))
		      (define-key map (kbd "M-o") 'nb-follow-other)
		      (define-key map (kbd "M-O") 'nb-follow-other-frame)
		      (define-key map (kbd "M-s") 'nb-follow-sys)
		      (define-key map (kbd "M-b") 'nb-link-bash)
		      (define-key map (kbd "M-e") 'nb-link-explorer)
		      (define-key map (kbd "M-f") 'nb-link-projectile-find-file)
		      (define-key map (kbd "M-h") 'nb-hydra/body)
		      (define-key map (kbd "<C-mouse-1>") 'nb-event)
		      map)
  "Key bindings for notebook links")

(org-link-set-parameters
 "nb"
 :follow #'nb-follow
 :store #'nb-store-link
 :complete #'nb-complete-link
 :help-echo #'nb-link-tooltip
 :activate-func #'nb-activate-link
 :face #'nb-link-face
 :keymap nb-link-map)

(defhydra nb-hydra (:hint nil :color blue)
  "
navigation      search              utilities            link
--------------------------------------------------------------------------------
_f_: file       _sa_: search all   _b_: Open in bash      _o_: open other window
_d_: dir        _ss_: search some  _e_: Open in explorer  _O_: open other frame
_D_: open root  _sb_: search bufs  _n_: new notebook      _y_: open with sys
"
  ("b" bash "bash")
  ("e" explorer "explorer")
  ("f" projectile-find-file-dwim "find-file")
  ("d" projectile-find-dir "find dir")
  ("D" projectile-dired "open root in dired")
  ("n" nb-new "new notebook")

  ("sa" nb-search-all "search all files")
  ("ss" nb-search "search some files")
  ("sb" projectile-multi-occur "search nb buffers")
  ("st" nb-search-title "Search by title/date")
  ("o" nb-follow-other "open other window")
  ("O" nb-follow-other-frame "Open other frame")
  ("y" nb-follow-sys "Open with system program"))

(setq nb-notebook-directory "~/my_projects/")
(global-set-key (kbd "M-s n") 'nb-hydra/body)

  ;; (require 'org-id)
  ;; (setq org-id-link-to-org-use-id t)
  ;; (org-link-set-parameters "id" :store #'org-id-store-link)
  ;; (org-link-set-parameters "nb" :store nil)
  ;; ;; Update ID file .org-id-locations on startup
  ;; ;; This adds too much time to startup
  ;; ;; (org-id-update-id-locations)

  ;; (setq org-id-method (quote uuidgen))
  ;; (setq org-id-track-globally t)
  (setq org-id-locations-file "~/my_org/emacs_meta/.org-id-locations")
  ;; (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
