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

(require 'cl)

(defun sr/fun/tangle-on-save-init ()
(when (string= buffer-file-name (file-truename "~/.emacs.d/init.org"))
(org-babel-tangle)))

(add-hook 'after-save-hook 'sr/fun/tangle-on-save-init)

(defun sr/fun/homedir (foldername)
"Function to extract the home directory path"
  (expand-file-name foldername (getenv "HOME")))

(defun sr/fun/project-dir (foldername)
"Function to prepend the project directory path to any folder. Starts from the home directory."
  (expand-file-name foldername (sr/fun/homedir "my_projects" )))

(defun sr/fun/org-dir (foldername)
"Function to prepend the org directory path to any folder. Starts from the home directory."
  (expand-file-name foldername (sr/fun/homedir "my_org" )))

(defun sr/fun/emacs-dir (foldername)
"Function to prepend the project directory path to any folder. Starts from the home directory."
  (expand-file-name foldername (sr/fun/homedir ".emacs.d" )))

(setq auto-save-default t)
(setq auto-save-timeout 20
      auto-save-interval 60)

(defvar emacs-autosave-directory
(concat user-emacs-directory "autosaves/"))

(unless (file-exists-p emacs-autosave-directory)
(make-directory emacs-autosave-directory))

(setq auto-save-file-name-transforms
`((".*" ,emacs-autosave-directory t)))

(setq backup-directory-alist `((".*" . ,emacs-autosave-directory)))

(use-package backup-each-save
:straight t
:config (add-hook 'after-save-hook 'backup-each-save))

(setq vc-make-backup-files t)

(setq backup-by-copying t)

(setq kept-new-versions 10
kept-old-verisons 0
delete-old-versions t)

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
    :diminish git-gutter
    :config
    (global-git-gutter-mode 't)
    :diminish git-gutter-mode)

  (use-package magit
    :init (setq magit-completing-read-function 'ivy-completing-read)
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

;; Indent by header level 

(with-eval-after-load 'org
   (add-hook 'org-mode-hook #'org-indent-mode))

;; Enable flyspell mode

(add-hook 'org-mode-hook 'flyspell-mode)

(use-package ox-pandoc
  :ensure t
  :straight t
  :defer 5)

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

(defun sr/fun/todo-active ()
'("* %doct(todo-state) %^{Description}"
":PROPERTIES:"
":CREATED: %U"
":PLANNED: %t"
":END:"
"%?"))

(defun sr/fun/todo-passive ()
'("* %doct(todo-state) %^{Description}"
":PROPERTIES:"
":CREATED: %U"
":END:"
"%?"))

(defun sr/fun/todo-mail-active ()
'("* %doct(todo-state) %a"
":PROPERTIES:"
":CREATED: %U"
":PLANNED: %t"
":END:"
"%?"))

(setq org-capture-templates
      '(("t" "Task entry")
        ("tt" "Todo - Fast Now" entry (file+headline "~/my_org/todo-global.org" "@Inbox")
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

(use-package bufler
  :straight (bufler :host github :repo "alphapapa/bufler.el")
:bind ("C-x C-b" . bufler-list))

  (use-package which-key
    :defer 5
    :diminish which-key-mode
    :straight t
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
    :straight (org-brain :type git :host github :repo "Kungsgeten/org-brain"
			 :fork (:host github :repo "dustinlacewell/org-brain"))
    :after org
    :bind ("M-s v" . org-brain-visualize)
    :config
    ;; this unbinds all default org-brain bindings
    (setcdr org-brain-visualize-mode-map nil)
    (setq
     ;; org-brain-path (f-join path-of-this-repo "brain")
     org-brain-visualize-default-choices 'all
     org-brain-include-file-entries t
     org-brain-scan-for-header-entries t
     org-brain-file-entries-use-title t
     org-brain-show-full-entry t
     org-brain-show-text t
     org-id-track-globally t
     org-brain-vis-current-title-append-functions '(org-brain-entry-tags-string)
     org-brain-title-max-length 24)
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:ID: [%(org-id-get-create)]\n:END:" :empty-lines 1
          org-capture-templates)
    (add-hook 'org-brain-refile 'org-id-get-create)))

  (defun my/org-brain-visualize-parent ()
    (interactive)
    (when (org-brain-parents (org-brain-entry-at-pt)) (org-brain-visualize-parent (org-brain-entry-at-pt))))

  (defun my/org-brain-visualize-child (entry &optional all)
    (interactive (list (org-brain-entry-at-pt)))
    (when (org-brain-children entry)
      (let* ((entries (if all (org-brain-children entry)
                      (org-brain--linked-property-entries
                       entry org-brain-children-property-name)))
           (child (cond
                   ((equal 1 (length entries)) (car-safe entries))
                   ((not entries) (error (concat entry " has no children")))
                   (t (org-brain-choose-entry "Goto child: " entries nil t)))))
        (org-brain-visualize child))))

  (defun my/next-button-with-category (category)
    (let ((original-point (point))
          (first-result (text-property-search-forward 'brain-category category t t)))
      (when first-result
            (goto-char (prop-match-beginning first-result)))
      (when (eq original-point (point))
        (beginning-of-buffer)
        (let ((second-result (text-property-search-forward 'brain-category category t t)))
          (when second-result
            (goto-char (prop-match-beginning second-result))))
        (when (eq 0 (point))
          (goto-char original-point))
        )
      ))

  (defun my/previous-button-with-category (category)
    (let ((result (text-property-search-backwards 'brain-category category nil t)))))

  (defun my/next-brain-child ()
    (interactive)
    (my/next-button-with-category 'child))

  (defun my/next-brain-history ()
    (interactive)
    (my/next-button-with-category 'history))

  (defun my/avy-brain-jump (category)
    (avy-jump "\\<." :pred (lambda () (and (eq category (get-text-property (point) 'brain-category))
                                      (eq (- (point) 1) (button-start (button-at (point))))))
              :action (lambda (p) (goto-char (+ 1 p)) (push-button))))

  (defun my/avy-brain-jump-history ()
    (interactive)
    (my/avy-brain-jump 'history))

  (defun my/avy-brain-jump-child ()
    (interactive)
    (my/avy-brain-jump 'child))

  (defun my/avy-brain-jump-parent ()
    (interactive)
    (my/avy-brain-jump 'parent))

  (defun my/avy-brain-jump-friend ()
    (interactive)
    (my/avy-brain-jump 'friend))

  (defun my/avy-brain-jump-sibling ()
    (interactive)
    (my/avy-brain-jump 'sibling))

  (use-package polybrain
    :defer nil
    :after org-brain
    :straight (polybrain :type git :host github :repo "dustinlacewell/polybrain.el")
    :bind (:map org-brain-visualize-mode-map
           ("m" . org-brain-visualize-mind-map)
           ("<tab>" . backward-button)
           ("S-<tab>" . forward-button)
           ("DEL" . org-brain-visualize-back)
           ("r" . org-brain-open-resource)
           ("v" . org-brain-visualize)

           ("i" . org-brain-pin)
           ("T" . org-brain-set-title)
           ("t" . org-brain-set-tags)
           ("d" . org-brain-delete-entry)
           ("R" . org-brain-visualize-add-resource)
           ("o" . org-brain-goto-current)
           ("O" . org-brain-goto)

           ("c" . org-brain-add-child)
           ("C" . org-brain-remove-child)

           ("p" . org-brain-add-parent)
           ("P" . org-brain-remove-parent)

           ("f" . org-brain-add-friendship)
           ("F" . org-brain-remove-friendship)

           ("e" . org-brain-annotate-edge)


           ("M-p" . my/avy-brain-jump-parent)
           ("M-c" . my/avy-brain-jump-child)
           ("M-s" . my/avy-brain-jump-sibling)
           ("M-f" . my/avy-brain-jump-friend)
           ("M-h" . my/avy-brain-jump-history)

           :map poly-brain-mode-map
           ("C-x C-s" . polybrain-save)
           ("<M-SPC>" . polybrain-switch))
    :config 
    (require 'polybrain))

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

(use-package nov
:straight t
:config
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(straight-use-package 'helm-ag)

(use-package helm-org-rifle
  :straight t
  :config
  (global-set-key (kbd "C-c C-w") #'helm-org-rifle--refile))

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
  :diminish undo-tree-mode                      ;; Don't show an icon in the modeline
  :config
    ;; Always have it on
    (global-undo-tree-mode)

    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)

    ;; Show a diff window displaying changes between undo nodes.
;; Execute (undo-tree-visualize) then navigate along the tree to witness
;; changes being made to your file live!
)

  (use-package yasnippet
    :straight t
    :config
  (yas-global-mode 1))
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

(setq custom-safe-themes t)

(use-package poet-theme
  :straight t
  :config
  (set-face-attribute 'default nil :family "iA Writer Mono V" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka term")
  (set-face-attribute 'variable-pitch nil :family "iA Writer Duo S")
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.3 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2 :weight bold))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0 :weight bold))))
   )
  ;; Enabling the variable pitch mode
  (add-hook 'text-mode-hook
	    (lambda ()
	      (variable-pitch-mode 1)
	      (visual-line-mode 1)))
  (load-theme 'poet-dark))

(use-package olivetti
:config
(olivetti-mode 1))

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

;; Removing all the bars 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; No start up message and nothing to pollute the scratch buffer
(setq inhibit-startup-message t initial-scratch-message nil)

(use-package visual-fill-column
:config (global-visual-fill-column-mode))

(setq-default fill-column 70)

;; (add-to-list 'load-path (sr/fun/scimax-ref-dir "scimax-stuff"))
;; (defvar scimax-dir (sr/fun/scimax-ref-dir "scimax-stuff"))

    (use-package helm-bibtex)

    (use-package helm-projectile)

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

    ;; (use-package avy)

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

  (straight-use-package 'dash)
  (straight-use-package 'dash-functional)
  (straight-use-package 'cl)
  (straight-use-package 'ov)
(straight-use-package 'flx)

  (use-package auto-complete
    :diminish auto-complete-mode
    :config (ac-config-default))

(use-package google-this
  :config
  (google-this-mode 1))

(straight-use-package 'ggtags)
(straight-use-package 'ibuffer-projectile)

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

(require 'org-inlinetask)
(require 'org-mouse)

;; Make editing invisible regions smart
(setq org-catch-invisible-edits 'smart)

;; allow lists with letters in them.
(setq org-list-allow-alphabetical t)

(setq org-src-tab-acts-natively t)

(setq org-use-speed-commands t)

(add-to-list 'org-speed-commands-user (cons "P" 'org-set-property))
(add-to-list 'org-speed-commands-user (cons "d" 'org-deadline))

;; Mark a subtree
(add-to-list 'org-speed-commands-user (cons "m" 'org-mark-subtree))

;; Widen
(add-to-list 'org-speed-commands-user (cons "S" 'widen))

;; kill a subtree
(add-to-list 'org-speed-commands-user (cons "k" (lambda ()
						  (org-mark-subtree)
						  (kill-region
						   (region-beginning)
						   (region-end)))))

;; Jump to headline
(add-to-list 'org-speed-commands-user
	     (cons "q" (lambda ()
			 (avy-with avy-goto-line
			   (avy--generic-jump "^\\*+" nil avy-style)))))

;; * Babel settings
;; enable prompt-free code running
(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil)

;; register languages in org-mode
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
   (fortran . t)
   (C . t)))

;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)

(setq org-babel-default-header-args:python
      '((:results . "output replace")
	(:session . "none")
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))

(setq org-startup-with-inline-images "inlineimages")

;; default with images open
(setq org-startup-with-inline-images "inlineimages")

;; default width
(setq org-image-actual-width nil)
;; redisplay figures when you run a block so they are always current.
(add-hook 'org-babel-after-execute-hook
	  'org-display-inline-images)

;; This automatically aligns tables, which is nice if you use code to generate
;; tables.
(defun scimax-align-result-table ()
  "Align tables in the subtree."
  (save-restriction
    (save-excursion
      (unless (org-before-first-heading-p) (org-narrow-to-subtree))
      (org-element-map (org-element-parse-buffer) 'table
	(lambda (tbl)
	  (goto-char (org-element-property :post-affiliated tbl))
	  (org-table-align))))))

(add-hook 'org-babel-after-execute-hook
	  'scimax-align-result-table)

;; * Markup commands for org-mode


(defun org-markup-region-or-point (type beginning-marker end-marker)
  "Apply the markup TYPE with BEGINNING-MARKER and END-MARKER to region, word or point.
This is a generic function used to apply markups. It is mostly
the same for the markups, but there are some special cases for
subscripts and superscripts."
  (cond
   ;; We have an active region we want to apply
   ((region-active-p)
    (let* ((bounds (list (region-beginning) (region-end)))
	   (start (apply 'min bounds))
	   (end (apply 'max bounds))
	   (lines))
      (unless (memq type '(subscript superscript))
	(save-excursion
	  (goto-char start)
	  (unless (looking-at " \\|\\<")
	    (backward-word)
	    (setq start (point)))
	  (goto-char end)
	  (unless (or (looking-at " \\|\\>")
		      (looking-back "\\>" 1))
	    (forward-word)
	    (setq end (point)))))
      (setq lines
	    (s-join "\n" (mapcar
			  (lambda (s)
			    (if (not (string= (s-trim s) ""))
				(concat beginning-marker
					(s-trim s)
					end-marker)
			      s))
			  (split-string
			   (buffer-substring start end) "\n"))))
      (setf (buffer-substring start end) lines)
      (forward-char (length lines))))
   ;; We are on a word with no region selected
   ((thing-at-point 'word)
    (cond
     ;; beginning of a word
     ((looking-back " " 1)
      (insert beginning-marker)
      (re-search-forward "\\>")
      (insert end-marker))
     ;; end of a word
     ((looking-back "\\>" 1)
      (insert (concat beginning-marker end-marker))
      (backward-char (length end-marker)))
     ;; not at start or end so we just sub/sup the character at point
     ((memq type '(subscript superscript))
      (insert beginning-marker)
      (forward-char (- (length beginning-marker) 1))
      (insert end-marker))
     ;; somewhere else in a word and handled sub/sup. mark up the
     ;; whole word.
     (t
      (re-search-backward "\\<")
      (insert beginning-marker)
      (re-search-forward "\\>")
      (insert end-marker))))
   ;; not at a word or region insert markers and put point between
   ;; them.
   (t
    (insert (concat beginning-marker end-marker))
    (backward-char (length end-marker)))))


(defun org-italics-region-or-point ()
  "Italicize the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'italics "/" "/"))


(defun org-bold-region-or-point ()
  "Bold the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'bold "*" "*"))


(defun org-underline-region-or-point ()
  "Underline the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'underline "_" "_"))


(defun org-code-region-or-point ()
  "Mark the region, word or character at point as code.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'underline "~" "~"))


(defun org-verbatim-region-or-point ()
  "Mark the region, word or character at point as verbatim.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'underline "=" "="))


(defun org-strikethrough-region-or-point ()
  "Mark the region, word or character at point as strikethrough.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'strikethrough "+" "+"))


(defun org-subscript-region-or-point ()
  "Mark the region, word or character at point as a subscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'subscript "_{" "}"))


(defun org-superscript-region-or-point ()
  "Mark the region, word or character at point as superscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'superscript "^{" "}"))

;; * New org links

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "pydoc"
     :follow (lambda (path)
	       (pydoc path))
     :export (lambda (path desc format)
	       "Generate a url"
	       (let (url)
		 (setq url (cond
			    ((s-starts-with? "scipy" path)
			     (format
			      "https://docs.scipy.org/doc/scipy/reference/generated/%s.html"
			      path))
			    ((s-starts-with? "numpy" path)
			     (format
			      "https://docs.scipy.org/doc/numpy/reference/generated/%s.html"
			      path))
			    (t
			     (format
			      "https://www.google.com/#safe=off&q=%s"
			      path))))


		 (cond
		  ((eq format 'md)
		   (format "[%s](%s)" (or desc path) url))))))
  (org-add-link-type
   "pydoc"
   (lambda (path)
     (pydoc path))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "attachfile"
     :follow (lambda (link-string) (org-open-file link-string))
     :export (lambda (keyword desc format)
	       (cond
		((eq format 'html) (format ""))	; no output for html
		((eq format 'latex)
		 ;; write out the latex command
		 (format "\\attachfile{%s}" keyword)))))

  (org-add-link-type
   "attachfile"
   (lambda (link-string) (org-open-file link-string))
   ;; formatting
   (lambda (keyword desc format)
     (cond
      ((eq format 'html) (format ""))	; no output for html
      ((eq format 'latex)
       ;; write out the latex command
       (format "\\attachfile{%s}" keyword))))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "altmetric"
     :follow (lambda (doi)
	       (browse-url (format  "http://dx.doi.org/%s" doi)))
     :export (lambda (keyword desc format)
	       (cond
		((eq format 'html)
		 (format "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>
<div data-badge-type='medium-donut' class='altmetric-embed' data-badge-details='right' data-doi='%s'></div>" keyword))
		((eq format 'latex)
		 ""))))

  (org-add-link-type
   "altmetric"
   (lambda (doi)
     (browse-url (format  "http://dx.doi.org/%s" doi)))
   (lambda (keyword desc format)
     (cond
      ((eq format 'html)
       (format "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>
<div data-badge-type='medium-donut' class='altmetric-embed' data-badge-details='right' data-doi='%s'></div>" keyword))
      ((eq format 'latex)
       "")))))


(defun org-man-store-link ()
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    (let* ((page (save-excursion
		   (goto-char (point-min))
		   (re-search-forward " ")
		   (buffer-substring (point-min) (point))))
	   (link (concat "man:" page))
	   (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "man"
     :follow (lambda (path)
	       (man path))
     :store 'org-man-store-link))


;; * ivy navigation
(defun ivy-org-jump-to-visible-headline ()
  "Jump to visible headline in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style)))


(defun ivy-jump-to-visible-sentence ()
  "Jump to visible sentence in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy--generic-jump (sentence-end) nil avy-style))
  (forward-sentence))


(defun ivy-org-jump-to-heading ()
  "Jump to heading in the current buffer."
  (interactive)
  (let ((headlines '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      ;; this matches org headings in elisp too.
	      "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$"  nil t)
	(cl-pushnew (list
		     (format "%-80s"
			     (match-string 0))
		     (cons 'position (match-beginning 0)))
		    headlines)))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(goto-char (cdr (assoc 'position candidate)))
			(outline-show-entry)))))


(defun ivy-org-jump-to-agenda-heading ()
  "Jump to a heading in an agenda file."
  (interactive)
  (let ((headlines '()))
    ;; these files should be open already since they are agenda files.
    (loop for file in (org-agenda-files) do
	  (with-current-buffer (find-file-noselect file)
	    (save-excursion
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(cl-pushnew (list
			     (format "%-80s (%s)"
				     (match-string 0)
				     (file-name-nondirectory file))
			     :file file
			     :position (match-beginning 0))
			    headlines)))))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(find-file (plist-get (cdr candidate) :file))
			(goto-char (plist-get (cdr candidate) :position))
			(outline-show-entry)))))


(defun ivy-org-jump-to-heading-in-files (files &optional fontify)
  "Jump to org heading in FILES.
Optional FONTIFY colors the headlines. It might slow things down
a lot with large numbers of org-files or long org-files. This
function does not open the files."
  (let ((headlines '()))
    (loop for file in files do
	  (when (file-exists-p file)
	    (with-temp-buffer
	      (insert-file-contents file)
	      (when fontify
		(org-mode)
		(font-lock-fontify-buffer))
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(cl-pushnew (list
			     (format "%-80s (%s)"
				     (match-string 0)
				     (file-name-nondirectory file))
			     :file file
			     :position (match-beginning 0))
			    headlines)))))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(find-file (plist-get (cdr candidate) :file))
			(goto-char (plist-get (cdr candidate) :position))
			(outline-show-entry)))))


(defun ivy-org-jump-to-heading-in-directory (&optional recursive)
  "Jump to heading in an org file in the current directory.
Use a prefix arg to make it RECURSIVE.
Use a double prefix to make it recursive and fontified."
  (interactive "P")
  (let ((fontify nil))
    (when (equal recursive '(16))
      (setq fontify t))
    (ivy-org-jump-to-heading-in-files
     (f-entries "."
		(lambda (f)
		  (and
		   (f-ext? f "org")
		   (not (s-contains? "#" f))))
		recursive)
     fontify)))


(defun ivy-org-jump-to-project-headline (&optional fontify)
  "Jump to a headline in an org-file in the current project.
The project is defined by projectile. Use a prefix arg FONTIFY
for colored headlines."
  (interactive "P")
  (ivy-org-jump-to-heading-in-files
   (mapcar
    (lambda (f) (expand-file-name f (projectile-project-root)))
    (-filter (lambda (f)
	       (and
		(f-ext? f "org")
		(not (s-contains? "#" f))))
	     (projectile-current-project-files)))
   fontify))


(defun ivy-org-jump-to-open-headline (&optional fontify)
  "Jump to a headline in an open org-file.
Use a prefix arg FONTIFY for colored headlines."
  (interactive "P")
  (ivy-org-jump-to-heading-in-files
   (mapcar 'buffer-file-name
	   (-filter (lambda (b)
		      (-when-let (f (buffer-file-name b))
			(f-ext? f "org")))
		    (buffer-list)))
   fontify))


(defun ivy-org-jump-to-recent-headline (&optional fontify)
  "Jump to a headline in an org-file in `recentf-list'."
  (interactive)
  (ivy-org-jump-to-heading-in-files
   (-filter (lambda (f) (f-ext? f "org")) recentf-list)
   fontify))


(defcustom scimax-ivy-jump-functions
  '((heading . ivy-org-jump-to-heading)
    (visible . ivy-org-jump-to-visible-headline)
    (sentence . ivy-jump-to-visible-sentence)
    (recent-org ivy-org-jump-to-recent-headline)
    (directory . ivy-org-jump-to-heading-in-directory)
    (project . ivy-org-jump-to-project-headline )
    (agenda ivy-org-jump-to-agenda-heading))
  "alist of jump functions. The first one is the default.")


(defun ivy-org-jump (&optional arg)
  "Jump to a location in org file. The default is the first entry
in `scimax-ivy-jump-functions'. With a prefix arg, you can choose
the scope."
  (interactive "P")
  (let ((jumpfn (if arg (cdr (assoc (intern-soft (ivy-read "Scope: " scimax-ivy-jump-functions)) scimax-ivy-jump-functions))
		  ;; the default choice.
		  (cdr (car scimax-ivy-jump-functions)))))
    (funcall jumpfn)))

;; * A better return

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
	  (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes - add new or delete empty
     ((org-at-item-checkbox-p)
      (cond
       ;; at the end of a line.
       ((and (eolp)
	     (not (eq 'item (car (org-element-context)))))
	(org-insert-todo-heading nil))
       ;; no content, delete
       ((and (eolp) (eq 'item (car (org-element-context))))
	(setf (buffer-substring (line-beginning-position) (point)) ""))
       ((eq 'paragraph (car (org-element-context)))
	(goto-char (org-element-property :end (org-element-context)))
	(org-insert-todo-heading nil))
       (t
	(org-return))))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (cond
       ;; empty definition list
       ((and (looking-at " ::")
	     (looking-back "- " 3))
	(beginning-of-line)
	(delete-region (line-beginning-position) (line-end-position)))
       ;; empty item
       ((and (looking-at "$")
	     (looking-back "- " 3))
	(beginning-of-line)
	(delete-region (line-beginning-position) (line-end-position)))
       ;; numbered list
       ((and (looking-at "$")
	     (looking-back "[0-9]+. " (line-beginning-position)))
	(beginning-of-line)
	(delete-region (line-beginning-position) (line-end-position)))
       ;; insert new item
       (t
	(end-of-line)
	(org-insert-item))))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
	  (progn
	    ;; Go to end of subtree suggested by Pablo GG on Disqus post.
	    (org-end-of-subtree)
	    (org-insert-heading-respect-content)
	    (outline-show-entry))
	;; The heading was empty, so we delete it
	(beginning-of-line)
	(setf (buffer-substring
	       (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
	   (lambda (x) (not (string= "" x)))
	   (nth
	    (- (org-table-current-dline) 1)
	    (remove 'hline (org-table-to-lisp))))
	  (org-return)
	;; empty row
	(beginning-of-line)
	(setf (buffer-substring
	       (line-beginning-position) (line-end-position)) "")
	(org-return)))

     ;; fall-through case
     (t
      (org-return)))))


(defcustom scimax-return-dwim t
  "When t redefine the Ret behavior to add items, headings and table rows."
  :group 'scimax)


(when scimax-return-dwim
  (define-key org-mode-map (kbd "RET")
    'scimax/org-return))

  (use-package scimax-yas
    :init (setq scimax-snippet-dir (sr/fun/emacs-dir "snippets"))
    :straight (scimax-yas :local-repo "scimax-subset" :files ("scimax-yas.el")))
(require 'scimax-yas)

  (use-package scimax-ivy
    :after scimax-org
    :straight (scimax-ivy :local-repo "scimax-subset" :files ("scimax-ivy.el")))
(require 'scimax-ivy)

  (use-package scimax-apps
    :after scimax-org
    :straight (scimax-apps :local-repo "scimax-subset" :files ("scimax-apps.el")))
(require 'scimax-apps)

(load-file (concat (sr/fun/emacs-dir "straight/repos/scimax-subset/scimax-hydras.el")))

;; (use-package scimax-hydra
;;   :straight (scimax-hydra :local-repo "scimax-subset" :files ("scimax-hydras.el"))
;;   :bind ("<f12>" . scimax/body))
;; (require 'scimax-hydras)

(use-package scimax-notebook
  :straight (scimax-notebook :local-repo "scimax-subset" :files ("scimax-notebook.el"))
  :bind ("M-s n" . 'nb-hydra/body)
  :config 
(setq nb-notebook-directory "~/my_projects/"))
;; (global-set-key (kbd "M-s n") 'nb-hydra/body))
(require 'scimax-notebook)

(use-package scimax-journal
  :init (setq scimax-journal-root-dir "~/my_org/journal/")
  :bind ("H-j" . scimax-journal/body)
  :straight (scimax-journal :local-repo "scimax-subset" :files ("scimax-journal.el")))
(require 'scimax-journal)

  (use-package scimax-utils
    :straight (scimax-utils :local-repo "scimax-subset" :files ("scimax-utils.el" )))
(require 'scimax-utils)

    (use-package ob-ipython
      :straight (ob-ipython :host github :repo "gregsexton/ob-ipython" :files ("client.py" "ob-ipython.el"))
      :config
      (require 'ob-ipython))

  ;;; * Applying John's customisations and monkeypatches 
  ;;; These are related to ipython kernel management
  ;;; * These  are related mostly to org-babel customisations
    (use-package scimax-ob
      :straight (scimax-ob :local-repo "scimax-subset" :files ("scimax-ob.el")))

    (use-package scimax-org-babel-ipython-upstream
          :straight (scimax-org-babel-ipython-upstream  :local-repo "scimax-subset" :files ("scimax-org-babel-ipython-upstream.el")))

(use-package ess
  :ensure t
  :config
  (require 'ess)
  (show-paren-mode)
  (use-package ess-R-data-view)
  (use-package polymode)
  (setq ess-describe-at-point-method nil)
  (setq ess-switch-to-end-of-proc-buffer t)
  (setq ess-rutils-keys +1)
  (setq ess-eval-visibly 'nil)
  (setq ess-use-flymake "lintr::default_linters()")
   (setq ess-use-company t)
  (setq ess-history-file "~/.Rhistory")
  (setq ess-use-ido t)
  (setq ess-roxy-hide-show-p t)
  ;;(speedbar-add-supported-extension ".R")
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)

;;setting up eldoc
(setq ess-use-eldoc t)
(setq ess-eldoc-show-on-symbol t)
(setq ess-doc-abbreviation-style 'aggresive)
  )

;; The following chunk is taken from: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/ess/packages.el
;;; Follow Hadley Wickham's R style guide
(setq ess-first-continued-statement-offset 2
      ess-continued-statement-offset 4
      ess-expression-offset 4
      ess-nuke-trailing-whitespace-p t
      ess-default-style 'DEFAULT)


;; Adding Poly-R package

(use-package poly-R
  :ensure t
  )
;; The following chunk is taken from antonio's answer from https://stackoverflow.com/questions/16172345/how-can-i-use-emacs-ess-mode-with-r-markdown
(defun rmd-mode ()
  "ESS Markdown mode for rmd files."
  (interactive)
  (require 'poly-R)
  (require 'poly-markdown)
  (poly-markdown+r-mode))

(use-package ess-view
  :ensure t
  :config
  (require 'ess-view)
  (if (system-type-is-darwin)
      (setq ess-view--spreadsheet-program
            "/Applications/Tad.app/Contents/MacOS/Tad"
            )
    )
  (if (system-type-is-gnu)
      (setq ess-view--spreadsheet-program
            "tad"
            )
    )
  )

;; This is taken and slightly modified from the ESS manual
;; The display config is similar to that of Rstudio

(setq display-buffer-alist
      `(("*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.33)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.35)
         (reusable-frames . nil))
        ("*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.33)
         (reusable-frames . nil))))

(message "Loaded ESS configuration")

(straight-use-package 'flycheck)

(use-package blacken
:straight t
:hook (python-mode . blacken-mode))

(defun episteme-search ()
  (interactive)
  (helm-do-ag (sr/fun/project-dir "episteme/brain"))
  (let* ((p (point))
         (f (org-brain-first-headline-position))
         (adjusted-point (max 0 (- p f))))
    (org-brain-visualize (file-name-sans-extension (buffer-name)))
    (with-current-buffer "*org-brain*"
      (let ((minmax (polybrain--get-point-min-max)))
        (goto-char (+ (car minmax) adjusted-point))))))(require 'f)

(defun sr/fun/proj-search ()
  (interactive)
  (helm-do-ag (sr/fun/project-dir "")))

(defun sr/fun/org-search ()
  (interactive)
  (helm-do-ag (sr/fun/org-dir "")))

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
