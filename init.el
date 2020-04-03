;;; -*- lexical-binding: t -*-
(setq-default lexical-binding t)

(setq gc-cons-threshold 50000000)

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

;; Base function to create the home directory
(defun sr/fun/homedir (foldername)
"Function to extract the home directory path"
  (expand-file-name foldername (getenv "HOME")))

;; Emacs directory defauling to .emacs.d
(defun sr/fun/emacs-dir (foldername)
"Function to prepend the project directory path to any folder. Starts from the home directory."
  (expand-file-name foldername (sr/fun/homedir ".emacs.d" )))

(fset 'yes-or-no-p 'y-or-n-p)

(load (sr/fun/emacs-dir "dotemacs.el"))
