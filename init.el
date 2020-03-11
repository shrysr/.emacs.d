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

(use-package org)
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))
