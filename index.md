
# Table of Contents

1.  [Introduction](#org200c1c8)
    1.  [Overall setup](#org2ba214e)
    2.  [Inspirations (and other literate configurations)](#orge79766a)
    3.  [Packages I've found very useful](#org242f5e4)
2.  [Init setup](#org1a6541a)
    1.  [Lexical Scope and binding](#orgb55b4a6)
    2.  [Garbage collection](#org8c9f409)
    3.  [Package management](#orgc223006)
        1.  [Straight](#org7c0eb9d)
        2.  [Use-package integration with straight](#org53b472d)
    4.  [Some basic directory definitions](#org9adc7d2)
    5.  [Shorten yes or no](#org1c41a51)
    6.  [Load main config](#org50917ef)
3.  [.gitignore](#org97ff03a)
4.  [Tangle Emacs config on save](#orgd49bf51)
    1.  [Tangle on save without async](#org4109489)
    2.  [Local file variables](#org5fa2220)
    3.  [Async function to tangle org file on save.](#orga784a7f)
5.  [Various directories](#org2241fe9)
6.  [Auto-save](#org91c5853)
7.  [OS Level variables](#org660b04e)
8.  [Org-mode related](#orgd8658ae)
    1.  [Installing org and org plus contrib via straight](#org4dba606)
        1.  [Old](#org11eb847)
    2.  [Collection of hooks for org mode](#org40ccee0)
    3.  [Exports](#org861cc80)
        1.  [Markdown export](#org8d33549)
        2.  [ox-pandoc](#orge6a0a68)
    4.  [Agenda mechanics](#orgcd2de12)
        1.  [Weekday starts on Monday](#orgfcacd16)
        2.  [Display heading tags farther to the right](#orgecf2916)
        3.  [Default org directory and agenda file directory](#org710edbf)
        4.  [Include gpg files in agenda generation](#org60e0e65)
        5.  [Enable default fuzzy search like in google](#org4202148)
        6.  [Hooks for org-agenda](#org2cd8e15)
        7.  [org-habit](#org0104460)
    5.  [Archiving mechanics](#org0db4408)
    6.  [Capture mechanics](#org1727cc7)
        1.  [Doct for org capture templates](#orgba1194f)
        2.  [Closing org-capture frame on abort](#org7fe550c)
        3.  [Controlling org-capture buffers](#org31d71a2)
    7.  [Refile mechanics](#org6430df7)
        1.  [Refile target level](#orgc510a0d)
        2.  [General refiling settings](#org32dd3e0)
    8.  [Clocking mechanics](#org9647347)
        1.  [Continuous clocking + punch in/out approach](#orgcee473e)
        2.  [set idle timer for clocked task](#org5c4487a)
        3.  [No zero clocks](#org325e31f)
        4.  [Clocking accuracy](#org582fe19)
        5.  [org-mru-clock](#orgfb8e2f8)
        6.  [counsel-org-clock](#org46d76de)
    9.  [Task state sequences](#org67c8158)
    10. [org-source-window split setup](#org2b89661)
    11. [Log done](#org6a5d7ac)
    12. [Shortcuts (to be replaced via hydra)](#org15ef2ec)
    13. [org-ql](#org470fda9)
    14. [Org git link](#orgcb36a70)
        1.  [Note](#org70928ff)
        2.  [script](#org948ae16)
9.  [Temporary package list](#org6f37671)
10. [Org journal](#org7d496ff)
    1.  [Base config](#org912c1a8)
    2.  [org-capture template for Journal](#org50d5335)
    3.  [Figure out easy encryption approach for org journal](#org0ca10db)
11. [Crypto](#orgcbd2136)
    1.  [Basic crypto](#orgfdd3861)
    2.  [TEST org-crypt](#org4c31e71)
    3.  [Setting auth sources](#org444343a)
12. [git related](#org2fdd416)
    1.  [Git gutter](#org64c6563)
    2.  [magit settings](#org8898810)
    3.  [Time machine for git](#orge676110)
13. [PDF related](#org184cb21)
    1.  [STABLE PDF Tools](#org8de6482)
    2.  [org-noter](#org671e899)
14. [Window, frame and buffer management](#org0d5b036)
    1.  [winum](#orgfe26791)
    2.  [Winner mode](#org818ee93)
    3.  [TEST eyebrowse](#orgb6e262e)
    4.  [Bufler](#org5102334)
    5.  [Frame settings](#orgcd94981)
    6.  [crux](#org5388a15)
15. [Emacs information](#org774b50c)
    1.  [which key](#org5654985)
16. [Project management](#orgdeed6f3)
    1.  [org-projectile](#org9434cb7)
    2.  [projectile](#org38c3c6d)
17. [Knowledge management](#orgf90f750)
    1.  [org-brain](#org76559d7)
        1.  [Main configuration](#orge59e439)
        2.  [org-cliplink function](#org695820b)
        3.  [Chronological entries](#orgc004d5e)
        4.  [Navigation Helpers](#org4481d16)
    2.  [define-word](#org672df1c)
    3.  [Wiktionary lookup](#org5e7ce82)
    4.  [merriam-webster](#org1540975)
    5.  [pinboard <code>[2/2]</code>](#orga7ffac3)
    6.  [org-web-tools](#orgf314593)
    7.  [org-download](#org76700d8)
    8.  [oddmuse-curl](#org16c76da)
    9.  [ox-oddmuse](#orgde7d093)
18. [Dired](#orgf58f710)
19. [Treemacs <code>[0/3]</code>](#org4087ff8)
20. [Selection, search, navigation and jumps](#org17baa89)
    1.  [smex](#org09ea995)
    2.  [Abbrev definitions](#orgc4ac0b9)
    3.  [Counsel](#org09b94a8)
    4.  [ripgrep](#orge11ea40)
    5.  [Expand region package](#orgd7617d3)
    6.  [Hippie Expand](#org60222d7)
    7.  [Browse kill ring](#orga4f20cd)
    8.  [Multiple Cursors](#orga48c531)
    9.  [Undo tree](#org746d387)
    10. [Setting undo limits](#org1a5c61d)
    11. [yasnippet and ivy-yasnippet](#org15402ff)
    12. [swiper](#orgff962e2)
    13. [avy](#orgbde671e)
    14. [Super and Hyper key setting](#orgc02df97)
    15. [Helm packages and functions](#org8850e25)
        1.  [Helm notes](#org8257a71)
        2.  [Setting helm for some basic functions](#org706a115)
        3.  [General settings for helm](#org787f10b)
        4.  [Helm bibtex and projectile](#org5082a3e)
        5.  [helm-ag](#org65629ab)
        6.  [helm-rg](#org5d4c28a)
        7.  [helm-org-rifle](#orgdaa1e61)
        8.  [Helm swoop](#org4882683)
    16. [Projectile and helm](#orgb099b8b)
    17. [Copy simple without breaks](#org8e9a49b)
21. [Email](#org005096c)
    1.  [mu4e via use-package](#orgdaf1c2a)
    2.  [TEST org-msg](#org0d04455)
        1.  [Basic setup](#orgdcddb88)
        2.  [Compose CSS](#org0da1476)
        3.  [Set the css to the above custom function](#org88a8d67)
22. [Programming customisations](#org8ef7fb0)
    1.  [Common editing modes](#orge383ee3)
    2.  [Electic pair mode only for specified modes](#org5c36641)
    3.  [flyspell with markdown](#orgc053148)
    4.  [eshell](#org07eca57)
        1.  [configuration](#org67ef3d9)
        2.  [Visual executables](#org76bc107)
        3.  [Aliases](#org49eee4a)
        4.  [Find file and edit](#org1234894)
        5.  [git repo](#orgd120843)
        6.  [Replacing $HOME with tilde](#org32daf7a)
        7.  [shorter directory names](#org2a8362f)
        8.  [break up directory into parent and base](#org1d825f6)
        9.  [virtual environments](#org3911e7d)
        10. [Tie up](#org2345eae)
        11. [eshell here](#orgab3c39d)
        12. [C-d behavior](#orge629719)
    5.  [shell-pop - ansi term](#org5a35b7a)
    6.  [orgit](#org17cbeb1)
    7.  [realgud](#orgc9d4446)
    8.  [emmet-mode](#orgb43850d)
    9.  [web-mode](#org9e85d8e)
    10. [ESS configuration](#org2da482b)
    11. [lispy](#org9489c0e)
    12. [flycheck](#org5462353)
    13. [Python <code>[0/1]</code>](#orgd062c95)
        1.  [EIN notebooks](#orgb12ae23)
        2.  [emacs-lsp](#org9e7a39a)
        3.  [pyvenv, pydoc and elpy](#org6cd1e89)
        4.  [Blacken](#orge983b92)
23. [Docker](#orgc633c00)
        1.  [Docker package](#org39a702c)
24. [Custom search functions <code>[0/1]</code>](#orgf1ce2fa)
    1.  [Episteme search](#orgdcd002a)
    2.  [Projects search <code>[1/6]</code>](#orgb597f61)
    3.  [Org files search](#org192403f)
    4.  [Journal files search](#org8f75890)
    5.  [Downloads and Desktop search](#org0c5856f)
25. [Pastes and gists](#orgd19d270)
    1.  [Webpaste](#org9b3700f)
    2.  [github gists](#org2722cab)
26. [Hydra Hera and nougat](#orgb189145)
    1.  [hera](#org4c8d2f0)
    2.  [nougat-hydra](#org4406392)
    3.  [hydra-dwim](#org2e6f137)
    4.  [hydra-default](#orgeeb6606)
    5.  [hydra-straight](#org01f4d91)
    6.  [hydra-mu4e](#orge66ce76)
    7.  [hydra-pyvenv](#org2e98931)
    8.  [hydra-window](#org97b6d01)
    9.  [hydra-treemacs](#org0969fe8)
    10. [hydra-journal](#org0ec9ad7)
    11. [hydra-clock](#org30903fb)
    12. [hydra-links](#org79adeb4)
    13. [hydra-brain](#org9b70c08)
    14. [hydra-functions](#org63a254f)
    15. [hydra-gists and pastes](#orgeee90d9)
27. [org-id setup](#orgd365422)
28. [eww](#org4b7128d)
        1.  [Default browser to be eww and basic settings](#org4275d5a)
        2.  [Keyboard map for default external browser](#org2b657eb)
        3.  [Wikipedia search](#org0893707)
        4.  [Access Hacker News](#orge0bb146)
        5.  [Youtube music playlist](#org3fab18a)
        6.  [Open specific browser depending on the URL](#orgf89831a)
29. [New Scimax port](#org8fa2b2e)
    1.  [org-ref](#org291eaa0)
    2.  [scimax-org port](#org1238e72)
        1.  [General Org mode related](#org0f55934)
        2.  [Babel settings](#org793a870)
        3.  [Org formatting functions](#org4845e54)
        4.  [Links and Jumping functions](#org6755e6e)
        5.  [Better return](#orgff79ca8)
        6.  [Numbered headings and overlays](#org5e9e270)
        7.  [PDF and EPS images in org mode](#orga4f0558)
    3.  [scimax-ivy](#org9766bc8)
    4.  [scimax-apps](#org856bcba)
    5.  [scimax-hydra](#orgc742d09)
    6.  [scimax-notebook](#orgc1d0e3a)
    7.  [scimax-ipython](#orga1d5510)
30. [Aesthetics](#orga5bc83d)
    1.  [Setting custom themes to be safe](#org2af97d9)
    2.  [Theme : modus](#orgad5550c)
        1.  [modus operandi](#org084eab3)
    3.  [Font and other aesthetics](#org9b12ed9)
        1.  [Fixed pitch and variable pitch fonts](#org71ffbc2)
        2.  [Faces for specific org elements](#org679f6ca)
    4.  [Fill column and auto fill](#orgcb3e1b9)
    5.  [Spaceline : modeline configuration](#org35d5ef7)
    6.  [Striking out Done headlines](#org814f7a9)
    7.  [keywords as boxes with inverted colors](#org46c60c7)
    8.  [Remove the bars and startup messages](#orgd18041e)
31. [Hugo](#orgd48c1a3)
    1.  [ox-hugo setup](#org6879fb0)
    2.  [Auto-generate some properties](#orgd4e436f)
32. [Loading secret config](#org329e7ec)



<a id="org200c1c8"></a>

# Introduction

I've been using my own configuration on top of the excellent Scimax for
the past many years. This is a reboot to reverse the bias, and create my
very own emacs configuration. It obviously includes massive snippets of
code from scimax, as well as other configurations I come across, not to
mention the excellent advice often received from #emacs and other
geniuses lurking in irc channels, and of course the vast
internet.

> Note that this config is still under a great deal of flux at the
> moment and will be for the foreseeable future. I am learning elisp
> every (other) day and am rather obsessed like most Emacsers to
> optimise the entire toolset. This is an an opinionated setup.
> 
> I would recommend studying the setup and using bits and pieces of it
> as you require, because one primary power of Emacs is being able to
> customise your tool to your needs. However, the eventual goal will
> also be to enable using this configuration as a whole for anybody
> interested.


<a id="org2ba214e"></a>

## Overall setup

This file is `emacs-config.org`. This is the source of truth for the
entire config. It is tangled into 2 files, an init.el and a dotemacs.el.

The init.el file is included in the git commit as it will (eventually)
enable starting up emacs with basic libraries, and loading (and if
necessary tangling) this config if one is starting from scratch on a new
machine.

Due to the above - it is important to be careful with using header
arguments for source blocks. As per current Org mode features,
specifying the header arguments, even a `:tangle yes`, will over-ride
the file settings specified elsewhere and thus parts of the config will
never get inserted into the el files that are loaded during init.

The better method to disable tangling of specific blocks would be to
comment out Org headings. It should also be possible to have a setup
wherein headings with specific tags are not tangled, similar to the
noexport tag option with typical org exports. The caveat of using this
option is that these headings will be neglected in an export to say,
something like a Hugo document.

Package management is done via the interesting straight.el. There are
other approaches like borg, using quelpa and so on that I have not
tried. However, the point is that it is better to rely on git to draw
packages rather than MELPA, which can be frustrating at times.

The last 2 headlines contain a 'Testing Area' and 'Disabled'
headline. This contains all the packages that are yet to be a part of my
main setup, or packages that I use occasionally and activate when I
need to. These are available in the raw org file in the repository of
the configuration.

General notes:

-   I was initially using a single init.org file that tangled to an
    init.el file. This kind of setup makes it hard to make meaningful
    commits, especially if the init.el is also a part of the visible
    commits. After all one caveat here is still that ~some external
    library is necessary to tangle these org files into elisp files for
    loading emacs.
-   I would prefer not using git hooks for the tangling because my commit
    workflow is still erratic. Sometimes, there is no change in code, but
    just some additional notes to the configuration.


<a id="orge79766a"></a>

## Inspirations (and other literate configurations)

This is a growing and non-comprehensive list of configurations that I
admire and have liberally copied from in constructing my own.

-   [Bernt Hansen's very detailed Org-mode config](http://doc.norang.ca/org-mode.html)
-   [Sacha Chua](http://pages.sachachua.com/.emacs.d/Sacha.html)
-   [Mathieu Marques](https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org)
-   [Lee Hinman](https://writequit.org/org/)
-   [Karl Voit](https://karl-voit.at/2017/06/03/emacs-org/)
-   [Dustin Lacewell](https://dustinlacewell.github.io/emacs.d/)
-   [GitHub - wasamasa/dotemacs: Literate Emacs configuration](https://github.com/wasamasa/dotemacs)
-   [GitHub - IvanMalison/dotfiles: Configuration files for XMonad, Emacs, NixOS, Taffybar and more.](https://github.com/IvanMalison/dotfiles)

In general - code and snippet source references are added as and when
possible though this is a tedius task for those with configurations
under a great rate of flux.


<a id="org242f5e4"></a>

## TODO Packages I've found very useful

Though I'm using a huge number of packages - this list reflects the
absolute core that I consider to be essential to my current *daily*
workflow. This list is being formulated so that I can develop a minimal
Emacs config that can be used for a rapid setup on a headless server.

1.  org-download
2.  org-web-tools
3.  org-ref
4.  treemacs
5.  eyebrowse
6.  projectile
7.  org-projectile
8.  counsel + ivy + swiper
9.  Helm (some functions)
10. magit
11. hydra (and other variants)
12. straight.el


<a id="org1a6541a"></a>

# Init setup

This part of the config has to be tangled to init.el so that straight
and use-package can be setup. This is a separate init file as it is
expected to remain stable and will be the only .el file that is in git
commit.


<a id="orgb55b4a6"></a>

## Lexical Scope and binding

A reasonable explanation of the importance of lexical binding is available [in the elisp manual](elisp#Lexical%20Binding). This is essentially similar to the quoted or unquoted variables in R.

> A lexically-bound variable has lexical scope, meaning that any reference to the variable must be located textually within the binding
> construct.

    ;;; -*- lexical-binding: t -*-
    (setq-default lexical-binding t)


<a id="org8c9f409"></a>

## Garbage collection

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-09 Thu 18:09] </span></span>   
    As the doom emacs FAQ states - the GC collection has to be set back to
    normal as well.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-02 Thu 22:11] </span></span>   
    Just increasing teh gc threshold seems to have reduced my init time by
    50% ! From ~7 seconds to 3.5s at the moment. This is found from
    (emacs-init-time). This is probably the fastest init time I have ever
    had.

References:

-   [dotemacs/init.org at master · wasamasa/dotemacs · GitHub](https://github.com/wasamasa/dotemacs/blob/master/init.org#memory-management)
-   [doom-emacs/faq.org  · hlissner/doom-emacs · GitHub](https://github.com/hlissner/doom-emacs/blob/665b627b7c07c8d29ec8d334588cecc2ba308248/docs/faq.org#how-does-doom-start-up-so-quickly)

-   [ ] There are additional options that can be tried on this
    subject. Like garbage collection when focus is lost from
    Emacs. The doom docs claim this is done via the gcmh package.

    (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
          gc-cons-percentage 0.6)
    
    (add-hook 'emacs-startup-hook
      (lambda ()
        (setq gc-cons-threshold 16777216 ; 16mb
              gc-cons-percentage 0.1)))
    
    ;; Raising gc-cons threshold when the minibuffer is active
    
    (defun doom-defer-garbage-collection-h ()
      (setq gc-cons-threshold most-positive-fixnum))
    
    (defun doom-restore-garbage-collection-h ()
      ;; Defer it so that commands launched immediately after will enjoy the
      ;; benefits.
      (run-at-time
       1 nil (lambda () (setq gc-cons-threshold 16777216 ; 16mb
              gc-cons-percentage 0.1) )))
    
    (add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
    (add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)


<a id="orgc223006"></a>

## Package management


<a id="org7c0eb9d"></a>

### Straight

This snippet essentially bootstraps straight.el, which has several advantages over use-package, along with the ability to seamlessly work with use-package as well.

By bootsrapping, this means that the straight package is downloaded to the user's emacs directory and compiled and installed. Unless set otherwise, the user's emacs directory is `~/.emacs.d`

     (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))


<a id="org53b472d"></a>

### Use-package integration with straight

    (setq straight-use-package-by-default t)
    (straight-use-package 'use-package)
    (use-package git) ;; ensure we can install from git sources


<a id="org9adc7d2"></a>

## Some basic directory definitions

    ;; Base function to create the home directory
    (defun sr/fun/homedir (foldername)
    "Function to extract the home directory path"
      (expand-file-name foldername (getenv "HOME")))
    
    ;; Emacs directory defauling to .emacs.d
    (defun sr/fun/emacs-dir (foldername)
    "Function to prepend the project directory path to any folder. Starts from the home directory."
      (expand-file-name foldername (sr/fun/homedir ".emacs.d" )))


<a id="org1c41a51"></a>

## Shorten yes or no

It is infuriating that this is not a default in emacs. Therefore this minor snippet is included in the init.

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org50917ef"></a>

## Load main config

    (load (sr/fun/emacs-dir "dotemacs.el"))


<a id="org97ff03a"></a>

# .gitignore

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-26 Thu 11:38] </span></span>   
    In this process, I realised that as long as there is a .gitignore file present (and not in a commit) and the specified files have never been in a commit - git automatically ignores these files. i.e there is no need to commit a .gitignore file.

    auto-save-list
    autosaves
    elpa
    eshell
    recentf
    smex-items
    sr-secrets.org.el
    projectile-bookmarks.eld
    bookmarks
    ac-comphist.dat
    .mc-lists.el
    transient
    elpy
    tramp
    url
    dotemacs.el
    org-journal.cache
    request/
    .cache/
    var/
    .lsp-session*


<a id="orgd49bf51"></a>

# Tangle Emacs config on save


<a id="org4109489"></a>

## TODO Tangle on save without async

As such the tangling hardly takes any time. [Literate Emacs Configuration | Sean Miller: The Wandering Coder](https://thewanderingcoder.com/2015/02/literate-emacs-configuration/) provides an example of setting up a function. This uses the buffer file name to tangle for the emacs config file. A hook is added to the save function to check.

-   [ ] Add a force tangle option if files do not exist. This is because, if for troubleshooting purposes, the el files are deleted, and there is no change in the org file, then the tangling does not take place at all. In general, it may be better to ensure the el files are deleted and tangled again.

    (defun sr/fun/tangle-on-save-init ()
    (when (string= buffer-file-name (file-truename "~/.emacs.d/emacs-config.org"))
    (org-babel-tangle)))
    
    (add-hook 'after-save-hook 'sr/fun/tangle-on-save-init)


<a id="org5fa2220"></a>

## Local file variables

One way to do this is via local file variables, adding the following to the init file (or any file). However, it seems that this is not 'activated' by default.

    # Local variables:
    # eval: (add-hook 'after-save-hook (lambda () (org-babel-tangle)) t t)
    # end:


<a id="orga784a7f"></a>

## TODO Async function to tangle org file on save.

This is inspired from [Asynchronous tangle and compile of config.org(question/issue) : emacs](https://www.reddit.com/r/emacs/comments/5ej8by/asynchronous_tangle_and_compile_of_configorg/) on reddit and a work in progress. Since I am using straight.el, the byte compilation of packages is not necessary (or already taken care of). It is probably worth noting that the tangling process is almost instant and maybe this effort is not warranted.

(sr/fun/async-tangle-init)

    (defun sr/fun/async-tangle-init ()
      (async-start
       (lambda ()
         (org-babel-tangle))
    (message "Tangle async done")))


<a id="org2241fe9"></a>

# Various directories

    
    
    (defun sr/fun/project-dir (foldername)
    "Function to prepend the project directory path to any folder. Starts from the home directory."
      (expand-file-name foldername (sr/fun/homedir "my_projects" )))
    
    (defun sr/fun/org-dir (foldername)
    "Function to prepend the org directory path to any folder. Starts from the home directory."
      (expand-file-name foldername (sr/fun/homedir "my_org" )))


<a id="org91c5853"></a>

# Auto-save

Copied from ldleworth's config. I think this makes sense for me at the moment. Here is a summary:

-   Setup auto-save for every file that is visited.
-   Set the auto-save directory explicitly to save all the auto-saves in a single location.
    -   The directory will be created if not available, and will be ignored for
        git.
-   Use the autosave directory for backups as well.
-   [ ] Save every <del>20</del> 60 seconds (experiment with the time frame)
    -   This causes too much lag and has been disabled.
-   [ ] Backup on each save.
    -   [ ] This uses a package. I am not sure whether this is necessary.
-   Backup files even if version controlled
-   [ ] Copy files to avoid various problems.
    -   [ ] check whether this causes any lag with operating emacs.
-   keep 10 versions of old backups and delete old backups.

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


<a id="org660b04e"></a>

# OS Level variables

Since I switch between a Linux machine and a Mac frequently, it is better to define variables that can be used to set other variables depending on the OS.

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


<a id="orgd8658ae"></a>

# Org-mode related

These have packages and settings that are mostly related to org-mode
though there may be other settings that bleed in. org-babel has been
given it's own section though it is org-mode related.


<a id="org4dba606"></a>

## Installing org and org plus contrib via straight


<a id="org11eb847"></a>

### Old

Reference: [Crookster's blog post](https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/)

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


<a id="org40ccee0"></a>

## Collection of hooks for org mode

This is intended to be a collection of hooks loaded after org mode. It may be more convenient to add such hooks in the package configurations since the hooks will not work if the package is not available.

However, hooks have the potential to slow down search, opening multiple files like in org-agenda, tramp files and so on. Therefore, the idea is to try collect the hooks here and include logic to discard hooks if the mode or package is not installed.

-   [ ] Maybe work on a method to switch off all the hooks after org mode since this mode is being used extensively.

List of hooks

-   [ ] Org indent mode
-   [ ] Flyspell mode
-   [ ] 

    ;; Indent by header level
    
    (with-eval-after-load 'org
       (add-hook 'org-mode-hook #'org-indent-mode))
    
    ;; Enable flyspell mode
    
    (add-hook 'org-mode-hook 'flyspell-mode)


<a id="org861cc80"></a>

## Exports


<a id="org8d33549"></a>

### Markdown export

    (require 'ox-md)


<a id="orge6a0a68"></a>

### ox-pandoc

    (use-package ox-pandoc
      :ensure t
      :straight t
      :defer 5)


<a id="orgcd2de12"></a>

## TODO Agenda mechanics


<a id="orgfcacd16"></a>

### Weekday starts on Monday

    (setq org-agenda-start-on-weekday nil)


<a id="orgecf2916"></a>

### Display heading tags farther to the right

    (setq org-agenda-tags-column -130)


<a id="org710edbf"></a>

### Default org directory and agenda file directory

    (setq
     org-directory "~/my_org/"
     org-agenda-files '("~/my_org/"))


<a id="org60e0e65"></a>

### Include gpg files in agenda generation

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-05-14 Thu 23:12] </span></span>   
    This is tested to work as expected. Active date entries from the gpg
    files are being included.

Source: <https://emacs.stackexchange.com/questions/36542/include-org-gpg-files-in-org-agenda>
Note that this must be set first and then the agenda files specified.

    (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
      (setq org-agenda-file-regexp
            (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                      org-agenda-file-regexp)))
    
    ;;(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")


<a id="org4202148"></a>

### Enable default fuzzy search like in google

    (setq org-agenda-search-view-always-boolean t)


<a id="org2cd8e15"></a>

### Hooks for org-agenda

    (add-hook 'org-agenda-mode-hook
              '(lambda ()
     	     (hl-line-mode 1)))


<a id="org0104460"></a>

### DONE org-habit

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 13:20] </span></span>   
    Adding a require has brought org-habit back on track.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:50] </span></span>   
    Appears the use-package config for org-habit is not correct and there is some issue in downloading it as a package.

I want to shift the org habit graph in the agenda further out right so as to leave enough room for the headings to be visible.

    (require 'org-habit)
    (setq org-habit-graph-column 90)
    ;; Setting this startup with nil so that it can be toggled off.
    ;; This seems to speed up agenda slightly.
    ;; toggle habits using the 'K' in org agenda
    (setq org-habit-toggle-habits nil)


<a id="org0db4408"></a>

## Archiving mechanics

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-06 Mon 23:27] </span></span>   
    I prefer to keep my archived files in a separate folder to promote a
    cleaner look and less files in the main org directory. The earlier
    archive file used to replicate the structure of the file where the entry
    was archived from. However, I have realised that the properties of
    archived entries provide all the information that I would need from an
    archived file.
    
    Projects may require a separate approach. Perhaps archived subtrees
    would help in that case. For general GTD based workflows, the simple
    approach of archiving under a 'Archive' heading seems sufficient. This
    will also mark the difference between using this approach and the
    earlier complete replication.

    (setq org-archive-mark-done nil)
    (setq org-archive-location (sr/fun/org-dir "archive/%s_archive::* Archive"))


<a id="org1727cc7"></a>

## TODO Capture mechanics

Over time I've found an efficient capture mechanism to be important to
nurture productivity while accounting for interruptions. I think the
process of gathering templates takes time, and can be facilitated by
keenly observing the typical repetitive capture-like tasks that are
performed through the day. Needless to say, this has to be integrated
into GTD. The ideal method seems to be to capture quickly ~somewhere and
then review and refile, as well as schedule a time block for the
tasks. Musa Al-hassy's [A Life Configuring Emacs](https://alhassy.github.io/init/) talks about the
above. Some other good starting points are:

-   [Bernt Hansen's config](http://doc.norang.ca/org-mode.html)
-   [org | Pragmatic Emacs](http://pragmaticemacs.com/category/org/)
-   [Sacha Chua's Emacs configuration](https://pages.sachachua.com/.emacs.d/Sacha.html)


<a id="orgba1194f"></a>

### TODO Doct for org capture templates

[DOCT](https://github.com/progfolio/doct) makes it a lot easier to define capture templates in a clean manner. At the moment, I am interested in adding hooks to specific functions and improving the entire capture process.

1.  Install doct

        (straight-use-package 'doct)

2.  doct functions

    -   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-12 Sun 11:42] </span></span>   
        It seems easier to prompt for a date with respect to active dates. The
        default date is today, and there requires just another RET to select
        today. The prompt ensures being able to cater to tasks that definitely
        belong tomorrow or another date down the line. Without a template, an
        additional refile, or schedule, or manipulation in the capture window
        is required. This approach seems to cater to more situations in a
        standard manner.
    
        (defun sr/fun/todo-act-today ()
        '("* %{todo-state} %?"
        ":PROPERTIES:"
        ":CREATED: %<%Y-%m-%d %a %H:%M>"
        ":PLANNED: %t"
        ":END:"))
        
        (defun sr/fun/todo-act-date ()
        '("* %{todo-state} %?"
        ":PROPERTIES:"
        ":CREATED: %<%Y-%m-%d %a %H:%M>"
        ":PLANNED: %^t"
        ":END:"))
        
        (defun sr/fun/todo-passive ()
        '("* %{todo-state} %?"
        ":PROPERTIES:"
        ":CREATED: %U"
        ":END:"))
        
        (defun sr/fun/todo-link-act-today ()
        '("* %{todo-state} %a"
        ":PROPERTIES:"
        ":CREATED: %<%Y-%m-%d %a %H:%M>"
        ":PLANNED: %t"
        ":END:"
        "%?"))
        
        (defun sr/fun/todo-link-act-date ()
        '("* %{todo-state} %a"
        ":PROPERTIES:"
        ":CREATED: %<%Y-%m-%d %a %H:%M>"
        ":PLANNED: %^t"
        ":END:"
        "%?"))
        
        (defun sr/fun/todo-file-link-act-date ()
        '("* %{todo-state} [[file:%F][%f]]"
        ":PROPERTIES:"
        ":CREATED: %<%Y-%m-%d %a %H:%M>"
        ":PLANNED: %^t"
        ":END:"
        "%?"))
        
        (defun sr/fun/todo-file-ext-link-act-date ()
        '("* %{todo-state} %(org-web-tools--get-url %x)"
        ":PROPERTIES:"
        ":CREATED: %<%Y-%m-%d %a %H:%M>"
        ":PLANNED: %^t"
        ":END:"
        "%?"))
        
        (defun sr/fun/note-link-passive ()
        '("* %a"
        ":PROPERTIES:"
        ":CREATED: %U"
        ":END:"
        "%?"))
        
        (defun sr/fun/note-passive ()
        '("* %?"
        ":PROPERTIES:"
        ":CREATED: %U"
        ":END:"))

3.  doct templates

    -   [X] Created inactive date for all entries
    -   [X] Mail : Active date + mu4e link
    -   [X] Mail : Passive date + mu4e link. Meant for general notes and archive.
    -   [X] Note : passive date. Generally not refiled.
    -   [ ] Note : With active date to be refiled or acted upon.
        -   I am not sure if this makes sense. If action is required, it should
            be a task.
    -   [ ] Link :
    -   [ ] Capture to today's journal
    -   [ ] Capture to tomorrow's journal
    -   [ ] Capture to current clocked task
    
        (setq org-capture-templates
              (doct '(("capture" :keys "c"
                       :file "~/my_org/todo-global.org"
                       :prepend t
        	       :children (("inbox"
        			   :keys "t"
        			   :type entry
        	                   :file "~/my_org/refile.org"
        			   :headline "inbox"
        			   :todo-state "TODO"
        			   :template sr/fun/todo-passive)
        			  ("mail"
        			   :keys "m"
        			   :type entry
        	                   :file "~/my_org/refile.org"
        			   :todo-state "TODO"
        			   :headline "mail"
        			   :template sr/fun/todo-link-passive)
                                  ("reading" :keys "r"
                                   :headline   "reading"
                                   :todo-state "TODO"
        			   :template sr/fun/todo-link-passive)
        			  ("emacs" :keys "e"
                                   :headline   "emacs"
                                   :todo-state "TODO"
        			   :template sr/fun/todo-link-passive)))
        	      ("Todo" :keys "t"
                       :file "~/my_org/todo-global.org"
                       :prepend t
        	       :children (("inbox"
        			   :keys "t"
        			   :type entry
        			   :headline "@inbox"
        			   :todo-state "TODO"
        			   :template sr/fun/todo-act-date)
        			  ("mail"
        			   :keys "m"
        			   :type entry
        			   :headline "@mail"
        			   :todo-state "TODO"
        			   :template sr/fun/todo-link-act-date)
        			  ("article"
        			   :keys "r"
        			   :type entry
        			   :headline "@reading"
        			   :todo-state "TODO"
        			   :template sr/fun/todo-link-act-date)
        			  ("File link"
        			   :keys "f"
        			   :type entry
        			   :headline "@inbox"
        			   :todo-state "TODO"
        			   :clock-in t
        			   :template sr/fun/todo-file-link-act-date)
        			  ("External link"
        			   :keys "e"
        			   :type entry
        			   :headline "@reading"
        			   :todo-state "TODO"
        			   :template sr/fun/todo-file-ext-link-act-date)))
        	      ("Notes" :keys "n"
                       :file "~/my_org/notes.org"
                       :prepend t
                       :template sr/fun/note-passive
                       :children (("Fast note"
        			   :keys "n"
        			   :type entry
        			   :headline   "@Notes")
        			  ("Emacs note"
        			   :keys "e"
        			   :file "~/my_org/emacs.org"
        			   :type entry)
        			  ("Mail Archive"
        			   :template sr/fun/note-link-passive
        			   :keys "m"
        			   :type entry
        			   :headline "@mail archive")
        			  ("Read Archive"
        			   :template sr/fun/note-link-passive
        			   :keys "r"
        			   :type entry
        			   :headline "@read archive")
        			  ("DS Link note"  :keys "d"
        			         :file "~/my_org/datascience.org"
                                   :headline   "@Datascience @Notes"
                                   :todo-state "TODO"
        			   :template sr/fun/todo-link-passive)))
        	      ;; ("Project" :keys "p"
                      ;;  :file "~/my_org/project-tasks.org"
                      ;;  :template sr/fun/todo-link-active)
        	      )))


<a id="org7fe550c"></a>

### Closing org-capture frame on abort

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-03-13 Wed 07:35] </span></span>   
    This basically ensures a clean exit in case of aborting a capture, and
    also maintains buffer configuration on going ahead with the capture.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:53]  </span></span>    
    Needs further review.

Source: [emacs - hook or advice when aborting org-capture before template selection? - Stack Overflow](http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection)

    (defadvice org-capture
        (after make-full-window-frame activate)
      "Advise capture to be the only window when used as a popup"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-other-windows)))
    
    (defadvice org-capture-finalize
        (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (if (equal "emacs-capture" (frame-parameter nil 'name))))


<a id="org31d71a2"></a>

### TODO Controlling org-capture buffers

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-03-13 Wed 08:01] </span></span>   
    This interferes with org-journal's capture format.

I dislike the way org-capture disrupts my current window, and shows me
the capture buffer, and the target buffer as well. I would prefer a
small pop up window, and then a revert back to the existing windows once
the capture is completed or aborted. However this does not seem possible
without modifying Org-mode's source code. This is a workaround described
at
<https://stackoverflow.com/questions/54192239/open-org-capture-buffer-in-specific-Window>
,which partially resolves the issue by enabling just a single capture
buffer.

    
    (defun my-org-capture-place-template-dont-delete-windows (oldfun args)
      (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
        (apply oldfun args)))
    
    (with-eval-after-load "org-capture"
      (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))


<a id="org6430df7"></a>

## TODO Refile mechanics


<a id="orgc510a0d"></a>

### Refile target level

    (setq org-refile-targets
          '((nil :maxlevel . 3)
            (org-agenda-files :maxlevel . 2)))


<a id="org32dd3e0"></a>

### General refiling settings

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-reverse-note-order t)
    (setq org-refile-allow-creating-parent-nodes 'confirm)


<a id="org9647347"></a>

## Clocking mechanics


<a id="orgcee473e"></a>

### Continuous clocking + punch in/out approach

This approach and code snippets are adapted (and shamelessly borrowed)
from [Bernt Hansen's approach](http://doc.norang.ca/org-mode.html). While Bernt follows a complex approach of
clocking into parent tasks - my current workflow favors clocking in
directly to set clocking headlines within projects, which are placed in
my org-projectile todo task file.

I have a default continuous clock after punching in (defined by org-id)
which will cater to general re-organisation, including capturing notes,
refiling , email etc. Other tasks or even mini projects can be directly
clocked into when required. These mini-projets are often just located
within my org-agenda files and not as a separate git repositoy. Every
time I am on my computer, whether on Emacs or not, I would like the
automatic clock to capture time, unless it is being clocked to a
specific project.

1.  Defining default Task

        (defvar sr/var/organization-task-id "a8712a47-a648-477f-bdbf-d6004a0cc70b")
        
        (defun sr/clock-in-organization-task-as-default ()
          (interactive)
          (org-with-point-at (org-id-find sr/var/organization-task-id 'marker)
            (org-clock-in '(16))))

2.  Punch in

    Bernt Hansen shares that he has a default punch in and punch out task that keeps the clock on all day. I think this will work for me as well. Other than work and projects, most of the time I am tinkering with Emacs, or writing a journal note or trying to re-organise my stuff. By using a punch in and out, I can track how much time I am engaged with a computer, other than specific projects.
    
        (defun sr/punch-in (arg)
            (interactive "p")
          (setq sr/keep-clock-running t)
          (sr/clock-in-organization-task-as-default))

3.  Punch Out

        (defun sr/punch-out ()
          (interactive)
          (setq sr/keep-clock-running nil)
          (when (org-clock-is-active)
            (org-clock-out)))

4.  Advising clock Out

        (defun sr/clock-out-maybe ()
          (when (and sr/keep-clock-running
                     (not org-clock-clocking-in)
                     (marker-buffer org-clock-default-task)
                     (not org-clock-resolving-clocks-due-to-idleness))
            (sr/clock-in-organization-task-as-default)))
        
        (add-hook 'org-clock-out-hook 'sr/clock-out-maybe 'append)

5.  Shortcuts for punch in and punch out

        (global-set-key (kbd "C-<f9>") 'sr/punch-in)
        (global-set-key (kbd "M-<f9>") 'sr/punch-out)


<a id="org5c4487a"></a>

### set idle timer for clocked task

    ;; setting idle timer to 15 minutes
    (setq org-clock-idle-time 15)


<a id="org325e31f"></a>

### No zero clocks

    (setq org-clock-out-remove-zero-time-clocks t)


<a id="org582fe19"></a>

### Clocking accuracy

This is borrowed off Bernt Hansen's method.

    (setq org-agenda-clock-consistency-checks
          (quote (:max-duration "4:00"
                  :min-duration 0
                  :max-gap 0
                  :gap-ok-around ("4:00"))))


<a id="orgfb8e2f8"></a>

### org-mru-clock

-   [ ] use the functions included to capture to the current clocked tasks.
-   [ ] method to jump to recent clocked task instead of starting the clock

This is a handy package to quickly select past tasks which have been clocked in.

    (use-package org-mru-clock
      :ensure t
      :bind (("M-s 1" . org-mru-clock-in)
              ("C-c C-x C-j" . org-mru-clock-select-recent-task))
      :init
      (setq org-mru-clock-how-many 100
            org-mru-clock-completing-read #'ivy-completing-read))


<a id="org46d76de"></a>

### counsel-org-clock

Here is a comparison of counsel-org-clock and org-mru-clock: [Marcin
Borkowski: 2018-04-28 org-mru-clock](http://mbork.pl/2018-04-28_org-mru-clock). As mentioned, one main advantage of
this package are the extension via ivy actions, though these can be
defined for `org-mru-clock`. Since I want to jump around previously
clocked tasks and examine them, these functions are useful to
me. However, the advantage of org-mru-clock is the list of all the
clocked tasks from the agenda.

    (use-package counsel-org-clock)


<a id="org67c8158"></a>

## Task state sequences

The difference between cancelled and failed would be that -

failed: things I could have planned and done, and it would have been a
good thing to do - but I did not. These kind of tasks are worth tracking
in a general sense, and that is why a failed tag would be useful. The
agenda can then be used to filter failed tasks to see whether there have
been repeated failures in the past.

Cancelled tasks - these are tasks that do not require to be done. The
reason could be that they were pre-emptive tasks that were superceded by
events, or simply deemed unnecessary at all levels. Such tasks would not
warrant a detailed review in general.

Looking at Bernt Hansen's documentation - it also makes sense to have
some more key words for filtering. Summary of sequences that make sense
at this point:

-   TODO
-   Next
-   Done

This sequence will include a note as to why the state is being set.

-   Waiting
-   Hold
-   Cancelled
-   Failed

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "FAILED(f@/!)"))))


<a id="org2b89661"></a>

## org-source-window split setup

    (setq org-src-window-setup 'split-window-right)


<a id="org6a5d7ac"></a>

## Log done

    (setq org-log-done 'time)


<a id="org15ef2ec"></a>

## TODO Shortcuts (to be replaced via hydra)

    (global-set-key (kbd "C-c d") 'org-time-stamp)
    (global-set-key (kbd "M-s s") 'org-save-all-org-buffers)


<a id="org470fda9"></a>

## TODO org-ql

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-24 Fri 09:59] </span></span>   
    This is an interesting package that I want to master using
    effectively.

    (use-package org-ql
    :straight t)


<a id="orgcb36a70"></a>

## Org git link


<a id="org70928ff"></a>

### Note

This is very handy to save as a capture and ready reference to a
particular commit. This also works well while referring to links in
conversations.

Open the particular commit and then use the org link function. The exact
commit link will be saved.

-   [ ] perhaps this should be stored as a package and therefore byte
    compiled rather than loaded for each start up.


<a id="org948ae16"></a>

### script

    ;;; org-git-link.el --- Provide org links to specific file version
    
    ;; Copyright (C) 2009-2013  Reimar Finken
    
    ;; Author: Reimar Finken <reimar.finken@gmx.de>
    ;; Keywords: files, calendar, hypermedia
    
    ;; This file is not part of GNU Emacs.
    
    ;; This program is free software; you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.
    
    ;; This program is distaributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.
    
    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
    ;;; Commentary:
    
    ;; `org-git-link.el' defines two new link types. The `git' link
    ;; type is meant to be used in the typical scenario and mimics the
    ;; `file' link syntax as closely as possible. The `gitbare' link
    ;; type exists mostly for debugging reasons, but also allows e.g.
    ;; linking to files in a bare git repository for the experts.
    
    ;; * User friendy form
    ;;   [[git:/path/to/file::searchstring]]
    
    ;;   This form is the familiar from normal org file links
    ;;   including search options. However, its use is
    ;;   restricted to files in a working directory and does not
    ;;   handle bare repositories on purpose (see the bare form for
    ;;   that).
    
    ;;   The search string references a commit (a tree-ish in Git
    ;;   terminology). The two most useful types of search strings are
    
    ;;   - A symbolic ref name, usually a branch or tag name (e.g.
    ;;     master or nobelprize).
    ;;   - A ref followed by the suffix @ with a date specification
    ;;     enclosed in a brace pair (e.g. {yesterday}, {1 month 2
    ;;     weeks 3 days 1 hour 1 second ago} or {1979-02-26 18:30:00})
    ;;     to specify the value of the ref at a prior point in time
    ;;
    ;; * Bare git form
    ;;   [[gitbare:$GIT_DIR::$OBJECT]]
    ;;
    ;;    This is the more bare metal version, which gives the user most
    ;;    control. It directly translates to the git command
    ;;    git --no-pager --git-dir=$GIT_DIR show $OBJECT
    ;;    Using this version one can also view files from a bare git
    ;;    repository. For detailed information on how to specify an
    ;;    object, see the man page of `git-rev-parse' (section
    ;;    SPECIFYING REVISIONS). A specific blob (file) can be
    ;;    specified by a suffix clolon (:) followed by a path.
    
    ;;; Code:
    
    (require 'org)
    (defcustom org-git-program "git"
      "Name of the git executable used to follow git links."
      :type '(string)
      :group 'org)
    
    ;; org link functions
    ;; bare git link
    (org-add-link-type "gitbare" 'org-gitbare-open)
    
    (defun org-gitbare-open (str)
      (let* ((strlist (org-git-split-string str))
             (gitdir (first strlist))
             (object (second strlist)))
        (org-git-open-file-internal gitdir object)))
    
    
    (defun org-git-open-file-internal (gitdir object)
      (let* ((sha (org-git-blob-sha gitdir object))
             (tmpdir (concat temporary-file-directory "org-git-" sha))
             (filename (org-git-link-filename object))
             (tmpfile (expand-file-name filename tmpdir)))
        (unless (file-readable-p tmpfile)
          (make-directory tmpdir)
          (with-temp-file tmpfile
            (org-git-show gitdir object (current-buffer))))
        (org-open-file tmpfile)
        (set-buffer (get-file-buffer tmpfile))
        (setq buffer-read-only t)))
    
    ;; user friendly link
    (org-add-link-type "git" 'org-git-open)
    
    (defun org-git-open (str)
      (let* ((strlist (org-git-split-string str))
             (filepath (first strlist))
             (commit (second strlist))
             (dirlist (org-git-find-gitdir (file-truename filepath)))
             (gitdir (first dirlist))
             (relpath (second dirlist)))
        (org-git-open-file-internal gitdir (concat commit ":" relpath))))
    
    
    ;; Utility functions (file names etc)
    
    (defun org-git-split-dirpath (dirpath)
      "Given a directory name, return '(dirname basname)"
      (let ((dirname (file-name-directory (directory-file-name dirpath)))
            (basename (file-name-nondirectory (directory-file-name dirpath))))
        (list dirname basename)))
    
    ;; finding the git directory
    (defun org-git-find-gitdir (path)
      "Given a file (not necessarily existing) file path, return the
      a pair (gitdir relpath), where gitdir is the path to the first
      .git subdirectory found updstream and relpath is the rest of
      the path. Example: (org-git-find-gitdir
      \"~/gitrepos/foo/bar.txt\") returns
      '(\"/home/user/gitrepos/.git\" \"foo/bar.txt\"). When not in a git repository, return nil."
      (let ((dir (file-name-directory path))
            (relpath (file-name-nondirectory path)))
        (catch 'toplevel
          (while (not (file-exists-p (expand-file-name ".git" dir)))
            (let ((dirlist (org-git-split-dirpath dir)))
              (when (string= (second dirlist) "") ; at top level
                (throw 'toplevel nil))
              (setq dir (first dirlist)
                    relpath (concat (file-name-as-directory (second dirlist)) relpath))))
          (list (expand-file-name ".git" dir) relpath))))
    
    
    (eval-and-compile
      (if (featurep 'xemacs)
          (defalias 'org-git-gitrepos-p 'org-git-find-gitdir)
        (defalias 'org-git-gitrepos-p 'org-git-find-gitdir
          "Return non-nil if path is in git repository")))
    
    ;; splitting the link string
    
    ;; Both link open functions are called with a string of
    ;; consisting of two parts separated by a double colon (::).
    (defun org-git-split-string (str)
      "Given a string of the form \"str1::str2\", return a list of
      two substrings \'(\"str1\" \"str2\"). If the double colon is mising, take str2 to be the empty string."
      (let ((strlist (split-string str "::")))
        (cond ((= 1 (length strlist))
               (list (car strlist) ""))
              ((= 2 (length strlist))
               strlist)
              (t (error "org-git-split-string: only one :: allowed: %s" str)))))
    
    ;; finding the file name part of a commit
    (defun org-git-link-filename (str)
      "Given an object description (see the man page of
      git-rev-parse), return the nondirectory part of the referenced
      filename, if it can be extracted. Otherwise, return a valid
      filename."
      (let* ((match (and (string-match "[^:]+$" str)
                         (match-string 0 str)))
             (filename (and match (file-name-nondirectory match)))) ;extract the final part without slash
        filename))
    
    ;; creating a link
    (defun org-git-create-searchstring (branch timestring)
      (concat branch "@{" timestring "}"))
    
    
    (defun org-git-create-git-link (file)
      "Create git link part to file at specific time"
      (interactive "FFile: ")
      (let* ((gitdir (first (org-git-find-gitdir (file-truename file))))
             (branchname (org-git-get-current-branch gitdir))
             (timestring (format-time-string "%Y-%m-%d" (current-time))))
        (concat "git:" file "::" (org-git-create-searchstring branchname timestring))))
    
    (defun org-git-store-link ()
      "Store git link to current file."
      (when (buffer-file-name)
        (let ((file (abbreviate-file-name (buffer-file-name))))
          (when (org-git-gitrepos-p file)
    	(org-store-link-props
    	 :type "git"
    	 :link (org-git-create-git-link file))))))
    
    (add-hook 'org-store-link-functions 'org-git-store-link)
    
    (defun org-git-insert-link-interactively (file searchstring &optional description)
      (interactive "FFile: \nsSearch string: \nsDescription: ")
      (insert (org-make-link-string (concat "git:" file "::" searchstring) description)))
    
    ;; Calling git
    (defun org-git-show (gitdir object buffer)
      "Show the output of git --git-dir=gitdir show object in buffer."
      (unless
          (zerop (call-process org-git-program nil buffer nil
                               "--no-pager" (concat "--git-dir=" gitdir) "show" object))
        (error "git error: %s " (with-current-buffer buffer (buffer-string)))))
    
    (defun org-git-blob-sha (gitdir object)
      "Return sha of the referenced object"
        (with-temp-buffer
          (if (zerop (call-process org-git-program nil t nil
                                   "--no-pager" (concat "--git-dir=" gitdir) "rev-parse" object))
              (buffer-substring (point-min) (1- (point-max))) ; to strip off final newline
            (error "git error: %s " (buffer-string)))))
    
    (defun org-git-get-current-branch (gitdir)
      "Return the name of the current branch."
      (with-temp-buffer
        (if (not (zerop (call-process org-git-program nil t nil
                                      "--no-pager" (concat "--git-dir=" gitdir) "symbolic-ref" "-q" "HEAD")))
            (error "git error: %s " (buffer-string))
          (goto-char (point-min))
          (if (looking-at "^refs/heads/")   ; 11 characters
              (buffer-substring 12 (1- (point-max))))))) ; to strip off final newline
    
    (provide 'org-git-link)
    
    ;;; org-git-link.el ends here
    (require 'org-git-link)


<a id="org6f37671"></a>

# Temporary package list

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-30 Mon 07:58] </span></span>   
    A lot of these are borrowed from scimax and will be slowly whittled down to the essentials.

        ;; (use-package helm-bibtex)
    
        ;; Functions for working with hash tables
        (use-package ht)
    
        (use-package hy-mode)
    
        (use-package hydra
          :init
          (setq hydra-is-helpful t)
          :config
          (require 'hydra-ox))
    
        (use-package jedi)
    
        (use-package diminish)
    
      (use-package rainbow-mode)
    
      ;; Provides functions for working with files
      (use-package f)
    
    
      (use-package auto-complete
        :diminish auto-complete-mode
        :config (ac-config-default))
    
    (straight-use-package 'ggtags)
    (straight-use-package 'ibuffer-projectile)

    t


<a id="org7d496ff"></a>

# Org journal

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-10 Fri 17:29] </span></span>   
    Scimax journal has the benefit of being well integrated with Scimax. The
    other benefit is being able to have journal files within a
    folder. However, for the journal to be included into GTD workflow and
    not just a scratch pad, the org-journal package offers a lot more out of
    the box. I see from the documentation that is also possible now to have
    a weekly, daily or yearly journal. In addition, the agenda integration
    simplifies things in terms of tracking what I am doing and then refiling
    the entries into appropriate locations.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-02-22 Sat 10:58] </span></span>   
    I've been using scimax journal for several months now, as it has some
    defaults which suit my current workflow. However, org-journal does
    have a bunch of nifty features that I want to try and port to scimax.


<a id="org912c1a8"></a>

## Base config

    (use-package org-journal
      :ensure t
      :defer t
      :config
      (defun org-journal-file-header-func (time)
        "Custom function to create journal header."
        (concat
         (pcase org-journal-file-type
           (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
           (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
           (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
           (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
      :custom
      (org-journal-dir "~/my_org/journal/")
      (org-journal-file-format "%Y/%m/%Y-%m-%d.org")
      (org-journal-enable-agenda-integration t)
      (org-journal-date-format "%A, %d %B %Y")
      (org-journal-file-type 'daily) ;; Set this explicitly, even though the default is daily.
      (org-journal-file-header 'org-journal-file-header-func)
      ;; Carrying over items though useful, sometimes results in duplication
      ;; and a loss of items, especially if journalling is carried out at
      ;; 12PM or when the day is changing.
      ;; New idea: Do not carry over clocked items. The remaining can be carried over
      ;; (org-journal-carryover-items "TODO")
      (org-journal-skip-carryover-drawers (list "LOGBOOK")))


<a id="org50d5335"></a>

## TODO org-capture template for Journal

    (defun org-journal-find-location ()
      ;; Open today's journal, but specify a non-nil prefix argument in order to
      ;; inhibit inserting the heading; org-capture will insert the heading.
      (org-journal-new-entry t)
      ;; Position point on the journal's top-level heading so that org-capture
      ;; will add the new entry as a child entry.
      (goto-char (point-min)))


<a id="org0ca10db"></a>

## TODO Figure out easy encryption approach for org journal


<a id="orgcbd2136"></a>

# Crypto


<a id="orgfdd3861"></a>

## Basic crypto

    (setq epa-file-encrypt-to "shreyas@fastmail.com")


<a id="org4c31e71"></a>

## TEST org-crypt

    (require 'org-crypt)
    (add-to-list 'org-modules 'org-crypt)
                                            ; Encrypt all entries before saving
    (org-crypt-use-before-save-magic)
    ;;(setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                            ; GPG key to use for encryption. nil for symmetric encryption
    ;;(setq org-crypt-key nil)
    (setq org-crypt-disable-auto-save t)
    ;;(setq org-crypt-tag-matcher "locked")


<a id="org444343a"></a>

## Setting auth sources

This was prompted by this discussion <https://emacs.stackexchange.com/questions/10207/how-to-get-org2blog-to-use-authinfo-gpg>

I have modified it to my own file names.

    (require 'auth-source)
    (setq auth-sources
          '((:source "~/.authinfo.gpg"
    		 "~/.bitly-access.token.gpg")))
    
    (setq epa-file-cache-passphrase-for-symmetric-encryption t)


<a id="org2fdd416"></a>

# git related


<a id="org64c6563"></a>

## TODO Git gutter

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:30]  </span></span>    
    Started using this today. It is actually very convenient to quickly view the changes made in the document. There is a function to pop up the changes at that location. I need to learn more about using this tool effectively.

    (use-package git-gutter
      :ensure t
      :config
      (global-git-gutter-mode 't)
      :diminish git-gutter-mode)


<a id="org8898810"></a>

## magit settings

    (use-package magit
      :init (setq magit-completing-read-function 'ivy-completing-read)
    :config
    (global-set-key (kbd "C-x g") 'magit-status)
    (setq magit-revert-buffers 'silent)
    (setq magit-process-find-password-functions '(magit-process-password-auth-source)))


<a id="orge676110"></a>

## TODO Time machine for git

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-08 Fri 13:21] </span></span>   
    Launched by `M-x git-timemachine`, this lets you navigate through the commit history with a single key press! This is especially awesome for tracking changes to a particular snippet of code.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:30]  </span></span>    
    Need to evaluate this. The purpose is for stepping through the history of a file recorded in git. This should be very interesting.

    (use-package git-timemachine
      :ensure t)


<a id="org184cb21"></a>

# PDF related


<a id="org8de6482"></a>

## STABLE PDF Tools

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-10-23 Wed 09:26] </span></span>   
    This appears to be setup via scimax already. Disabling for now.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-18 Mon 14:30] </span></span>   
    [osx - Install Pdf-Tools on Emacs MacOSX - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx)

    (use-package pdf-tools
      :ensure t
      :config
      (custom-set-variables
       '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead in the mac
      (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
      (pdf-tools-install)
    )


<a id="org671e899"></a>

## org-noter

> Org-noter's purpose is to let you create notes that are kept in sync when you scroll through the document, but that are external to it - the notes themselves live in an Org-mode file. As such, this leverages the power of Org-mode (the notes may have outlines, latex fragments, babel, etc) acting like notes that are made inside the document. Also, taking notes is very simple: just press i and annotate away!
> 
> [Goncalo Santos](https://github.com/weirdNox)

    (use-package org-noter
      :ensure t
      :defer t
      :config
      (setq org-noter-set-auto-save-last-location t)
      )


<a id="org0d5b036"></a>

# Window, frame and buffer management


<a id="orgfe26791"></a>

## winum

This package makes it easy to switch between frames, and is particularly useful in a multi screen setup of emacs.

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
       winum-scope                       'frame-local
       window-numbering-scope            'frame-local
       winum-ignored-buffers             '(" *which-key*")
       winum-ignored-buffers-regexp      '(" \\*Treemacs-.*"))
      (winum-mode))


<a id="org818ee93"></a>

## Winner mode

Enabling winner mode. This is convenient to switch between temporary window configurations in conjunction with somewhat more permanent configurations in eyebrowse.

    (winner-mode)


<a id="orgb6e262e"></a>

## TEST eyebrowse

This has to be combined with desktop.el or some other method to enable persistence across sessions. However, this does work well for a single session.

    (use-package eyebrowse
      :ensure t
      :config
      (setq eyebrowse-mode-line-separator " "
            eyebrowse-new-workspace t)
      (eyebrowse-mode 1)
      )


<a id="org5102334"></a>

## TODO Bufler

For the few occassions that I use the buffer-list command, I think the bufler
package provides a more functional interface.

-   [ ] explore the workspace configuration format. Can this restricted on a
    frame basis like eyebrowse? Does that even make sense?

    (use-package bufler
      :straight (bufler :host github :repo "alphapapa/bufler.el")
    :bind ("C-x C-b" . bufler-list))


<a id="orgcd94981"></a>

## Frame settings

I'm often running emacsclient with a daemon and have found that specific settings need to be set for each frame that is created, like the window size, and the visual fill column and line settings.

1.  [X] Full screen and windows on Mac OS
2.  [X] Enable global visual line mode and visual fill column mode. Apparently this was fixed without frame settings.

    (add-to-list 'default-frame-alist '(fullscreen . fullboth))


<a id="org5388a15"></a>

## crux

Crux has a handle re-open and root function that will open a file as root if the permissions are set so.

    (use-package crux
      :straight t
      :defer 10
      :bind (("C-c C-s" . crux-sudo-edit)
             ("C-c C-r" . crux-eval-and-replace)
             ("C-c o" . crux-open-with))
      :config
      (progn
        (crux-reopen-as-root-mode)))


<a id="org774b50c"></a>

# Emacs information


<a id="org5654985"></a>

## which key

    (use-package which-key
      :defer 5
      :diminish which-key-mode
      :straight t
      :config
      (which-key-mode))

    t


<a id="orgdeed6f3"></a>

# Project management


<a id="org9434cb7"></a>

## TODO org-projectile

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:42]  </span></span>    
    need to optimise further and convert to use-package style. Also need a way to capture Notes from projects, in addition to tasks.

Starting off with the basic configuration posted in org-projectile github repo.

    (use-package org-projectile
      :straight t
      :bind (("C-c n p" . org-projectile-project-todo-completing-read)
             ("C-c c" . org-capture))
      :config
      (setq org-projectile-projects-file
            "~/my_org/project-tasks.org")
      ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))) ;; Not necessary as my task projects are a part of the main org folder
      (setq org-projectile-capture-template "* TODO %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:\n")
      (push (org-projectile-project-todo-entry) org-capture-templates))

    org-capture


<a id="org38c3c6d"></a>

## projectile

-   [ ] Add a variable for the emacs<sub>meta</sub> directory.

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


<a id="orgf90f750"></a>

# Knowledge management


<a id="org76559d7"></a>

## org-brain


<a id="orge59e439"></a>

### Main configuration

    (use-package org-brain
      :straight (org-brain :type git :host github :repo "Kungsgeten/org-brain")
      ;; :straight (org-brain :type git :host github :repo "Kungsgeten/org-brain"
      ;; 			 :fork (:host github :repo "dustinlacewell/org-brain"))
      :after org
      :bind ("M-s v" . helm-brain)
      :config
      ;; this unbinds all default org-brain bindings
      ;; (setcdr org-brain-visualize-mode-map nil)
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
      ;; (push '("b" "Brain" plain (function org-brain-goto-end)
      ;;         "* %i%?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:ID: [%(org-id-get-create)]\n:END:" :empty-lines 1
      ;;         org-capture-templates))
      (push '("b" "Brain" plain (function org-brain-goto-end)
              "* %i%?" :empty-lines 1)
    	org-capture-templates))
    
    
    ;; (add-hook 'org-brain-refile 'org-id-get-create)))


<a id="org695820b"></a>

### org-cliplink function

    (defun org-brain-cliplink-resource ()
      "Add a URL from the clipboard as an org-brain resource.
    Suggest the URL title as a description for resource."
      (interactive)
      (let ((url (org-cliplink-clipboard-content)))
        (org-brain-add-resource
         url
         (org-cliplink-retrieve-title-synchronously url)
         t)))
    
    (define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)


<a id="orgc004d5e"></a>

### Chronological entries

    ;; Setup org-expiry and define a org-agenda function to compare timestamps
    (require 'org-expiry)
    (setq org-expiry-inactive-timestamps t)
    (defun org-expiry-created-comp (a b)
      "Compare `org-expiry-created-property-name' properties of A and B."
      (let ((ta (ignore-errors
                  (org-time-string-to-seconds
                   (org-entry-get (get-text-property 0 'org-marker a)
                                  org-expiry-created-property-name))))
            (tb (ignore-errors
                  (org-time-string-to-seconds
                   (org-entry-get (get-text-property 0 'org-marker b)
                                  org-expiry-created-property-name)))))
        (cond ((if ta (and tb (< ta tb)) tb) -1)
              ((if tb (and ta (< tb ta)) ta) +1))))
    
    ;; Add CREATED property when adding a new org-brain headline entry
    (add-hook 'org-brain-new-entry-hook #'org-expiry-insert-created)
    
    ;; Finally add a function which lets us watch the entries chronologically
    (defun org-brain-timeline ()
      "List all org-brain headlines in chronological order."
      (interactive)
      (let ((org-agenda-files (org-brain-files))
            (org-agenda-cmp-user-defined #'org-expiry-created-comp)
            (org-agenda-sorting-strategy '(user-defined-down)))
        (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))


<a id="org4481d16"></a>

### Navigation Helpers

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


<a id="org672df1c"></a>

## define-word

    (straight-use-package 'define-word)


<a id="org5e7ce82"></a>

## Wiktionary lookup

From [Dictionary and thesaurus in Emacs : emacs](https://www.reddit.com/r/emacs/comments/3yjzmu/dictionary_and_thesaurus_in_emacs/), this does not seem to
work.

    (defun lookup-word (word)
      (interactive (list (save-excursion (car (ispell-get-word nil)))))
      (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))


<a id="org1540975"></a>

## merriam-webster

The advantage of this is that synonyms and other definitions are shown,
and they are apparently within Emacs. Note that it is possible and may
even be necessary to register an API key with merriam-webster.

    (straight-use-package 'mw-thesaurus)


<a id="orga7ffac3"></a>

## pinboard <code>[2/2]</code>

-   [X] Figure out the auth file settings from before
-   [X] Checkout the org mode snippet [link](https://gist.github.com/khinsen/7ed357eed9b27f142e4fa6f5c4ad45dd)

To get started, visit your password settings page on Pinboard and get the API token that's displayed there.

Then edit ~/.authinfo and add a line like this:

    machine api.pinboard.in password foo:8ar8a5w188l3

This snippet seems to enable storing a pinboard link using the
org-store-link function. I find this very useful.

    (use-package pinboard
      :straight t
      :config
      (defun org-pinboard-store-link ()
        "Store a link taken from a pinboard buffer."
        (when (eq major-mode 'pinboard-mode)
          (pinboard-with-current-pin pin
    	(org-store-link-props
    	 :type "pinboard"
    	 :link (alist-get 'href pin)
    	 :description (alist-get 'description pin)))))
    
      (org-link-set-parameters "pinboard"
    			   :follow #'browse-url
    			   :store #'org-pinboard-store-link))


<a id="orgf314593"></a>

## org-web-tools

This package contains a bunch of useful tools which can cut down a lot of work

    (use-package org-web-tools
    :defer 5
    :bind (("H-y" . org-web-tools-insert-link-for-url)
           ("H-a" . org-web-tools-insert-web-page-as-entry)))


<a id="org76700d8"></a>

## org-download

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


<a id="org16c76da"></a>

## oddmuse-curl

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-01-21 Tue 19:23] </span></span>   
    converted to the use package method looking at the documentation.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-01-19 Sun 09:25] </span></span>   
    Current issue: Emacswiki is working as expected with oddmuse-curl. However, my own wiki is not being listed. I am able to curl the contents of pages from my wiki which indicates that curl is working for the website. Discussions are on with Alex to figure this out.

    
    
    ;; ;; Install the oddmuse package from github
    ;; (oddmuse-curl :type git :host github :repo "kensanata/oddmuse-curl")
    ;; (add-to-list 'load-path "~/scimax-personal/external_packages/oddmuse-curl/")
    
     (use-package oddmuse-curl
     :defer t
     :straight (:type git :host github :repo "kensanata/oddmuse-curl")
     :ensure t
     :config
    ;; user name
    (setq oddmuse-username "shrysr")
    
    ;; Wiki names
    (setq oddmuse-wikis
          '(("EmacsWiki" "https://www.emacswiki.org/emacs" utf-8 "abcd" nil)
    	("sr" "https://shrysr.ragavan.co" utf-8 nil nil)
    	("jobs" "https://wikijobs.ragavan.co" utf-8 nil nil)))
    
    ;; Directory where files will be downloaded from the wiki for editing.
    (setq oddmuse-directory "~/my_org/01_wiki/oddmuse/")
    
    ;; adding an oddmuse-odd for files in the fiki directory
    (add-to-list 'auto-mode-alist '("~/my_org/01_wiki/oddmuse" . oddmuse-mode))
    
    ;; autoload modes
    (autoload 'oddmuse-edit "oddmuse-curl"
      "Edit a page on an Oddmuse wiki." t)
    
    ;; Not yet sure what this does and how it related to version control.
    (add-to-list 'vc-handled-backends 'oddmuse)
    (defun vc-oddmuse-registered (file)
      "Handle files in `oddmuse-directory'."
      (string-match (concat "^" (expand-file-name oddmuse-directory))
                    (file-name-directory file))))
    
    ;; Since I work primarily with org before the wiki - I would rather not have the mode initialised.
    ;; (oddmuse-mode-initialize)
    
    ;; I would like to be able to call the wiki when desired and so the curl package is initialised.
    ;;(require 'oddmuse-curl)


<a id="orgde7d093"></a>

## ox-oddmuse

I'm building an oddmuse wiki to use for my blogging and as a repository for my braindump. A wiki driven blog is much more useful in inviting collaboration, and any technical note or article is always worth updating. There are many more advantages in using Oddmuse.

    (use-package ox-oddmuse
      :defer t
     :straight (:type git :host github :repo "mbork/ox-oddmuse")
     :init (require 'ox-oddmuse))


<a id="orgf58f710"></a>

# Dired

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-28 Sat 15:10] </span></span>   
    Apparently, dired is not available in to be installed via MELPA.

These are settings dervied from the configuration of [Angry Bacon](https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org). Also adapted from a [pragmatic emacs article](http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/).

Note that `C-x C-q` for `dired-toggle-read` only.

    (use-package dired-subtree
    :straight t
    :config
    (bind-keys :map dired-mode-map
               ("i" . dired-subtree-insert)
                 (";" . dired-subtree-toggle)))
    
    ;; Show directories first
     (defun me/dired-directories-first ()
        "Sort dired listings with directories first before adding marks."
        (save-excursion
          (let (buffer-read-only)
            (forward-line 2)
            (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
          (set-buffer-modified-p nil)))
    
    (advice-add 'dired-readin :after #'me/dired-directories-first)
    
    (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)
    			     (setq visual-fill-column-mode 'nil)))
    
    (setq dired-auto-revert-buffer t
       dired-dwim-target t
       dired-hide-details-hide-symlink-targets nil
       dired-listing-switches "-alh"
       dired-ls-F-marks-symlinks nil
       dired-recursive-copies 'always)


<a id="org4087ff8"></a>

# Treemacs <code>[0/3]</code>

-   [ ] Learn about treemacs projectile
-   [ ] Learn about treemacs-magit

As such most of these

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


<a id="org17baa89"></a>

# Selection, search, navigation and jumps


<a id="org09ea995"></a>

## smex

    (straight-use-package 'smex)


<a id="orgc4ac0b9"></a>

## Abbrev definitions

There are a number of terms and words that I use regularly in all my
writing, for example, Org mode, Emacs, DevonThink, and so on. These
words should be auto corrected to the correct intention considering the
frequency of their use. 

These definitions should be synced across machines.

Reference: <http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html>

    ;; Setting abbrev file location specifically
    (setq abbrev-file-name "~/my_org/emacs_meta/emacs-abbrev.el")
    
    ;;  save abbrevs silently on exit
    (setq save-abbrevs 'silently)
    
    ;;  Abbrev mode on by default
    (setq-default abbrev-mode t)


<a id="org09b94a8"></a>

## Counsel

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-30 Mon 18:08] </span></span>   
    Apparently, swiper, counsel, ivy are contained in the same repository
    and not as separate packages. Though I would prefer using counsel for
    everything with the minibuffer style, rather than helm - there are
    undoubtedly several advantages and built-in features in helm whereas
    counsel would need these carefully constructed. For example, it
    appears that the multiple select and action operation has to be
    separately defined for `counsel-find-file` and
    `counsel-switch-buffer`. While `counsel-M-x` seems generally more
    responsive than `helm-M-x`, helm offers a useful partition of the
    results in general based on recent commands/files and
    others. `helm-apropos` offers a list of commands and variables

This configuration is picked up from scimax.

-   [X] Figure out which of these functions benefit from helm rather than counsel.
    -   [X] helm-switch-buffer
    -   [X] helm-find-file
    -   [X] helm-appropos

    (use-package counsel
      :straight t
      :init
      (require 'ivy)
      (require 'smex)
      ;; switching to using helm for projectile completion
      ;;(setq projectile-completion-system 'ivy)
      (setq ivy-use-virtual-buffers t)
      (define-prefix-command 'counsel-prefix-map)
      (global-set-key (kbd "H-c") 'counsel-prefix-map)
    
      ;; default pattern ignores order.
      (setf (cdr (assoc t ivy-re-builders-alist))
            'ivy--regex-ignore-order)
      :bind
      (;; ("M-x" . counsel-M-x)
       ;; ("C-x b" . ivy-switch-buffer)
       ;; ("C-x C-f" . counsel-find-file)
       ("C-x l" . counsel-locate)
       ;; ("C-h f" . counsel-describe-function)
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


<a id="orge11ea40"></a>

## ripgrep

The ripgrep package for the system can be installed via brew or possibly
apt-get on Linux systems.

    brew install ripgrep

However, it appears an emacs package is required to interface with the
system installed ripgrep package via Emacs. In addition, there is a
separate helm-rg package to use helm for the interface. The latter is
preferred.

    (straight-use-package 'ripgrep)


<a id="orgd7617d3"></a>

## Expand region package

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:27]  </span></span>    
    Explore how this works, and customise it.

This can be set to intelligently expand the selection of text. For example, Using the designated binding, the first expansionh would cover say the content between quotes, and then expand outwards.

    (use-package expand-region
      :ensure t
      :bind ("C-=" . er/expand-region))
    
    (message "Loaded easier selection")


<a id="org60222d7"></a>

## Hippie Expand

This is a nifty little package that makes expansion of selection at point more customised, and is handy for expanding into variable names and function names in the same buffer, especially for a long snippet of code.

    (global-set-key (kbd "M-/") (make-hippie-expand-function
    			     '(try-expand-dabbrev-visible
    			       try-expand-dabbrev
    			       try-expand-dabbrev-all-buffers) t))


<a id="orga4f20cd"></a>

## Browse kill ring

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-30 Mon 18:20] </span></span>   
    This command allows an interactive browsing and insertion from the kill ring. However it does not allow a search. For enabling a search of the kill ring in addition to marking and insertion `helm-king-ring` can be used. However the latter method does not offer a preview of the material being yanked.

    (use-package browse-kill-ring
    :bind ("M-y" . browse-kill-ring)
      :ensure t
    )


<a id="orga48c531"></a>

## Multiple Cursors

    (use-package multiple-cursors
      :ensure t
      :config
      (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
      )
    
    (message "Loaded MC")


<a id="org746d387"></a>

## Undo tree

Reference: <https://github.com/alhassy/emacs.d>
This is an indispensable tool. The additional options of showing the timestamp and diff would be.

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


<a id="org1a5c61d"></a>

## TODO Setting undo limits

[performance - Emacs diagnosis: Org-mode unbearably slow and often
stalls - Stack Overflow](https://stackoverflow.com/questions/40793325/emacs-diagnosis-org-mode-unbearably-slow-and-often-stalls/50437758) talks abou the undo limit being rather large on
the Mac (ipto 80000) and that some people have had success in day long Emacs with
this option. I am starting with an undo of 10,000 and can increase this
based on the performance.

    (setq undo-limit 10000)


<a id="org15402ff"></a>

## yasnippet and ivy-yasnippet

-   [ ] setup the shortcut 'H-,' as desinged in scimax default for ivy-yasnippet

      (use-package yasnippet
        :straight t
        :config
        (setq yas-snippet-dirs (append yas-snippet-dirs
                                   '("~/.emacs.d/private-snippets/")))
      (yas-global-mode 1))
    
    (use-package ivy-yasnippet
      :bind ("M-s i" . ivy-yasnippet))


<a id="orgff962e2"></a>

## swiper

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-30 Mon 18:05] </span></span>   
    I had this to counsel-swiper-or-grep. However at times, the grep result would show as a binary file even though the file was clearly not binary. I have switched to using only swiper without counsel.

    (use-package swiper
      :bind
      ("C-s" . swiper)
      ("H-s" . swiper-all)
      :diminish ivy-mode
      :config
      (ivy-mode))


<a id="orgbde671e"></a>

## avy

    (use-package avy)


<a id="orgc02df97"></a>

## Super and Hyper key setting

    (if (system-type-is-darwin)
        (progn
          (setq mac-left-command-modifier 'super)
          (setq mac-right-option-modifier 'hyper)))


<a id="org8850e25"></a>

## Helm packages and functions

I am of the opinion that both helm and counsel/ivy have their advantages
for different workflows and mindsets. My configuration will attempt to
use both based on what I find useful.


<a id="org8257a71"></a>

### Helm notes

Despite using these command frequently, I find that I forget some of
them. This is an attempt to note down useful commands for ready
reference.

`C-u C-c C-k` using helm-find-file will copy the absolute path of the
file to the kill ring. Not using the modifier will copy only the
filename. `C-c C-i` or `C-c TAB` for copying the path to the current
buffer.

While browsing directories, use `C-j` to go into a directory and `C-l` to
move up.

`C-space` to select multiple files and open them with `RET`. This is useful when
switching to a project. Add a modifier `C-u RET` to open the files in an
alternate vertically split buffers.

Find file as root `C-c r` does not require the Tramp syntax!


<a id="org706a115"></a>

### Setting helm for some basic functions

    (use-package helm
    :config
    (helm-mode 1)
    (require 'helm-config)
    
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-h f") 'helm-apropos)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-c y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x C-r") 'helm-recentf))

    helm-find-file


<a id="org787f10b"></a>

### General settings for helm

There are some useful tips in the reddit thread [Why did you stop using
Helm? : emacs](https://www.reddit.com/r/emacs/comments/bsc8pc/why_did_you_stop_using_helm/). The [helm-intro](https://tuhdo.github.io/helm-intro.html) is a good resource. The documentation is
the best guide as always. Another good resource is the [Guide to get an
intuitive and more ido-like helm interface in Emacs.](https://github.com/clemera/helm-ido-like-guide)

    ;; Don't have helm do window management for me
    (setq helm-display-function 'pop-to-buffer)
    
    ;; optimising highlight speed of token matches
    (setq helm-mp-highlight-delay 0.3)
    
    ;; from the reddit thread above
    (setq helm-use-undecorated-frame-option t)
    (setq helm-display-buffer-reuse-frame t)
    
    ;; These are required to prevent crazy jumps in the helm buffer size
    (setq helm-autoresize-max-height 0)
    (setq helm-autoresize-min-height 30)
    (helm-autoresize-mode 1)
    
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     nil; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-echo-input-in-header-line t)


<a id="org5082a3e"></a>

### Helm bibtex and projectile

    (straight-use-package 'helm-bibtex)
    
    (straight-use-package 'helm-projectile)


<a id="org65629ab"></a>

### helm-ag

-   [ ] [How to tell Helm to ignore certain files? - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/30204/how-to-tell-helm-to-ignore-certain-files)
    -   This needs to be explored further for code project files and the
        specific options needed for different situations. I need to expand
        on this with more clarity.
    -   One method of ignoring certain patterns is using &#x2013;ignore=\*.csv
        -   Some specific patterns could be saved as prefixed options and
            added as optional search functions depending on the occassion.

> I'm not sure when this functionality was introduced, but you can exclude
> files from helm-find-files by setting helm-ff-skip-boring-files to t and
> making sure the ignored pattern is on helm-boring-file-regexp-list. From
> its documentation:
> 
> > Non&#x2013;nil to skip files matching regexps in
> helm-boring-file-regexp-list. This take effect in helm-find-files and
> file completion used by helm-mode i.e helm-read-file-name.
> 
> And the documentation for helm-boring-file-regexp-list says that by
> default it is built from completion-ignored-extensions.

Another option is to use [.agignore](https://github.com/emacsorphanage/helm-ag#helm-ag-use-agignoredefault-nil) at the project root. This is set to
nil by default. This kind of ignore option needs to be used carefully,
or it will exclude important results.

    (straight-use-package 'helm-ag)
    (setq helm-ag-use-agignore 1)


<a id="org5d4c28a"></a>

### helm-rg

    (straight-use-package 'helm-rg)


<a id="orgdaa1e61"></a>

### helm-org-rifle

This includes functions for org-brain

    (use-package helm-org-rifle
      :straight t
      :config
      (global-set-key (kbd "C-c C-w") #'helm-org-rifle--refile)
    
      (defun helm-org-rifle-brain ()
        "Rifle files in `org-brain-path'."
        (interactive)
        (let ((helm-org-rifle-close-unopened-file-buffers nil))
          (helm-org-rifle-directories (list org-brain-path))))
    
      (defun helm-org-rifle-open-in-brain (candidate)
        (-let (((buffer . pos) candidate))
          (with-current-buffer buffer
            (goto-char pos)
            (org-brain-visualize-entry-at-pt))))
    
      (add-to-list 'helm-org-rifle-actions
                   (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain)
                   t))


<a id="org4882683"></a>

### Helm swoop

    (use-package helm-swoop
      :ensure t
      :bind (("M-i" . helm-swoop)
             ("M-I" . helm-swoop-back-to-last-point)
             ("C-c M-i" . helm-multi-swoop))
      :config
      ;; When doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t
            ;; If this value is t, split window inside the current window
            helm-swoop-split-with-multiple-windows t
            ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
            helm-swoop-split-direction 'split-window-vertically
            ;; If nil, you can slightly boost invoke speed in exchange for text color
            helm-swoop-speed-or-color nil))

    helm-multi-swoop


<a id="orgb099b8b"></a>

## TODO Projectile and helm

Since implementing the Doom GC optimisation, the slight lag I
experienced while typing really fast in helm commands has
subsided. /However, there seems to be a slight lag in calling
describe-variable and describe-functions, which was not there
earlier/. The solution I am currently using is to switch to
`helm-appropos`, which apparently has some kind of mechanism to concat
large lists of commands, variables, functions. I think there is some
benefit in switching over to Helm for general options. This is an
experimental section to try this out. The article [Exploring large
projects with Projectile and Helm Projectile](https://tuhdo.github.io/helm-projectile.html) contains a large number of
tips in setting up projectile with helm.

    (setq projectile-completion-system 'helm)
    (setq projectile-indexing-method 'alien)
    (setq projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on)


<a id="org8e9a49b"></a>

## Copy simple without breaks

    ;; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
    ;; https://gist.github.com/xahlee/d364cbbff9b3abd12d29
    (defun my-copy-simple (&optional beg end)
      "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
      (interactive
       (if (region-active-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-end-position))))
      (let ((my-text (buffer-substring-no-properties beg end)))
        (with-temp-buffer
          (insert my-text)
          (goto-char 1)
          (while (looking-at "[ \t\n]")
            (delete-char 1))
          (let ((fill-column 9333999))
            (fill-region (point-min) (point-max)))
          (kill-region (point-min) (point-max)))))


<a id="org005096c"></a>

# Email


<a id="orgdaf1c2a"></a>

## TODO mu4e via use-package

I have mu installed via brew in my Mac OS.

-   [ ] Transfer all the settings for mu4e into a use-package layout with hooks.

    (use-package mu4e
    :straight t
    :ensure nil
    :hook
      ((mu4e-compose-mode . (lambda ()
                              (visual-line-mode)
                              (use-hard-newlines -1)
                              (flyspell-mode)
    			  (turn-off-auto-fill)))
       (mu4e-view-mode . (lambda() ;; try to emulate some of the eww key-bindings
                           (local-set-key (kbd "<tab>") 'shr-next-link)
                           (local-set-key (kbd "<backtab>") 'shr-previous-link)
    		       (fill-paragraph)
    		       (visual-line-mode)))
       (mu4e-headers-mode . (lambda ()
                              (interactive)
                              (setq mu4e-headers-fields
                                    `((:human-date . 25) ;; alternatively, use :date
                                      (:flags . 6)
                                      (:from . 22)
                                      (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                                      (:size . 7))))))
      :config
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
       message-kill-buffer-on-exit t
       mu4e-update-interval 500
       )
    
      ;; mu4e email refiling loations
      (setq
       mu4e-refile-folder "/Archive"
       mu4e-trash-folder  "/Trash"
       mu4e-sent-folder   "/Sent"
       mu4e-drafts-folder "/Drafts")
    
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
    
      (setq mu4e-view-use-gnus t)
    
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
       smtpmail-queue-dir  (expand-file-name "~/my_mail/fmail/Queue/cur")))


<a id="org0d04455"></a>

## TEST org-msg


<a id="orgdcddb88"></a>

### Basic setup

    (use-package org-msg
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
     -- \\\\
     *Shreyas Ragavan* \\\\
     E: shreyas@fastmail.com \\\\
     W: https://shreyas.ragavan.co \\\\
     M: +1 647-671-1851 \\\\
     #+end_signature")
    ;; Disabling the activation of org-msg mode. This can be used when
      ;; required.
      ;; (org-msg-mode)
      )
    ;; Attempt to solve the problem of forwarding emails especailly with attachments.
    ;(advice-add '(org-msg-mode) :after #'mu4e-compose-forward))


<a id="org0da1476"></a>

### Compose CSS

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-25 Wed 23:33] </span></span>   
    For some reason, the basic CSS does not look good. Even though some customisation is done below, it does not appear the font choice is respected. In any case, the size and line height are now acceptable.

    (defconst sr/org-msg-style
      (let* ((font-family '(font-family . "\"Calibri\""))
    	 (font-size '(font-size . "12pt"))
    	 (font `(,font-family ,font-size))
    	 (line-height '(line-height . "1.5em"))
    	 (bold '(font-weight . "bold"))
    	 (theme-color "#0071c5")
    	 (color `(color . ,theme-color))
    	 (table `(,@font (margin-top . "0px")))
    	 (ftl-number `(,@font ,color ,bold (text-align . "left")))
    	 (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
    			     fundamental ini json makefile man org plantuml
    			     python sh xml R))
    	 (inline-src `((color . ,(face-foreground 'default))
    		       (background-color . ,(face-background 'default))))
    	 (code-src
    	  (mapcar (lambda (mode)
    		    `(code ,(intern (concat "src src-" (symbol-name mode)))
    			   ,inline-src))
    		  inline-modes)))
      `((del nil (,@font (color . "grey") (border-left . "none")
    	      (text-decoration . "line-through") (margin-bottom . "0px")
    	      (margin-top . "10px") (line-height . "11pt")))
        (a nil (,color))
        (a reply-header ((color . "black") (text-decoration . "none")))
        (div reply-header ((padding . "3.0pt 0in 0in 0in")
    		       (border-top . "solid #e1e1e1 1.0pt")
    		       (margin-bottom . "20px")))
        (span underline ((text-decoration . "underline")))
        (li nil (,@font ,line-height (margin-bottom . "0px")
    	     (margin-top . "2px")))
        (nil org-ul ((list-style-type . "square")))
        (nil org-ol (,@font ,line-height (margin-bottom . "0px")
    		 (margin-top . "0px") (margin-left . "30px")
    		 (padding-top . "0px") (padding-left . "5px")))
        (nil signature (,@font (margin-bottom . "20px")))
        (blockquote nil ((padding-left . "5px") (margin-left . "10px")
    		     (margin-top . "20px") (margin-bottom . "0")
    		     (border-left . "3px solid #ccc") (font-style . "italic")
    		     (background . "#f9f9f9")))
        (code nil (,font-size (font-family . "monospace") (background . "#f9f9f9")))
        ,@code-src
        (nil linenr ((padding-right . "1em")
    		 (color . "black")
    		 (background-color . "#aaaaaa")))
        (pre nil ((line-height . "12pt")
    	      ,@inline-src
    	      (margin . "0px")
    	      (font-size . "9pt")
    	      (font-family . "monospace")))
        (div org-src-container ((margin-top . "10px")))
        (nil figure-number ,ftl-number)
        (nil table-number)
        (caption nil ((text-align . "left")
    		  (background . ,theme-color)
    		  (color . "white")
    		  ,bold))
        (nil t-above ((caption-side . "top")))
        (nil t-bottom ((caption-side . "bottom")))
        (nil listing-number ,ftl-number)
        (nil figure ,ftl-number)
        (nil org-src-name ,ftl-number)
    
        (table nil (,@table ,line-height (border-collapse . "collapse")))
        (th nil ((border . "1px solid white")
    	     (background-color . ,theme-color)
    	     (color . "white")
    	     (padding-left . "10px") (padding-right . "10px")))
        (td nil (,@table (padding-left . "10px") (padding-right . "10px")
    		     (background-color . "#f9f9f9") (border . "1px solid white")))
        (td org-left ((text-align . "left")))
        (td org-right ((text-align . "right")))
        (td org-center ((text-align . "center")))
    
        (div outline-text-4 ((margin-left . "15px")))
        (div outline-4 ((margin-left . "10px")))
        (h4 nil ((margin-bottom . "0px") (font-size . "11pt")
    	     ,font-family))
        (h3 nil ((margin-bottom . "0px") (text-decoration . "underline")
    	     ,color (font-size . "12pt")
    	     ,font-family))
        (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
    	     (font-style . "italic") ,color (font-size . "13pt")
    	     ,font-family))
        (h1 nil ((margin-top . "20px")
    	     (margin-bottom . "0px") ,color (font-size . "12pt")
    	     ,font-family))
        (p nil ((text-decoration . "none") (margin-bottom . "0px")
    	    (margin-top . "10px") (line-height . "11pt") ,font-size
    	    ,font-family (max-width . "100ch")))
        (div nil (,@font (line-height . "11pt"))))))


<a id="org88a8d67"></a>

### Set the css to the above custom function

    (setq org-msg-enforce-css 'sr/org-msg-style)


<a id="org8ef7fb0"></a>

# Programming customisations


<a id="orge383ee3"></a>

## Common editing modes

Source: [.emacs.d/init.el at master · zakandrewking/.emacs.d · GitHub](https://github.com/zakandrewking/.emacs.d/blob/master/init.el)

This is defined here so that settings specific to these modes could be
added more easily. For exmaple electric-pair mode.

    (defvar common-editing-modes
        (list 'latex-mode 'lisp-mode 'emacs-lisp-mode 'python-mode 'matlab-mode
        'sh-mode 'js2-mode 'markdown-mode 'haskell-mode 'org-mode 'c-mode 'css-mode
        'web-mode 'typescript-mode 'plantuml-mode))


<a id="org5c36641"></a>

## Electic pair mode only for specified modes

Source: [.emacs.d/init.el at master · zakandrewking/.emacs.d · GitHub](https://github.com/zakandrewking/.emacs.d/blob/master/init.el)

    ;; parens. automatically pair parens, but make it buffer local
    ;; http://emacs.stackexchange.com/questions/5981/how-to-make-electric-pair-mode-buffer-local
    (defun my-inhibit-electric-pair-mode (char)
      (not (member major-mode common-editing-modes)))
    (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)
    (electric-pair-mode 1)


<a id="orgc053148"></a>

## flyspell with markdown

Source:  [How to automatically load flyspell-mode when Markdown (.md, .mdwn) file is open? - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17266/how-to-automatically-load-flyspell-mode-when-markdown-md-mdwn-file-is-open)

    (add-hook 'markdown-mode-hook 'flyspell-mode)


<a id="org07eca57"></a>

## eshell

The following is borrowed from [howardabrams/dot-files · GitHub](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org). The ruby
functions are commented out since I do not use ruby.


<a id="org67ef3d9"></a>

### configuration

    (use-package eshell
      :init
      (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
            eshell-scroll-to-bottom-on-input 'all
            eshell-error-if-no-glob t
            eshell-hist-ignoredups t
            eshell-save-history-on-exit t
            eshell-prefer-lisp-functions nil
            eshell-destroy-buffer-when-process-dies t))


<a id="org76bc107"></a>

### Visual executables

    (use-package eshell
      :init
     (add-hook 'eshell-mode-hook
                (lambda ()
                  (add-to-list 'eshell-visual-commands "ssh")
                  (add-to-list 'eshell-visual-commands "tail")
                  (add-to-list 'eshell-visual-commands "top"))))


<a id="org49eee4a"></a>

### Aliases

    (add-hook 'eshell-mode-hook (lambda ()
        (eshell/alias "e" "find-file $1")
        (eshell/alias "ff" "find-file $1")
        (eshell/alias "emacs" "find-file $1")
        (eshell/alias "ee" "find-file-other-window $1")
    
        (eshell/alias "gd" "magit-diff-unstaged")
        (eshell/alias "gds" "magit-diff-staged")
        (eshell/alias "d" "dired $1")
    
        ;; The 'ls' executable requires the Gnu version on the Mac
        (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                      "/usr/local/bin/gls"
                    "/bin/ls")))
          (eshell/alias "ll" (concat ls " -AlohG --color=always")))))


<a id="org1234894"></a>

### Find file and edit

    (defun eshell/f (filename &optional dir try-count)
      "Searches for files matching FILENAME in either DIR or the
    current directory. Just a typical wrapper around the standard
    `find' executable.
    
    Since any wildcards in FILENAME need to be escaped, this wraps the shell command.
    
    If not results were found, it calls the `find' executable up to
    two more times, wrapping the FILENAME pattern in wildcat
    matches. This seems to be more helpful to me."
      (let* ((cmd (concat
                   (executable-find "find")
                   " " (or dir ".")
                   "      -not -path '*/.git*'"
                   " -and -not -path '*node_modules*'"
                   " -and -not -path '*classes*'"
                   " -and "
                   " -type f -and "
                   "-iname '" filename "'"))
             (results (shell-command-to-string cmd)))
    
        (if (not (s-blank-str? results))
            results
          (cond
           ((or (null try-count) (= 0 try-count))
            (eshell/f (concat filename "*") dir 1))
           ((or (null try-count) (= 1 try-count))
            (eshell/f (concat "*" filename) dir 2))
           (t "")))))
    
    (defun eshell/ef (filename &optional dir)
      "Searches for the first matching filename and loads it into a
    file to edit."
      (let* ((files (eshell/f filename dir))
             (file (car (s-split "\n" files))))
        (find-file file)))


<a id="orgd120843"></a>

### git repo

    (defun curr-dir-git-branch-string (pwd)
      "Returns current git branch as a string, or the empty string if
    PWD is not in a git repo (or the git command is not found)."
      (interactive)
      (when (and (not (file-remote-p pwd))
                 (eshell-search-path "git")
                 (locate-dominating-file pwd ".git"))
        (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
               (git-repo (file-name-base (s-trim git-url)))
               (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
               (git-branch (s-trim git-output))
               (git-icon  "\xe0a0")
               (git-icon2 (propertize "\xf020" 'face `(:family "octicons"))))
          (concat git-repo " " git-icon2 " " git-branch))))


<a id="org32daf7a"></a>

### Replacing $HOME with tilde

    (defun pwd-replace-home (pwd)
      "Replace home in PWD with tilde (~) character."
      (interactive)
      (let* ((home (expand-file-name (getenv "HOME")))
             (home-len (length home)))
        (if (and
             (>= (length pwd) home-len)
             (equal home (substring pwd 0 home-len)))
            (concat "~" (substring pwd home-len))
          pwd)))


<a id="org2a8362f"></a>

### shorter directory names

    (defun pwd-shorten-dirs (pwd)
      "Shorten all directory names in PWD except the last two."
      (let ((p-lst (split-string pwd "/")))
        (if (> (length p-lst) 2)
            (concat
             (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                   (substring elm 0 1)))
                        (butlast p-lst 2)
                        "/")
             "/"
             (mapconcat (lambda (elm) elm)
                        (last p-lst 2)
                        "/"))
          pwd)))  ;; Otherwise, we just return the PWD


<a id="org1d825f6"></a>

### break up directory into parent and base

    (defun split-directory-prompt (directory)
      (if (string-match-p ".*/.*" directory)
          (list (file-name-directory directory) (file-name-base directory))
        (list "" directory)))


<a id="org3911e7d"></a>

### virtual environments

This below is for working with pyenv.

    (defun python-prompt ()
      "Returns a string (may be empty) based on the current Python
       Virtual Environment. Assuming the M-x command: `pyenv-mode-set'
       has been called."
      (when (fboundp #'pyenv-mode-version)
        (let ((venv (pyenv-mode-version)))
          (when venv
            (concat
             (propertize "\xe928" 'face `(:family "alltheicons"))
             (pyenv-mode-version))))))


<a id="org2345eae"></a>

### Tie up

    (defun eshell/eshell-local-prompt-function ()
      "A prompt for eshell that works locally (in that is assumes
    that it could run certain commands) in order to make a prettier,
    more-helpful local prompt."
      (interactive)
      (let* ((pwd        (eshell/pwd))
             (directory (split-directory-prompt
                         (pwd-shorten-dirs
                          (pwd-replace-home pwd))))
             (parent (car directory))
             (name   (cadr directory))
             (branch (curr-dir-git-branch-string pwd))
    
    
    	 ;; (ruby   (when (not (file-remote-p pwd)) (ruby-prompt)))
             (python (when (not (file-remote-p pwd)) (python-prompt)))
    
             (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
             (for-bars                 `(:weight bold))
             (for-parent  (if dark-env `(:foreground "dark orange") `(:foreground "blue")))
             (for-dir     (if dark-env `(:foreground "orange" :weight bold)
                            `(:foreground "blue" :weight bold)))
             (for-git                  `(:foreground "green"))
             (for-ruby                 `(:foreground "red"))
             (for-python               `(:foreground "#5555FF")))
    
        (concat
         (propertize "⟣─ "    'face for-bars)
         (propertize parent   'face for-parent)
         (propertize name     'face for-dir)
         (when branch
           (concat (propertize " ── "    'face for-bars)
                   (propertize branch   'face for-git)))
         ;; (when ruby
         ;;   (concat (propertize " ── " 'face for-bars)
         ;;           (propertize ruby   'face for-ruby)))
         (when python
           (concat (propertize " ── " 'face for-bars)
                   (propertize python 'face for-python)))
         (propertize "\n"     'face for-bars)
         (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
         ;; (propertize " └→" 'face (if (= (user-uid) 0) `(:weight ultra-bold :foreground "red") `(:weight ultra-bold)))
         (propertize " "    'face `(:weight bold)))))
    
    (setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)


<a id="orgab3c39d"></a>

### eshell here

    (defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (height (/ (window-total-height) 3))
             (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))
    
        (insert (concat "ls"))
        (eshell-send-input)))
    
    (bind-key "C-!" 'eshell-here)


<a id="orge629719"></a>

### C-d behavior

    (use-package eshell
      :config
      (defun ha/eshell-quit-or-delete-char (arg)
        (interactive "p")
        (if (and (eolp) (looking-back eshell-prompt-regexp))
            (progn
              (eshell-life-is-too-much) ; Why not? (eshell/exit)
              (ignore-errors
                (delete-window)))
          (delete-forward-char arg)))
      :init
      (add-hook 'eshell-mode-hook
                (lambda ()
                  (bind-keys :map eshell-mode-map
                             ("C-d" . ha/eshell-quit-or-delete-char)))))


<a id="org5a35b7a"></a>

## shell-pop - ansi term

Found this [on a pragmatic emacs blog post](http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/). The issue at the moment is
the hijacking of the C-t shortcut, and also that once popped, the shell
buffer does not close. 

Eshell is useful for many purposes, however at times, there are commands
and operations which work better on an emulator. `ansi-term` seems to be
more useful than `term`.

    (use-package shell-pop
      :bind (("C-T" . shell-pop))
      :config
      (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
      (setq shell-pop-term-shell "/bin/zsh")
      ;; need to do this manually or not picked up by `shell-pop'
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))


<a id="org17cbeb1"></a>

## orgit

I would like to be able to create links to specific commits in
repos. Using orgit, I need to open the particular commit buffer and then
use an org-store-link command to store the link to the particular
revision.

    (use-package orgit
         :straight (orgit :type git :host github :repo "magit/orgit"))


<a id="orgc9d4446"></a>

## TODO realgud

Apparently this packages enhances the debuggers of many languages. Yet
to check out how this will help me. Elpy by itself has a bunch of
shortcuts with respect to python.

    (straight-use-package 'realgud)


<a id="orgb43850d"></a>

## emmet-mode

    (use-package emmet-mode
    :straight t
    :config
    
    ;; Auto-start on any markup modes
    (add-hook 'sgml-mode-hook 'emmet-mode)
    ;; enable Emmet's css abbreviation.
    (add-hook 'css-mode-hook  'emmet-mode)
    ;; enable HTML
    (add-hook 'html-mode-hook  'emmet-mode)
    ;; Enable cursor to be placed within quotes
    (setq emmet-move-cursor-between-quotes t)
    )


<a id="org9e85d8e"></a>

## web-mode

    (straight-use-package 'web-mode)
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

Forcing django engine for now

    (setq web-mode-engines-alist
          '(("django"    . "\\.html\\'")))


<a id="org2da482b"></a>

## ESS configuration

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


<a id="org9489c0e"></a>

## lispy

    ;; Superior lisp editing
    (use-package lispy
      :config
      (dolist (hook '(emacs-lisp-mode-hook
    		  hy-mode-hook))
        (add-hook hook
    	      (lambda ()
    		(lispy-mode)
    		(eldoc-mode)))))


<a id="org5462353"></a>

## flycheck

    (straight-use-package 'flycheck)


<a id="orgd062c95"></a>

## TODO Python <code>[0/1]</code>

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-23 Mon 18:10] </span></span>   
    For now I will disable this as it appears logical to use pyenv and
    other packages to set the correct interpreter. However, since shims
    are being used, one would assume that the correct environment will be
    used anyway.

At the moment, [Scimax packages](.emacs.d::init.org::#MY Scimax packages) require pydoc, elpy and jedi, which are already installed. These packages have to be grouped here.

[Emacs: The Best Python Editor? Real Python](https://realpython.com/emacs-the-best-python-editor/) is a good guide towards
setting up Emacs and python. [Managing a Python development environment
in Emacs - Analytics Vidhya - Medium](https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a) provides a satisfyingly detailed
configuration with customisations for python. The latter guide is worth
following.


<a id="orgb12ae23"></a>

### EIN notebooks

-   [ ] enable visual line mode?
-   [ ] Enable pictures by default
-   [ ] possible for better latex rendering?

    (use-package ein
    :defer t
    :ensure t
    :config
    (package-initialize)
    (require 'ein)
    (require 'ein-notebook)
    (require 'ein-subpackages))


<a id="org9e7a39a"></a>

### emacs-lsp

    (use-package lsp-python-ms
      :ensure t
      :hook (python-mode . (lambda ()
                              (require 'lsp-python-ms)
                              (lsp))))  ; or lsp-deferred

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">(lambda nil (require 'lsp-python-ms) (lsp))</td>
<td class="org-left">er/add-python-mode-expansions</td>
<td class="org-left">blacken-mode</td>
<td class="org-left">elpy-mode</td>
</tr>
</tbody>
</table>


<a id="org6cd1e89"></a>

### pyvenv, pydoc and elpy

    (straight-use-package 'pyvenv)
    (straight-use-package 'pydoc)
    
    (use-package elpy
      :straight t
      :config
      (elpy-enable))


<a id="orge983b92"></a>

### Blacken

The black library will re-format the code on a save. Elpy will automatically use black, as long as the elisp package installed, along with the python black package which can be installed using pip

    pip3 install black

    (use-package blacken
    :straight t
    :hook (python-mode . blacken-mode))


<a id="orgc633c00"></a>

# Docker

The following packages will enable me to handle docker containers right within Emacs.


<a id="org39a702c"></a>

### Docker package

With this package, I can easily view the containers available and run commands on them from emac without switching over the terminal.

    (use-package docker
      :ensure t
    
      :bind ("M-s d" . docker)
    )


<a id="orgf1ce2fa"></a>

# TODO Custom search functions <code>[0/1]</code>

This section contains sections to search in specific directories, with customised options.

-   [ ] Incorporate these functions into a hydra.
    -   [ ] Explore ldlework's hera and nougat hydra packages for this. Perhaps include into scimax under the applications menu.


<a id="orgdcd002a"></a>

## Episteme search

Adapted from ldlework

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


<a id="orgb597f61"></a>

## Projects search <code>[1/6]</code>

-   [ ] Exclude git files
-   [ ] Exclude csv files
-   [ ] Exclude html and related files
-   [X] decide between helm-ag and counsel-ag and the method to jump between files.
-   [ ] Decide about plugging in arguments to activate different kinds of search.
-   [ ] Find if multiple paths can be included or not.

    (defun sr/fun/proj-search ()
      (interactive)
      (helm-do-ag (sr/fun/project-dir "")))


<a id="org192403f"></a>

## Org files search

    (defun sr/fun/org-search ()
      (interactive)
      (helm-do-ag (sr/fun/org-dir "")))


<a id="org8f75890"></a>

## Journal files search

    (defun sr/fun/org-journal-search ()
      (interactive)
      (helm-do-ag (sr/fun/org-dir "journal")))


<a id="org0c5856f"></a>

## Downloads and Desktop search


<a id="orgd19d270"></a>

# Pastes and gists


<a id="org9b3700f"></a>

## Webpaste

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-29 Sun 11:18] </span></span>   
    This package is useful to use multiple paste services with a fallback. dpaste has a pleasant format and therefore chosen as the first priority. The advantage of using github gists is that org mode exports can be directly rendered. This package can also be configured to use a custom paste service.

    (use-package webpaste
      :ensure t
      :config
      (progn
        (setq webpaste-provider-priority '("dpaste.org" "ix.io"))))


<a id="org2722cab"></a>

## github gists

Gists posted right from emacs.

    (use-package gist
    :straight (gist :type git :host github :repo "defunkt/gist.el"))


<a id="orgb189145"></a>

# TODO Hydra Hera and nougat

This hydra setup is based off hera and nougat, which are packages
available in [ldlework's init.el](https://dustinlacewell.github.io/emacs.d/). I've adapted, modified and expanded his
setup. These cool packages make defining hydras a little easier,
especially in a stack.


<a id="org4c8d2f0"></a>

## hera

ldlework's package that provides an API for defining a stack of hydras.

    (use-package hera
    :demand t
    :straight (hera :type git :host github :repo "dustinlacewell/hera"))


<a id="org4406392"></a>

## nougat-hydra

    (defun nougat--inject-hint (symbol hint)
    (-let* ((name (symbol-name symbol))
    (hint-symbol (intern (format "%s/hint" name)))
    (format-form (eval hint-symbol))
    (string-cdr (nthcdr 1 format-form))
    (format-string (string-trim (car string-cdr)))
    (amended-string (format "%s\n\n%s" format-string hint)))
    (setcar string-cdr amended-string)))
    
    (defun nougat--make-head-hint (head default-color)
    (-let (((key _ hint . rest) head))
    (when key
    (-let* (((&plist :color color) rest)
    (color (or color default-color))
    (face (intern (format "hydra-face-%s" color)))
    (propertized-key (propertize key 'face face)))
    (format " [%s]: %s" propertized-key hint)))))
    
    (defun nougat--make-hint (heads default-color)
    (string-join
    (cl-loop for head in heads
    for hint = (nougat--make-head-hint head default-color)
    do (pp hint)
    collect hint) "\n"))
    
    (defun nougat--clear-hint (head)
    (-let* (((key form _ . rest) head))
    `(,key ,form nil ,@rest)))
    
    (defun nougat--add-exit-head (heads)
    (let ((exit-head '("SPC" (hera-pop) "to exit" :color blue)))
    (append heads `(,exit-head))))
    
    (defun nougat--add-heads (columns extra-heads)
    (let* ((cell (nthcdr 1 columns))
    (heads (car cell))
    (extra-heads (mapcar 'nougat--clear-hint extra-heads)))
    (setcar cell (append heads extra-heads))))
    
    (defmacro nougat-hydra (name body columns &optional extra-heads)
    (declare (indent defun))
    (-let* (((&plist :color default-color :major-mode mode) body)
    (extra-heads (nougat--add-exit-head extra-heads))
    (extra-hint (nougat--make-hint extra-heads default-color))
    (body (plist-put body :hint nil))
    (body-name (format "%s/body" (symbol-name name)))
    (body-symbol (intern body-name))
    (mode-support
    `(when ',mode
    (setq major-mode-hydra--body-cache
    (a-assoc major-mode-hydra--body-cache ',mode ',body-symbol)))))
    (nougat--add-heads columns extra-heads)
    (when mode
    (remf body :major-mode))
    `(progn
    (pretty-hydra-define ,name ,body ,columns)
    (nougat--inject-hint ',name ,extra-hint)
    ,mode-support)))
    
    ;; (nougat-hydra hydra-test (:color red :major-mode fundamental-mode)
    ;; ("First"
    ;; (("a" (message "first - a") "msg a" :color blue)
    ;; ("b" (message "first - b") "msg b"))
    ;; "Second"
    ;; (("c" (message "second - c") "msg c" :color blue)
    ;; ("d" (message "second - d") "msg d"))))


<a id="org2e6f137"></a>

## hydra-dwim

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-04 Sat 17:17] </span></span>   
    I still need to integrate these hydras with scimax hydras and figure out
    a good key for the exit.

    (defun my/hydra-dwim ()
    (interactive)
    (-let (((&alist major-mode mode) major-mode-hydra--body-cache))
    (if mode (major-mode-hydra)
    (hera-start 'hydra-default/body))))
    
    (setq kbd-hera-pop "Q")
    (global-set-key (kbd "H-l") 'my/hydra-dwim)
    (global-set-key (kbd "H-l") (lambda () (interactive) (hera-start 'hydra-default/body)))


<a id="orgeeb6606"></a>

## hydra-default

    (defhydra hydra-default (:color blue :hint nil)
    "
    
    Entrypoint Hydra
    
    "
    ("a" (org-agenda nil "a") "agenda" :column "Open")
    ;; ("p" (hera-push 'hydra-projectile/body) "projectile")
    ("c" (org-capture) "capture")
    ("t" (hera-push 'hydra-treemacs/body))
    ("f" (hera-push 'hydra-functions/body) "Functions" :column "Emacs")
    ("m" (hera-push 'hydra-mu4e/body) "mail")
    ("w" (hera-push 'hydra-window/body) "windows")
    ("z" (hera-push 'hydra-zoom/body) "zoom")
    ("R" (hera-push 'hydra-registers/body) "registers")
    ("n" (hera-push 'hydra-notes/body) "notes" :column "Misc")
    ("S" (hera-push 'hydra-straight/body) "Straight")
    ("s" (call-interactively 'helm-imenu) "semantic")
    ("g" (hera-push 'hydra-gist/body) "gist")
    ("p" (hera-push 'hydra-pyvenv/body) "pyvenv" :column "python")
    ("j" (hera-push 'hydra-journal/body) "Journal" :column "Daily")
    ("o" (hera-push 'hydra-clock/body) "Clock" :column "Daily")
    ("l" (hera-push 'hydra-links/body) "Links")
    ("b" (hera-push 'hydra-brain/body) "brain"))


<a id="org01f4d91"></a>

## hydra-straight

    (defun sr/fun/straight-pull-rebuild-combo (&optional package)
      "Function to select a package, and then -> pull and rebuild using straight."
      (interactive)
      (let ((package (or package
                         (straight--select-package "Pull & Rebuild package" 'for-build 'installed))))
        (message package)
        (straight-pull-package package)
        (straight-rebuild-package package)))
    
    
    (nougat-hydra hydra-straight (:color red)
      ("Straight" (("P" (call-interactively 'straight-pull-package)
    		"Pull one")
    	       ("p" (call-interactively 'sr/fun/straight-pull-rebuild-combo)
    		"Pull and Rebuild one package" :column "Pull")
    	       ("C-a" (straight-pull-all) "Pull ALL")
    	       ("f" (call-interactively 'straight-fetch-package)
    		"Fetch one")
    	       ("F" (straight-fetch-all) "Fetch ALL")
    	       ("w" (call-interactively 'straight-visit-package-website) "visit package Website")
    	       ("g" (call-interactively 'straight-get-recipe)
    		"Get recipe")
    	     ("r" (call-interactively 'straight-rebuild-package)
    		"rebuild package")
    	       )))


<a id="orge66ce76"></a>

## hydra-mu4e

    (nougat-hydra hydra-mu4e (:color blue)
    ("mu4e" (("m" (mu4e) "mail")
    	     ("u" (mu4e-update-mail-and-index) "Update mail and index"))))


<a id="org2e98931"></a>

## hydra-pyvenv

    (nougat-hydra hydra-pyvenv (:color red)
      ("pyvenv - virtualenv" (("c" (call-interactively 'pyvenv-create)
    		"create")
    	       ("w" (call-interactively 'pyvenv-workon) "work on")
    	       ("a" (call-interactively 'pyvenv-activate)	"activate")
    	       ("d" (call-interactively 'pyvenv-deactivate) "deactivate")
    	       ("t" (call-interactively 'pyvenv-tracking-mode) "tracking mode")
    	       ("r" (call-interactively 'pyvenv-restart-python) "restart python")
    	       )))


<a id="org97b6d01"></a>

## hydra-window

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-08 Wed 12:34] </span></span>   
    For some reason, ace commands for windows ask for a 2nd number even with
    only 2 windows in the frame. This may be because the buffer is open in
    another frame as well. THis has to be fixed.

    (use-package ace-window)
    (winner-mode 1)
    
    (nougat-hydra hydra-window (:color red)
    ("Jump"
    (("h" windmove-left "left")
    ("l" windmove-right "right")
    ("k" windmove-up "up")
    ("j" windmove-down "down")
    ("a" ace-select-window "ace")
    ("s" ace-swap-window "ace-swap"))
    "Split"
    (("q" split-window-right "left")
    ("r" (progn (split-window-right) (call-interactively 'other-window)) "right")
    ("e" split-window-below "up")
    ("w" (progn (split-window-below) (call-interactively 'other-window)) "down"))
    "Do"
    (("d" delete-window "delete")
    ("o" delete-other-windows "delete others")
    ("u" winner-undo "undo")
    ("R" winner-redo "redo")
    ("t" nougat-hydra-toggle-window "toggle"))))

Toggle window split

    (defun my/toggle-window-split (&optional arg)
    "Switch between 2 windows split horizontally or vertically.
    With ARG, swap them instead."
    (interactive "P")
    (unless (= (count-windows) 2)
    (user-error "Not two windows"))
    ;; Swap two windows
    (if arg
    (let ((this-win-buffer (window-buffer))
    (next-win-buffer (window-buffer (next-window))))
    (set-window-buffer (selected-window) next-win-buffer)
    (set-window-buffer (next-window) this-win-buffer))
    ;; Swap between horizontal and vertical splits
    (let* ((this-win-buffer (window-buffer))
    (next-win-buffer (window-buffer (next-window)))
    (this-win-edges (window-edges (selected-window)))
    (next-win-edges (window-edges (next-window)))
    (this-win-2nd (not (and (<= (car this-win-edges)
    (car next-win-edges))
    (<= (cadr this-win-edges)
    (cadr next-win-edges)))))
    (splitter
    (if (= (car this-win-edges)
    (car (window-edges (next-window))))
    'split-window-horizontally
    'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
    (funcall splitter)
    (if this-win-2nd (other-window 1))
    (set-window-buffer (selected-window) this-win-buffer)
    (set-window-buffer (next-window) next-win-buffer)
    (select-window first-win)
    (if this-win-2nd (other-window 1))))))


<a id="org0969fe8"></a>

## hydra-treemacs

    (nougat-hydra hydra-treemacs (:color red)
    ("Workspace"
    (("t" treemacs "Treemacs buffer" :color red)
    ("o" treemacs-switch-workspace "switch workspace" :color red)
    ("n" treemacs-create-workspace "new workspace" :color red)
    ("k" treemacs-delete-workspace "kill")
    ("r" treemacs-rename-workspace "rename")
    ("c" treemacs-collapse-other-projects "collapse other projects" :color blue))))


<a id="org0ec9ad7"></a>

## hydra-journal

    (nougat-hydra hydra-journal (:color blue)
    ("Journal"
    (("j"  org-journal-new-entry "Entry")
     ("n" org-journal-open-next-entry "Next entry" :color red)
     ("p" org-journal-open-previous-entry "Previous entry" :color red)
     ("F" org-journal-new-scheduled-entry "Future scheduled")
     ("s" sr/fun/org-journal-search "Journal search")
     ("S" sr/fun/org-search "Org directory search"))))
    
    (global-set-key (kbd "H-j") (lambda () (interactive) (hera-start 'hydra-journal/body)))


<a id="org30903fb"></a>

## hydra-clock

-   [ ] I will need to integrate this down the line with shortcuts for org mode.

    (nougat-hydra hydra-clock (:color blue)
    ("clock"
    (("j" counsel-org-clock-goto "clock go to current/last" :color red)
    ("i" counsel-org-clock-history "counsel clock in/historyo" :color blue)
    ("c" counsel-org-clock-context "counsel clock context" :color blue)
    ("o" org-clock-out "clock out" :color red)
    ("h" org-mru-clock-in "mru clock in" :color blue))))


<a id="org79adeb4"></a>

## hydra-links

-   [ ] [Building personal search infrastructure for your knowledge and
    code | beepb00p](https://beepb00p.xyz/pkm-search.html#web) has  great list of search terms for different search
    engines. This is worth implementing directly.

    (nougat-hydra hydra-links (:color blue)
    ("links"
    (("y" yt "youtube music playlist" :color blue)
    ("h" hn "hacker news" :color blue)
    ("w" wikipedia-search "wikipedia" :color blue)
    ("g" counsel-google "counsel web search" :color blue)
    ("G" google "Launch google" :color blue)
    ("d" define-word "define word" :color red)
    ("D" lookup-word "wiktionary lookup" :color red)
    ("m" mw-thesaurus-lookup-at-point "Merriam-webster thesaurus" :color red))))


<a id="org9b70c08"></a>

## hydra-brain

    (nougat-hydra hydra-brain (:color blue)
    ("brain"
    (("b" helm-brain "helm-brain" :color blue)
    ("s" helm-org-rifle-brain "helm rifle brain" :color blue)
    ("r" org-brain-refile "refile to brain" :color blue)
    ("v" org-brain-visualize "Org brain visualize" :color blue))))


<a id="org63a254f"></a>

## hydra-functions

This is an attempt to collect some functions that I presume to use on a
frequent basis, to the extent that having a hydra is reasonably less
typing on finding the command with M-x.

It is worth noting here that while some keys may be long, but the virtue
of head color the hydra commands can then be repeated as many times as
required.

Another benefit for example, is disabling the other keymaps for
eyebrowse and restoring the org-refile command

    (nougat-hydra hydra-functions (:color blue)
    ("functions"
    (("j" helm-org-rifle "helm org rifle" :color blue)
    ("r" org-refile "org refile" :color blue)
    ("hb" helm-org-rifle-current-buffer "helm org rifle current buffer" :color red)
    ("hsp" helm-multi-swoop-projectile "helm multi swoop projectile" :color red)
    ("hsm" helm-multi-swoop "helm multi swoop" :color blue)
    ("en" eyebrowse-rename-window-config "eyebrowse rename" :color red)
    ("ec" eyebrowse-create-window-config "eyebrowse create" :color red)
    ("es" eyebrowse-switch-to-window-config "eyebrowse switch" :color blue)
    ("[" winner-undo "winner UNDO" :color red)
    ("]" winner-redo "winner REDO" :color red)
    ("sp" sr/fun/proj-search "Ag PROJECTS directory" :color blue)
    ("so" sr/fun/org-search "Ag ORG directory" :color blue))))


<a id="orgeee90d9"></a>

## TODO hydra-gists and pastes

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-05-23 Sat 11:02] </span></span>   
    The paste region does not seem to work as expected. The mistake seems to
    be that the region is not being supplied via the hydra. However, the
    command works as expected when accessed via M-x.

    (nougat-hydra hydra-gist (:color blue)
    ("Gist and Pastes" (("g" (gist-region-or-buffer) "public gist")
    ("G" (gist-region-or-buffer-private) "private gist")
    ("b" (browse-url "https://gist.github.com/shrysr") "browse gists")
    ("pb" (webpaste-paste-buffer) "webpaste buffer")
    ("pr" (webpaste-paste-buffer-or-region) "webpaste selected region"))))


<a id="orgd365422"></a>

# TODO org-id setup

Using the org-id for reference to headings ensures that even if the
heading changes, the links will still work.

In addition, I would like an org id to be created every time the capture
is used. This facilitates using packages like org-brain which rely
extensively on org-id's.

    (require 'org-id)
    (setq org-id-link-to-org-use-id t)
    ;; (org-link-set-parameters "id" :store #'org-id-store-link)
    ;; (org-link-set-parameters "nb" :store nil)
    (org-link-set-parameters 'nil)
    ;; (org-link-set-parameters "nb" :store nil)
    ;; ;; Update ID file .org-id-locations on startup
    ;; ;; This adds too much time to startup
    ;; ;; (org-id-update-id-locations)
    
    (setq org-id-method (quote uuidgen))
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/my_org/emacs_meta/.org-id-locations")
    ;; (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)


<a id="org4b7128d"></a>

# eww


<a id="org4275d5a"></a>

### Default browser to be eww and basic settings

    (setq browse-url-browser-function 'eww-browse-url)


<a id="org2b657eb"></a>

### Keyboard map for default external browser

    ;; Open the current URL in the default external browser
    (eval-after-load 'eww
      '(progn
         (define-key eww-mode-map "o" 'eww-browse-with-external-browser)
         ))


<a id="org0893707"></a>

### Wikipedia search

    (defun wikipedia-search (search-term)
      "Search for SEARCH-TERM on wikipedia"
      (interactive
       (let ((term (if mark-active
                       (buffer-substring (region-beginning) (region-end))
                     (word-at-point))))
         (list
          (read-string
           (format "Wikipedia (%s):" term) nil nil term)))
       )
      (browse-url
       (concat
        "http://en.m.wikipedia.org/w/index.php?search="
        search-term
        ))
      )


<a id="orge0bb146"></a>

### Access Hacker News

    (defun hn ()
      (interactive)
      (browse-url "http://news.ycombinator.com"))


<a id="org3fab18a"></a>

### Youtube music playlist

    (defun yt ()
      (interactive)
      (browse-url "https://www.youtube.com/playlist?list=PLzDQK0UVrg2TiemiFoRmrwq-YbQ-Zn1AN"))


<a id="orgf89831a"></a>

### TODO Open specific browser depending on the URL

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-05-11 Mon 20:57] </span></span>   
    I find that it is better to use eww as the default browser to maximise
    my use. From eww it is easier to access other websites.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-26 Sun 06:46] </span></span>   
    The idea here is to gather the websites and links which work fine on eww
    and have the remaining links open directly on the default web browser.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-03-07 Thu 11:59] </span></span>   
    This is worth setting up. It would be convenient for frequently visited websites like reddit and others, to open in the external browser, especially as they do not render well within w3m.

Source : <http://ergoemacs.org/emacs/emacs_set_default_browser.Html>

    ;; use browser depending on url
    (setq
     browse-url-browser-function
     '(
      ("wikipedia\\.org" . eww-browse-url)
      ("wiktionary\\.org" . eww-browse-url)
      ("github" . eww-browse-url)
      ("thefreedictionary\\.com" . eww-browse-url)
      ("link\\.wired\\.com" . eww-browse-url)
      ("links\\.gatesnotes\\.com" . eww-browse-url)
      ("bitbucket\\.org\\blog*+" . eww-browse-url)
      ("dev\\.to\*+" . eww-browse-url)
    ;;  ("." . browse-url-default-browser)
      ("." . eww-browse-url)
      ("youtube" . browse-url-default-macosx-browser)))


<a id="org8fa2b2e"></a>

# New Scimax port


<a id="org291eaa0"></a>

## org-ref

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-11 Wed 23:18] </span></span>   
    Apparently, the scimax org-ref module required the gitter package. This is strange because it is pulled in as a git submodule. However, the issue was resolved  when org-ref was pulled in from it's own repo.

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


<a id="org1238e72"></a>

## scimax-org port


<a id="org0f55934"></a>

### General Org mode related

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


<a id="org793a870"></a>

### Babel settings

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
       (matlab . nil)
       (sqlite . t)
       (ruby . nil)
       (perl . t)
       (org . t)
       (dot . t)
       (plantuml . t)
       (R . t)
       (fortran . nil)
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


<a id="org4845e54"></a>

### Org formatting functions

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


<a id="org6755e6e"></a>

### Links and Jumping functions

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


<a id="orgff79ca8"></a>

### Better return

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


<a id="org5e9e270"></a>

### TODO Numbered headings and overlays


<a id="orga4f0558"></a>

### TODO PDF and EPS images in org mode


<a id="org9766bc8"></a>

## scimax-ivy

      (use-package scimax-ivy
        :straight (scimax-ivy :local-repo "scimax-subset" :files ("scimax-ivy.el")))
    (require 'scimax-ivy)


<a id="org856bcba"></a>

## scimax-apps

      (use-package scimax-apps
        :after scimax-org
        :straight (scimax-apps :local-repo "scimax-subset" :files ("scimax-apps.el")))
    (require 'scimax-apps)


<a id="orgc742d09"></a>

## TODO scimax-hydra

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-03-24 Tue 10:38] </span></span>   
    For some reason, the scimax-hydra package will not byte compile.

    (load-file (concat (sr/fun/emacs-dir "straight/repos/scimax-subset/scimax-hydras.el")))
    
    ;; (use-package scimax-hydra
    ;;   :straight (scimax-hydra :local-repo "scimax-subset" :files ("scimax-hydras.el"))
    ;;   :bind ("<f12>" . scimax/body))
    ;; (require 'scimax-hydras)


<a id="orgc1d0e3a"></a>

## scimax-notebook

    
    (use-package scimax-notebook
      :straight (scimax-notebook :local-repo "scimax-subset" :files ("scimax-notebook.el"))
      :bind ("M-s n" . 'nb-hydra/body)
      :config
    (setq nb-notebook-directory "~/my_projects/"))
    ;; (global-set-key (kbd "M-s n") 'nb-hydra/body))
    (require 'scimax-notebook)


<a id="orga1d5510"></a>

## scimax-ipython

-   [X] use the file keyword to add the client.py file as a symlink. Note here that the entire list of files have to be provided.

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

scimax-ipython upstream

    (require 'scimax-org-babel-ipython-upstream)
    (require 'scimax-ob)


<a id="orga5bc83d"></a>

# Aesthetics

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2020-04-24 Fri 20:06] </span></span>   
    I've tried quite a few themes over the years, and have even had phases
    where I was constantly changing themes. One caveat I've noticed is that
    not all themes are designed to work well with the terminal as
    well. After yet another quick foray into the Doom themes today - I will
    settle down on the following themes to be the best for me (note that
    relatively minor customisations like the font, proportional headings and
    so on are made to all)
    1.  Modus operandi: well designed light theme that does not hurt my eyes
        and offers a good basic configuration. This theme appears to work
        well with terminal emacs as well.
    2.  Zenburn : As many say, this theme is definitely the easiest on my
        eyes and will be a preferred dark theme. There are a tonne of
        configuration options out of the box.
    3.  Modus vivendi: This is a good dark theme that I prefer since it is
        based on similar design principles as the modus operandi. However,
        the contrast is jarring as compared to the mute nature of the Zenburn
        theme. I would not want to use the Modus vivendi theme for long
        durations in low light.
    4.  Leuven: I used this theme for a long time when I was on
        Scimax. Undoubtedly this light theme has its advantages, however, it
        seems a little too opinionated on a lot of things, and I would prefer
        a lighter theme that also plays well on terminals.
    5.  Doom's solarized dark theme: this is an honorable mention. As a dark theme
        that does not hurt the themes - this is similar to zenburn, but not
        as good looking.


<a id="org2af97d9"></a>

## Setting custom themes to be safe

    (setq custom-safe-themes t)


<a id="orgad5550c"></a>

## Theme : modus


<a id="org084eab3"></a>

### modus operandi

    (straight-use-package 'modus-operandi-theme)
    
    (use-package modus-operandi-theme
    :straight t
    :config
    (setq modus-operandi-theme-scale-headings t)
    (setq modus-operandi-theme-proportional-fonts t)
    (setq modus-operandi-theme-slanted-constructs t)
    (setq modus-operandi-theme-visible-fringes t)
    (setq modus-operandi-theme-distinct-org-blocks t)
    (load-theme 'modus-operandi t))
    
    ;; These are placed here for ready reference
    ;; (setq modus-operandi-theme-scale-1 1.05)
    ;; (setq modus-operandi-theme-scale-2 1.1)
    ;; (setq modus-operandi-theme-scale-3 1.15)
    ;; (setq modus-operandi-theme-scale-4 1.2)


<a id="org9b12ed9"></a>

## Font and other aesthetics


<a id="org71ffbc2"></a>

### Fixed pitch and variable pitch fonts

    (custom-theme-set-faces
     'user
     '(variable-pitch ((t (:family "iA Writer Mono V"))))
     '(fixed-pitch ((t ( :family "Iosevka term"
    			     :slant normal
    			     :weight normal
    			     :height 1.1
    			     :width normal)))))
    
    ;; Setting the default general font
    (set-face-attribute 'default nil
    		    :family "iA Writer Mono V"
    		    :height 130
    		    )


<a id="org679f6ca"></a>

### Faces for specific org elements

    (custom-theme-set-faces
     'user
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow variable-pitch)))))
     '(org-document-info ((t (:foreground "dark orange"))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide variable-pitch)))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face -pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow variable-pitch))))))


<a id="orgcb3e1b9"></a>

## Fill column and auto fill

    (setq-default fill-column 72)
    (global-visual-line-mode 1)
    (add-hook 'text-mode-hook 'auto-fill-mode)


<a id="org35d5ef7"></a>

## Spaceline : modeline configuration

-   [X] [Get that spacemacs look without spacemacs | Pragmatic Emacs](http://pragmaticemacs.com/emacs/get-that-spacemacs-look-without-spacemacs/) - this provides a bare bones setup
-   [ ] [Amit's Thoughts: Emacs spaceline mode line](http://amitp.blogspot.com/2017/01/emacs-spaceline-mode-line.html) : This is an excellent guide covering a lot of possibilities to customise so many aspects of the modeline and make it look great.
-   [ ] It seems that spaceline adds a few extra seconds to the init time. I wonder if there are lighter packages that achieve the same effect.

    (use-package spaceline
      :straight t
      :init
      (setq powerline-default-separator 'arrow-fade)
      :config
      (disable-theme 'smart-mode-line-light)
      (require 'spaceline-config)
      (spaceline-spacemacs-theme)
      (spaceline-toggle-buffer-position-off)
      (spaceline-toggle-hud-off)
      (setq spaceline-python-pyvenv-on 1)
      (spaceline-toggle-minor-modes-off))


<a id="org814f7a9"></a>

## Striking out Done headlines

source: Sacha Chua

    (setq org-fontify-done-headline t)
    (custom-set-faces
     '(org-done ((t (:foreground "DarkGreen"
    			     :weight normal
    			     :strike-through t))))
     '(org-headline-done
       ((((class color) (min-colors 16) (background dark))
         (:foreground "LightSalmon" :strike-through t)))))


<a id="org46c60c7"></a>

## keywords as boxes with inverted colors

Source : SO [link](https://stackoverflow.com/questions/12707492/add-custom-markers-to-emacs-org-mode) ,

    (set-face-attribute 'org-todo nil
                        :box '(:line-width 2
    				       :color "red"
    				       :style released-button)
                        :inverse-video t)
    
    (set-face-attribute 'org-done nil
                        :box '(:line-width 2
    				       :color "lightgreen"
    				       :style released-button)
                        :inverse-video t
                        )
    (set-face-attribute 'org-priority nil
                        :inherit font-lock-keyword-face
                        :inverse-video t
                        :box '(:line-width 2
    				       :color "black"
    				       :style released-button))
    (set-face-attribute 'org-checkbox
    		    nil
    		    :inherit font-lock-keyword-face
    		    :inverse-video t
    		    :box '(:line-width 2
    				       :color "black"
    				       :style released-button))


<a id="orgd18041e"></a>

## Remove the bars and startup messages

    ;; Removing all the bars
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    ;; No start up message and nothing to pollute the scratch buffer
    (setq inhibit-startup-message t initial-scratch-message nil)


<a id="orgd48c1a3"></a>

# Hugo


<a id="org6879fb0"></a>

## ox-hugo setup

    (use-package ox-hugo
      :ensure t
      :after ox
      :custom
      (org-hugo--tag-processing-fn-replace-with-hyphens-maybe t)
    (require 'ox-hugo)
      )


<a id="orgd4e436f"></a>

## TODO Auto-generate some properties

The earlier procedure by choice was to use the org-id to name
posts. This ensured that the post URL's would remain unique and
indifferent to a title. However, this does not make for a human-friendly
url to quickly distinguish which post is being referred to. Even so, I
would like to generate some properties and attributes based on the
heading, tags and so on.


<a id="org329e7ec"></a>

# Loading secret config

    ;; Loading secret config containing personal information
    (org-babel-load-file (sr/fun/emacs-dir "sr-secrets.org.gpg"))

