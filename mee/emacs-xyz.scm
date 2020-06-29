;; emacs-xyz.scm
;; Copyright (C) 2020 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (mee emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (guix utils)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-explain-pause
  (package
    (name "emacs-explain-pause")
    (version "d18eea75fb2433a901501e078ec5cd38bf97db19")
    (source
     (origin (method git-fetch)
             (uri (git-reference (url "https://github.com/lastquestion/explain-pause-mode")
                                 (commit version)))
             (file-name (git-file-name name version))
        (sha256
          (base32
           "1y9mqyjbsdrjh7f6lysypfwq232m6fwk7bq8vwkrg9fczkwjp9v4"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/ch11ng/xelb")
    (synopsis "X protocol Emacs Lisp Binding")
    (description
     #f)
    (license license:gpl3+)))

(define-public emacs-no-x-toolkit
  (let ((commit "4645430b9287c3f5ae9863d465a5dd4158e313a9")
        (revision "0")
        (emacs-version "28.0.50"))
    (package
     (inherit emacs-next)
     (name "emacs-no-x-toolkit")
     (version (git-version emacs-version revision commit))
     (source (origin (inherit (package-source emacs-next))
                     (uri (git-reference
                           (url "https://git.savannah.gnu.org/git/emacs.git")
                           (commit commit)))
                     (sha256 (base32 "1fvffxz1pw894c6zhixkr7xdfps01h83jh18nyxx1v84hrr6smg5"))
                     (file-name (git-file-name name version))
                     ))
     (inputs (append `(("inotify-tools" ,inotify-tools))
                     (alist-delete "gtk+" (package-inputs emacs-next))))
     (arguments
      `(,@(substitute-keyword-arguments (package-arguments emacs-next)
                                        ((#:configure-flags cf)
                                         `(cons "--with-x-toolkit=no" ,cf))))))))

;; (define-public emacs-helm-core
;;   (package
;;     (name "emacs-helm-core")
;;     (version "20200429.1931")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (string-append
;;                "https://melpa.org/packages/helm-core-"
;;                version
;;                ".tar"))
;;         (sha256
;;           (base32
;;             "1pzsjg12flynvygsgdyi0gsbhf27dghjhzcssg9cm25shhsl7c0x"))))
;;     (build-system emacs-build-system)
;;     (propagated-inputs
;;       `(("emacs-async" ,emacs-async)))
;;     (home-page "https://emacs-helm.github.io/helm/")
;;     (synopsis "Development files for Helm")
;;     (description "No description available.")
;;     (license #f)))

(define-public emacs-helm-themes
  (package
    (name "emacs-helm-themes")
    (version "20200323.712")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/helm-themes-"
               version
               ".el"))
        (sha256
          (base32
            "03kbzkjr2dy8rh96ddzpx5hykmak3h082jl9j1aq8lhmmvc6pd05"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (home-page
      "https://github.com/syohex/emacs-helm-themes")
    (synopsis
      "Color theme selection with helm interface")
    (description
      "helm-themes.el provide theme selection with helm interface.
Its persistent action can set theme temporary.
")
    (license #f)))

(define-public emacs-artbollocks-mode
  (package
    (name "emacs-artbollocks-mode")
    (version "20170524.422")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/artbollocks-mode-"
               version
               ".el"))
        (sha256
          (base32
            "1gk6xqm96qvpxwh3425h96gj84km4msvslz9nad7v5nkd4djgbxa"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/sachac/artbollocks-mode")
    (synopsis
      "Improve your writing (especially about art)")
    (description
      "Usage

To use, save artbollocks-mode.el to a directory in your load-path.

(require 'artbollocks-mode)
(add-hook 'text-mode-hook 'artbollocks-mode)

or

M-x artbollocks-mode

NOTE: If you manually turn on artbollocks-mode,
you you might need to force re-fontification initially:

  M-x font-lock-fontify-buffer
")
    (license #f)))

(define-public emacs-hyperbole
  (package
    (name "emacs-hyperbole")
    (version "7.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://elpa.gnu.org/packages/hyperbole-"
               version
               ".tar"))
        (sha256
          (base32
            "08gi4v76s53nfmn3s0qcxc3zii0pspjfd6ry7jq1kgm3z34x8hab"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases
        %standard-phases
        ;; Many issues with emacs -Q -L . -f batch-byte-compile *.el, it seems
        ;; like the devs didn't try that at all. And some more problems from
        ;; doing it in a container without /home.  Leave uncompiled for now.
        (delete 'build)  
        (add-after 'patch-source-shebangs 'patch
         (lambda _
           ;; in retrospect, probs only needed in hsettings.el
           (substitute* '("hmouse-key.el" "hinit.el" "hibtypes.el" "hib-doc-id.el"
                          "hib-kbd.el" "hib-debbugs.el" "hactypes.el" "hsettings.el")
                        ((";;; Code:")
                         ";;; Code:
(defun hyperbole-toggle-messaging (&optional arg)
  \"Toggle Hyperbole support for explicit buttons in mail and news buffers.
Toggles the boolean variable `inhibit-hyperbole-messaging’ and either
adds hooks (nil value) or removes them (t value).

With optional prefix ARG > 0, enables support.  If ARG <= 0,
disables/inhibits support.\"
  (interactive \"P\")
  (setq inhibit-hyperbole-messaging (if (null arg)
					(not inhibit-hyperbole-messaging)
				      (<= (prefix-numeric-value arg) 0)))
  (if inhibit-hyperbole-messaging
      (var:remove-all)
    (var:append-all)
    ;; Add any hooks that were skipped when inhibit-hyperbole-messaging
    ;; was nil.
    (cond ((boundp 'hyperbole-loading))
	  ((not after-init-time)
	   (add-hook 'after-init-hook (lambda () (load \"hyperbole\"))))
	  (t (load \"hyperbole\"))))
  (if (called-interactively-p 'interactive)
      (message \"Hyperbole messaging button support is %s\"
	       (if inhibit-hyperbole-messaging \"disabled\" \"enabled\"))))"
                         ))
           (substitute* "hypb.el"
                        (("\\(or \\(file-exists-p \"/usr/bin/domainname\"\\)")
                         "")
	                (("\\(file-exists-p \"/bin/domainname\"\\)\\)")
                         "(executable-find \"domainname\")"))
           #t)))))
    (home-page
     "http://www.gnu.org/software/hyperbole")
    (synopsis
      "GNU Hyperbole: The Everyday Hypertextual Information Manager")
    (description
      #f)
    (license license:gpl3+)))

(define-public emacs-xelb-next
  (package
    (name "emacs-xelb-next")
    (version "0.18-delta")
    (source
     (origin (method git-fetch)
             (uri (git-reference (url "https://github.com/ch11ng/xelb")
                                 (commit "0f10c95")))
             (file-name (git-file-name name version))
        (sha256
          (base32
           "0vfvl3ygccdwsm15hjlbx4hail83i8gakk1p85ywplfmm1b8hraz"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-cl-generic" ,emacs-cl-generic)))
    (home-page "https://github.com/ch11ng/xelb")
    (synopsis "X protocol Emacs Lisp Binding")
    (description
     #f)
    (license license:gpl3+)))

(define-public emacs-exwm-next
  (package
    (name "emacs-exwm-next")
    (version "0.23-delta")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/ch11ng/exwm")
                           (commit "95d6aa9")))
       (file-name (git-file-name name version))
        (sha256
          (base32
           "189xj19nskshzh28bzz1vakhkygw6c5h2mrgly2jkfxz9bmz1sz1"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-xelb-next" ,emacs-xelb-next)))
    (home-page "https://github.com/ch11ng/exwm")
    (synopsis "Emacs X Window Manager")
    (description #f)
    (license license:gpl3+)))

(define-public emacs-hardcore-mode
  (package
    (name "emacs-hardcore-mode")
    (version "20151114.701")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/hardcore-mode-"
               version
               ".el"))
        (sha256
          (base32
            "0hi0fc6cc25r3z21mdn9ipwglpdsy1wy1gzb43wyhhs9b1w8wx11"))))
    (build-system emacs-build-system)
    (home-page "unspecified")
    (synopsis
      "Disable arrow keys + optionally backspace and return")
    (description
     #f)
    (license #f)))

;; emacs-srfi

(define-public emacs-bm
  (package
    (name "emacs-bm")
    (version "20190807.1217")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/bm-"
               version
               ".tar"))
        (sha256
          (base32
            "1kzwmsqjybv2cgmmbaxcfbs6gqab8w9x61j1mpmdxnfz2bak49iq"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/joodland/bm")
    (synopsis "Visible bookmarks in buffer.")
    (description
     #f)
    (license #f)))

(define-public emacs-ts
  (package
    (name "emacs-ts")
    (version "20191010.210")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/ts-"
               version
               ".el"))
        (sha256
          (base32
            "0zbqavmz01l1q89l812iqhcwpw08z3zrgsraa5d4d3fkdaxjnvrv"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-dash" ,emacs-dash) ("emacs-s" ,emacs-s)))
    (home-page "http://github.com/alphapapa/ts.el")
    (synopsis "Timestamp and date/time library")
    (description
     #f)
    (license #f)))

;; (define-public emacs-ts
;;   (let ((commit "df48734ef046547c1aa0de0f4c07d11964ef1f7f")
;;         (revision "1"))
;;     (package
;;       (name "emacs-ts")
;;       (version (git-version "0.2" revision commit))
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/alphapapa/ts.el")
;;                       (commit commit)))
;;                 (sha256
;;                  (base32
;;                   "0hi0dfcwrr0vxp26v3f6radpmmwxbiz3px3br0cydfi6axikw9xl"))
;;                 (file-name (git-file-name name version))))
;;       (build-system emacs-build-system)
;;       (propagated-inputs
;;        `(("emacs-s" ,emacs-s)
;;          ("emacs-dash" ,emacs-dash)))
;;       (arguments
;;        ;; XXX: Three tests are failing because of a timezone-related issue
;;        ;; with how they're written.  On my machine, all the failing test
;;        ;; results are 18000 seconds (5 hours) off.

;;        ;; The ts-parse-org function accepts a string without any timezone
;;        ;; info, not assumed to be in Unix time, and converts it to a so-called
;;        ;; ts struct.  The ts-unix function (accessor) accepts a ts struct,
;;        ;; then seems to assume the struct's corresponding time is in terms of
;;        ;; the user's current time zone, before returning a Unix time in
;;        ;; seconds.

;;        ;; The failing tests all have similar problems, but nothing else about
;;        ;; the library seems particularly off.

;;        `(#:tests? #t
;;          #:test-command '("emacs" "--batch"
;;                           "-l" "test/test.el"
;;                           "-f" "ert-run-tests-batch-and-exit")
;;          #:phases
;;          (modify-phases %standard-phases
;;            (add-before 'check 'make-tests-writable
;;              (lambda _
;;                (make-file-writable "test/test.el")
;;                #t))
;;            (add-before 'check 'delete-failing-tests
;;              (lambda _
;;                (emacs-batch-edit-file "test/test.el"
;;                  `(progn (progn
;;                           (goto-char (point-min))
;;                           (dolist (test-regexp '("ert-deftest ts-format"
;;                                                  "ert-deftest ts-parse-org\\_>"
;;                                                  "ert-deftest ts-parse-org-element"))
;;                                   (re-search-forward test-regexp)
;;                                   (beginning-of-line)
;;                                   (kill-sexp)))
;;                          (basic-save-buffer)))
;;                #t)))))
;;       (home-page "https://github.com/alphapapa/ts.el")
;;       (synopsis "Timestamp and date/time library")
;;       (description "This package facilitates manipulating dates, times, and
;; timestamps by providing a @code{ts} struct.")
;;       (license license:gpl3+))))

(define-public emacs-stan-mode
  (package
    (name "emacs-stan-mode")
    (version "20200221.2025")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/stan-mode-"
               version
               ".tar"))
        (sha256
          (base32
            "0s3nmahbac6gzpwd9ln799x0bgf3x5d256l440gjrhs3gayy99w3"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/stan-dev/stan-mode/tree/master/stan-mode")
    (synopsis "Major mode for editing Stan files")
    (description
      "This is a major mode for the Stan modeling language for Bayesian
statistics.  (See URL `https://mc-stan.org/').

This major mode supports syntax-highlighting, indentation,
`imenu-mode', and `compilation-mode'.

Usage:

  (require 'stan-mode)
")
    (license #f)))


(define-public emacs-nadvice
  (package
   (name "emacs-nadvice")
   (version "0.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/nadvice-"
           version
           ".el"))
     (sha256
      (base32
       "0gi3csnxbs8h7iy0scsl35sic3gv90swa89hhdjwb7qvpirfdcgw"))))
   (build-system emacs-build-system)
   (home-page
    "http://elpa.gnu.org/packages/nadvice.html")
   (synopsis
    "Forward compatibility for Emacs-24.4's nadvice")
   (description
    "This package tries to re-implement some of nadvice.el's functionality
on top of the old defadvice system, to help users of defadvice
move to the new advice system without dropping support for Emacs<24.4.

Limitations;
- only supports `advice-add', `advice-remove', and `advice-member-p'.
- only handles the :before, :after, :override, and :around kinds of advice;
- requires a named rather than anonymous function;
- and does not support any additional properties like `name' or `depth'.

It was tested on Emacs-22 and I can't see any obvious reason why it
wouldn't work on older Emacsen.")
   (license license:gpl3+)))

(define-public emacs-poly-markdown
  (package
   (name "emacs-poly-markdown")
   (version "20190916.702")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/poly-markdown-"
           version
           ".el"))
     (sha256
      (base32
       "02znfzh5g6y9fll80abj5namqi56xc72w8gvhaprp0fjfpd1y8ci"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-polymode" ,emacs-polymode)
      ("emacs-markdown-mode" ,emacs-markdown-mode)))
   (home-page
    "https://github.com/polymode/poly-markdown")
   (synopsis "Polymode for markdown-mode")
   (description "")
   (license #f)))

(define-public emacs-keymap-utils
  (package
   (name "emacs-keymap-utils")
   (version "20191222.2258")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/keymap-utils-"
           version
           ".el"))
     (sha256
      (base32
       "17fpaqphwqpf0qqv0dnkaf6i5k240sdgjx8gmx6652dllizx5zs8"))))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/tarsius/keymap-utils")
   (synopsis "keymap utilities")
   (description
    "This package provides some utilities useful for inspecting and
modifying keymaps.
")
   (license #f)))


(define-public emacs-packed
  (package
   (name "emacs-packed")
   (version "20180318.1729")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/packed-"
           version
           ".el"))
     (sha256
      (base32
       "1q68mq9lkpf8xxbc00z79ihbary7kyb17a13fb4a0gi52mfzb77s"))))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/emacscollective/packed")
   (synopsis
    "package manager agnostic Emacs Lisp package utilities")
   (description
    "Packed provides some package manager agnostic utilities to work
with Emacs Lisp packages.  As far as Packed is concerned packages
are collections of Emacs Lisp libraries that are stored in a
dedicated directory such as a Git repository.  And libraries are
Emacs Lisp files that provide the correct feature (matching the
filename).

Where a package manager might depend on metadata, Packed instead
uses some heuristics to get the same information — that is slower
and might also fail at times but makes it unnecessary to maintain
package recipes.
")
   (license #f)))

(define-public emacs-auto-compile
  (package
   (name "emacs-auto-compile")
   (version "20191020.1040")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/auto-compile-"
           version
           ".el"))
     (sha256
      (base32
       "1j0vxv8vicnbnbmac7l1wa48m5wsi959rd0d5yi1cgsd2hd56lm0"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-packed" ,emacs-packed)))
   (home-page
    "https://github.com/emacscollective/auto-compile")
   (synopsis
    "automatically compile Emacs Lisp libraries")
   (description
    "This package provides two minor modes which automatically recompile
Emacs Lisp source files.  Together these modes guarantee that Emacs
never loads outdated byte code files.

`auto-compile-on-save-mode' re-compiles source files when they are
being saved and `auto-compile-on-load-mode' does so before they are
being loaded (by advising `load' and `require').  Both modes only
ever _re-compile_ a source file when the respective byte code file
already exists but is outdated.  Otherwise they do _not_ compile
the source file.

Even when using `auto-compile-on-save-mode' it can happen that some
source file is newer than the respective byte code file, which is a
problem because by default Emacs loads the byte code file even when
the respective source file has been modified more recently.

Starting with Emacs version 24.4, setting `load-prefer-newer' to t
prevents outdated byte code files from being loaded.  However this
does not cause re-compilation of the source file, to actually do
that `auto-compile-on-load-mode' is still required.

Setup
-----

To reduce the risk of loading outdated byte code files, you should
set `load-prefer-newer' and enable `auto-compile-on-load-mode' as
early as possible.  Then also enable `auto-compile-on-save-mode'.
You should also consider not byte-compiling your personal init
file, or setting `load-prefer-newer' in a system-wide init file.

If you use `package.el' then use something like this:

    ;;; init.el --- user init file
    (setq load-prefer-newer t)
    (package-initialize)
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)

otherwise:

    ;;; init.el --- user init file
    (setq load-prefer-newer t)
    (add-to-list 'load-path \"/path/to/packed\")
    (add-to-list 'load-path \"/path/to/auto-compile\")
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)

You might want to set the file-local value of `no-byte-compile' to
t, e.g. by adding \"-*- no-byte-compile: t -*-\" (without the quotes)
at the end of the very first line.  That way all user files benefit
from the protection offered by `load-prefer-newer' and the modes
that are defined here, otherwise `~/.emacs.d/init.el' is the only
exception.

If you are using Emacs 27 or later, then these settings should be
placed in `early-init.el', which should never be compiled:

    ;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
    (setq load-prefer-newer t)
    (add-to-list 'load-path \"/path/to/packed\")
    (add-to-list 'load-path \"/path/to/auto-compile\")
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    ;;; early-init.el ends here

Usage\n-----

Take note of the compile warnings and fix them.

To permanently or temporarily toggle automatic compilation of some
source file use the command `toggle-auto-compile'.  Since the modes
only ever _update_ byte code files, toggling automatic compilation
is done simply by either creating the byte code file or by removing
it.  `toggle-auto-compile' can also toggle automatic compilation of
multiple files at once; see its doc-string for more information.

Customization
-------------

Constantly having the *Compile-Log* buffer pop up when a file is
being saved can quickly become annoying.  Obviously the first thing
you should do to about that is to actually fix outstanding issues.

Once you have done that you might also want to keep that buffer
from being automatically displayed and instead only show the number
of compile warnings for the current file in the mode-line.

    (setq auto-compile-display-buffer nil)
    (setq auto-compile-mode-line-counter t)

To display the buffer use `M-x auto-compile-display-log' or click
on the counter in the mode-line.

Using `auto-compile-inhibit-compile-hook' it is possible to inhibit
automatic compilation under certain circumstances; e.g. when HEAD
is detached inside a Git repository (useful during rebase sessions).
")
   (license #f)))

(define-public emacs-frames-only-mode
  (package
   (name "emacs-frames-only-mode")
   (version "20190524.1439")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/frames-only-mode-"
           version
           ".el"))
     (sha256
      (base32
       "1v40lnmqzkl3mzxa2jpvh1br2xvj3118q5zb3vcy212paq2lfw1j"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-dash" ,emacs-dash) ("emacs-s" ,emacs-s)))
   (home-page
    "https://github.com/davidshepherd7/frames-only-mode")
   (synopsis "Use frames instead of Emacs windows")
   (description "")
   (license #f)))

(define-public emacs-elisp-format
  (package
   (name "emacs-elisp-format")
   (version "20160508.952")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/elisp-format-"
           version
           ".el"))
     (sha256
      (base32
       "0rqrn2713jmb95x1m610q2phsg58rxx53bklf9dc5671nsrjs5z1"))))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/Yuki-Inoue/elisp-format")
   (synopsis "Format elisp code")
   (description
    "This package is format elisp code.
This package is format by itself, so you can view format effect.

Below are commands you can use:

`elisp-format-region'
     Format region or defun.
`elisp-format-buffer'
     Format buffer.
`elisp-format-file'
     Format file.
`elisp-format-file-batch'
     Format file with `batch'.
`elisp-format-directory'
     Format recursive elisp files in directory.
`elisp-format-directory-batch'
     Format recursive elisp files in directory with `batch'.\n`elisp-format-dired-mark-files'
     Format dired marked files.
`elisp-format-library'
     Format library.

Tips:

If current mark is active, command `elisp-format-region'
will format region you select, otherwise it will format
`defun' around point.

If you want format many files, you can marked them in dired,
and use command `elisp-format-dired-mark-files' to format
marked files in dired.

You can format special file through
command `elisp-format-file'.

By default, when you format `huge' file, it will
hang emacs.
You can also use command `elisp-format-file-batch'
make format process at background.

You also can use command `elisp-format-directory'
format all recursive elisp files in special directory.

By default, when you use command `elisp-format-directory'
format too many elisp files, will hang emacs.
You can also use command `elisp-format-directory-batch'
make format process at background.

If you're sure lazy, you can use command `elisp-format-library'
format special library and don't need input long file path.

Note:

I can't ensure this package can perfect work with all situations.
So please let me know if you have suggestion or bug.


Installation:

Put elisp-format.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name \"~/elisp\"))

And the following to your ~/.emacs startup file.

(require 'elisp-format)

No need more.
")
   (license #f)))


(define-public emacs-right-click-context
  (package
   (name "emacs-right-click-context")
   (version "20190528.1832")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/right-click-context-"
           version
           ".el"))
     (sha256
      (base32
       "0r4k7cspw25ajgd7cq082z9w0shbj76w1smwaghc16aiwwpwpfhc"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-popup" ,emacs-popup)
      ("emacs-ordinal" ,emacs-ordinal)))
   (home-page
    "https://github.com/zonuexe/right-click-context")
   (synopsis "Right Click Context menu")
   (description
    "This mode focuses on providing operations similar to GUI context menus.
It not only activates commands, it also supports operations on Region.

Put the following into your .emacs file (~/.emacs.d/init.el) to enable context menu.

    (right-click-context-mode 1)

This function does not depend on GUI, it is fully available on terminal.
The menu is launched by \"right click\" (<mouse-3>) by default, but you can assign any key.

    (define-key right-click-context-mode-map (kbd \"C-c :\") 'right-click-context-menu)

This menu can be constructed with a simple DSL based on S-expression.
Additional information can be found in README and implementation code.

## Context-menu construction DSL

For example, the following code adds undo and redo to the beginning of the context menu.

    (setq right-click-context-global-menu-tree
          (append
           '((\\\"Undo\\\" :call (if (fboundp 'undo-tree-undo) (undo-tree-undo) (undo-only)))
             (\\\"Redo\\\"
             :call (if (fboundp 'undo-tree-redo) (undo-tree-redo))
             :if (and (fboundp 'undo-tree-redo) (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))))
           right-click-context-global-menu-tree))
")
   (license #f)))
(define-public emacs-ordinal
  (package
   (name "emacs-ordinal")
   (version "20190104.1421")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/ordinal-"
           version
           ".el"))
     (sha256
      (base32
       "18nbjmaij0wi01zwh7d35d8hhf0h8ksl1q8764xj6q28lx37fgwq"))))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/zonuexe/ordinal.el")
   (synopsis
    "Convert number to ordinal number notation")
   (description
    "This package simply provides conversion to English ordinal numbers.
(ex.  1st, 2nd, 3rd, 4th... Nth)

It is worth noting that this function accepts \"0th\" for compatibility with function `nth'.
If you do not like it you can control it with the ordinal-number-accept-0 variable.

(ordinal-format 0) ;; => \"0th\"

You can prohibit \"0th\" for correct English.

(let ((ordinal-number-accept-0 nil))
  (ordinal-format 0))
=>  Assertion failed: (>= n 1)

This variable works with dynamic scope.  Do not use `setq' for `ordinal-number-accept-0'.
")
   (license #f)))


(define-public emacs-sublimity
  (package
   (name "emacs-sublimity")
   (version "20181121.1311")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/sublimity-"
           version
           ".tar"))
     (sha256
      (base32
       "1gl67fmhagir9zwg5rw5v1fv1d7zi21fcwgah2z189qbi7lfb5y9"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/zk-phi/sublimity")
   (synopsis
    "smooth-scrolling, minimap and distraction-free mode")
   (description
    "Require this script and some of \"sublimity-scroll\" \"sublimity-map\".

  (require 'sublimity)
  (require 'sublimity-scroll)

  (require 'sublimity-map)

then call command \"M-x sublimity-mode\".

If you want to enable sublimity everywhere, call function
sublimity-global-mode.

  (sublimity-global-mode)

For more informations, see \"Readme\".
")
   (license #f)))

(define-public emacs-lv
  (package
   (name "emacs-lv")
   (version "20191214.1357")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/lv-"
           version
           ".el"))
     (sha256
      (base32
       "1kqkkibz7302y7wm822cmbazxyqq7xw7yzmj7hsfq751mjc4prqy"))))
   (build-system emacs-build-system)
   (home-page "unspecified")
   (synopsis "Other echo area")
   (description
    "This package provides `lv-message' intended to be used in place of
`message' when semi-permanent hints are needed, in order to not
interfere with Echo Area.

   \"Я тихо-тихо пiдглядаю,
    І тiшуся собi, як бачу то,
    Шо страшить i не пiдпускає,
    А iншi п’ють тебе, як воду пiсок.\"
    --  Андрій Кузьменко, L.V.
")
   (license #f)))

(define-public emacs-nu-mode
  (package
   (name "emacs-nu-mode")
   (version "20190404.2032")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/nu-mode-"
           version
           ".tar"))
     (sha256
      (base32
       "0j9lila395q4ngg37zwgmymyylwn8s2sgrki8ym0gww4xrwvs9a8"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-undo-tree" ,emacs-undo-tree)
      ("emacs-ace-window" ,emacs-ace-window)
      ("emacs-lv" ,emacs-lv)
      ("emacs-avy" ,emacs-avy)
      ("emacs-which-key" ,emacs-which-key)
      ("emacs-transpose-frame" ,emacs-transpose-frame)))
   (home-page "unspecified")
   (synopsis
    "Modern Emacs Prompts Based Keybinding.")
   (description "No description available.")
   (license #f)))

(define-public emacs-hungry-delete
  (package
   (name "emacs-hungry-delete")
   (version "20170412.102")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://melpa.org/packages/hungry-delete-"
           version
           ".el"))
     (sha256
      (base32
       "1cyd647dv54a0ab62g569c7slhzlra532dhkx6yi26214gmfc5m0"))))
   (build-system emacs-build-system)
   (home-page
    "http://github.com/nflath/hungry-delete")
   (synopsis "hungry delete minor mode")
   (description
    "cc-mode implements hungry deletion for its programming modes.  This
package borrows its implementation in a minor mode, so that hungry
deletion can be used in all modes.

Installation

To use this mode, put the following in your init.el:
(require 'hungry-delete)

You then need to enable hungry-delete-mode, either in
relevant hooks, with turn-on-hungry-delete-mode, or with
global-hungry-delete-mode.
")
   (license #f)))

(define-public emacs-nlinum
  (package
   (name "emacs-nlinum")
   (version "1.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/nlinum-"
           version
           ".el"))
     (sha256
      (base32
       "03zqlz58fvh4cpfl43h7py2fpnc7m37f1ys8zhrc511ccq9cwkdn"))))
   (build-system emacs-build-system)
   (home-page
    "http://elpa.gnu.org/packages/nlinum.html")
   (synopsis "Show line numbers in the margin")
   (description
    "This is like linum-mode, but uses jit-lock to be (hopefully)
more efficient.")
   (license license:gpl3+)))

(define-public emacs-hercules-next
  (package
   (inherit emacs-hercules)
   (name "emacs-hercules-next")
   (version "0.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/jjzmajic/hercules.el.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0snfmvri4cpmhrk5s4hxxlyzs3d58ysmqzaiyiz4979admdc1lmb"))))
   (propagated-inputs `(("emacs-which-key" ,emacs-which-key)))))

(define-public emacs-piper
  (package
   (name "emacs-piper")
   (version "5b5218a4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/howardabrams/emacs-piper")
           (commit version)))
     ;; (file-name (git-file-name name version))
     (sha256
      (base32
       "0njhhajyirak062irf41i1fpv8bm77pi92vjvm2xxj6br7rxyi3x"))))
   (build-system emacs-build-system)
   (propagated-inputs `(
                        ("emacs-dash"          ,emacs-dash)
                        ("emacs-f"             ,emacs-f)
                        ("emacs-hydra"         ,emacs-hydra)
                        ("emacs-s"             ,emacs-s)
                        ("emacs-visual-regexp" ,emacs-visual-regexp)))
   (home-page "")
   (synopsis "")
   (description "")
   (license license:gpl3+)
   ))

(define-public emacs-aio
  (package
    (name "emacs-aio")
    (version "20190601.753")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/aio-"
               version
               ".tar"))
        (sha256
          (base32
            "0jl9nmll73lry0wh7d2pshfir6ma3cwa51qbzswxnyn9pwbmncxb"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/skeeto/emacs-aio")
    (synopsis "async/await for Emacs Lisp")
    (description
      "`aio` is to Emacs Lisp as [`asyncio`][asyncio] is to Python. This
package builds upon Emacs 25 generators to provide functions that
pause while they wait on asynchronous events. They do not block any
thread while paused.

The main components of this package are `aio-defun' / `aio-lambda'
to define async function, and `aio-await' to pause these functions
while they wait on asynchronous events. When an asynchronous
function is paused, the main thread is not blocked. It is no more
or less powerful than callbacks, but is nicer to use.

This is implementation is based on Emacs 25 generators, and
asynchronous functions are actually iterators in disguise, operated
as stackless, asymmetric coroutines.
")
    (license #f)))

(define-public emacs-iter2
  (package
    (name "emacs-iter2")
    (version "20190113.1424")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/iter2-"
               version
               ".el"))
        (sha256
          (base32
            "0vs8i4nb2b2f4f7hq6xf1c0b249gn6nvx0dngi9gk2frap605zl3"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/doublep/iter2")
    (synopsis "Reimplementation of Elisp generators")
    (description
      "Fully compatible fast reimplementation of `generator' built-in
Emacs package.  This library provides `iter2-defun` and
`iter2-lambda` forms that can be used in place of `iter-defun` and
`iter-lambda`.  All other functions and macros (e.g. `iter-yield`,
`iter-next`) are intentionally not duplicated: just use the
original ones.
")
    (license #f)))

(define-public emacs-promise
  (package
    (name "emacs-promise")
    (version "20200209.616")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/promise-"
               version
               ".tar"))
        (sha256
          (base32
            "10a9440zmvr0bwxvxq217vwybiw2ppidk979fr7h9h4hgnj8p5gh"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/chuntaro/emacs-promise")
    (synopsis "Promises/A+")
    (description
      "This is a simple implementation of Promises/A+.

This implementation ported the following Promises/A+ implementation faithfully.
https://github.com/then/promise

* The same API as JavaScript version Promise can be used.
 * then, catch, resolve, reject, all, race, etc...
* supports \"thenable\"
* supports \"Inheritance of Promise\"
* supports \"rejection-tracking\"

Usage:
See `promise-examples.el' for details.
 https://raw.githubusercontent.com/chuntaro/emacs-promise/master/examples/promise-examples.el
 You can check the operation while downloading and running it interactively.

(require 'promise)

Please be sure to enable it when developing.
(promise-rejection-tracking-enable '((all-rejections . t)))

(defun do-something-async (delay-sec value)
  \"Return `Promise' to resolve the value asynchronously.\"
  (promise-new (lambda (resolve _reject)
                 (run-at-time delay-sec
                              nil
                              (lambda ()
                                (funcall resolve value))))))

(defun example4 ()
  \"All processes are asynchronous Promise chain.\"
  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message \"first result: %s\" result)
            (do-something-async 1 (* result 2))))

    (then (lambda (second-result)
            (message \"second result: %s\" second-result)
            (do-something-async 1 (* second-result 2))))

    (then (lambda (third-result)
            (message \"third result: %s\" third-result)))))
")
    (license #f)))

(define-public emacs-async-await
  (package
    (name "emacs-async-await")
    (version "20200117.828")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/async-await-"
               version
               ".el"))
        (sha256
          (base32
            "0m6vwdz7icinpkg6l10g16mmciwx2hvibs928088hasl4mdi2mgx"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-promise" ,emacs-promise)
        ("emacs-iter2" ,emacs-iter2)))
    (home-page
      "https://github.com/chuntaro/emacs-async-await")
    (synopsis "Async/Await")
    (description
      "This is a simple implementation of Async/Await.
Inspired by the Async/Await implementation of TypeScript.

Usage:
See `async-await-examples.el' for details.
 https://raw.githubusercontent.com/chuntaro/emacs-async-await/master/examples/async-await-examples.el
 You can check the operation while downloading and running it interactively.

(require 'async-await)

Please be sure to enable it when developing.
(promise-rejection-tracking-enable '((all-rejections . t)))

(defun wait-async (n)
  (promise-new (lambda (resolve _reject)
                 (run-at-time n
                              nil
                              (lambda ()
                                (funcall resolve n))))))

(async-defun example2 ()
  (print (await (wait-async 0.5)))
  (message \"---\")

  (print (await (wait-async 1.0)))
  (message \"---\")

  (print (await (wait-async 1.5)))
  (message \"---\")

  (message \"await done\"))

(example2) =>

0.5

---

1.0

---

1.5

---
await done

The result of the execution is outputted from the top to the bottom
like the order written in the code.  However, asynchronously!
")
    (license #f)))


(define-public emacs-heap
  (package
    (name "emacs-heap")
    (version "0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://elpa.gnu.org/packages/heap-"
               version
               ".el"))
        (sha256
          (base32
            "13qv0w3fi87c85jcy7lv359r6rpsgnp5zzs2f2zq4dl3540wzrxg"))))
    (build-system emacs-build-system)
    (home-page "http://www.dr-qubit.org/emacs.php")
    (synopsis
      "Heap (a.k.a. priority queue) data structure")
    (description
      "A heap is a form of efficient self-sorting tree. In particular, the root
node is guaranteed to be the highest-ranked entry in the tree. (The
comparison function used for ranking the data can, of course, be freely
defined). Therefore repeatedly removing the root node will return the data
in order of increasing rank. They are often used as priority queues, for
scheduling tasks in order of importance.

This package implements ternary heaps, since they are about 12% more
efficient than binary heaps for heaps containing more than about 10
elements, and for very small heaps the difference is negligible. The
asymptotic complexity of ternary heap operations is the same as for a
binary heap: 'add', 'delete-root' and 'modify' operations are all O(log n)
on a heap containing n elements.

Note that this package implements a heap as an implicit data structure on a
vector. Therefore, the maximum size of the heap has to be specified in
advance. Although the heap will grow dynamically if it becomes full, this
requires copying the entire heap, so insertion has worst-case complexity
O(n) instead of O(log n), though the amortized complexity is still
O(log n). (For applications where the maximum size of the heap is not known
in advance, an implementation based on binary trees might be more suitable,
but is not currently implemented in this package.)

You create a heap using `make-heap', add elements to it using `heap-add',
delete and return the root of the heap using `heap-delete-root', and modify
an element of the heap using `heap-modify'. A number of other heap
convenience functions are also provided, all with the prefix
`heap-'. Functions with prefix `heap--' are for internal use only, and
should never be used outside this package.")
    (license license:gpl3+)))

(define-public emacs-tnfa
  (package
    (name "emacs-tnfa")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://elpa.gnu.org/packages/tNFA-"
               version
               ".el"))
        (sha256
          (base32
            "01n4p8lg8f2k55l2z77razb2sl202qisjqm5lff96a2kxnxinsds"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-queue" ,emacs-queue)))
    (home-page "http://www.dr-qubit.org/emacs.php")
    (synopsis
      "Tagged non-deterministic finite-state automata")
    (description
      "A tagged, non-deterministic finite state automata (NFA) is an abstract
computing machine that recognises regular languages. In layman's terms,
they are used to decide whether a string matches a regular expression. The
\"tagged\" part allows the NFA to do group-capture: it returns information
about which parts of a string matched which subgroup of the regular
expression.

Why re-implement regular expression matching when Emacs comes with
extensive built-in support for regexps? Primarily, because some algorithms
require access to the NFA states produced part way through the regular
expression matching process (see the trie.el package for an
example). Secondarily, because Emacs regexps only work on strings, whereas
regular expressions can usefully be used in Elisp code to match other
sequence types, not just strings.

A tagged NFA can be created from a regular expression using
`tNFA-from-regexp', and its state can be updated using
`tNFA-next-state'. You can discover whether a state is a matching state
using `tNFA-match-p', extract subgroup capture data from it using
`tNFA-group-data', check whether a state has any wildcard transitions using
`tNFA-wildcard-p', and get a list of non-wildcard transitions using
`tNFA-transitions'. Finally, `tNFA-regexp-match' uses tagged NFAs to decide
whether a regexp matches a given string.

Note that Emacs' regexps are not regular expressions in the original
meaning of that phrase. Emacs regexps implement additional features (in
particular, back-references) that allow them to match far more than just
regular languages. This comes at a cost: regexp matching can potentially be
very slow (NP-hard in fact, though the hard cases rarely crop up in
practise), whereas there are efficient (polynomial-time) algorithms for
matching regular expressions (in the original sense). Therefore, this
package only supports a subset of the full Emacs regular expression
syntax. See the function docstrings for more information.

This package essentially implements Laurikari's algorithm, as described in
his master's thesis, but it builds the corresponding tagged deterministic
finite state automaton (DFA) on-the-fly as needed.

This package uses the queue package queue.el.")
    (license license:gpl3+)))

(define-public emacs-trie
  (package
    (name "emacs-trie")
    (version "0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://elpa.gnu.org/packages/trie-"
               version
               ".el"))
        (sha256
          (base32
            "0869fh3bghxil94wd9vgbb5bk1hx2qkh75vbvp0psmcima8dgzgw"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-tnfa" ,emacs-tnfa)
        ("emacs-heap" ,emacs-heap)))
    (home-page "http://www.dr-qubit.org/emacs.php")
    (synopsis "Trie data structure")
    (description
      "Quick Overview
--------------
A trie is a data structure used to store keys that are ordered sequences of
elements (vectors, lists or strings in Elisp; strings are by far the most
common), in such a way that both storage and retrieval are space- and
time-efficient. But, more importantly, a variety of more advanced queries
can also be performed efficiently: for example, returning all strings with
a given prefix, searching for keys matching a given wildcard pattern or
regular expression, or searching for all keys that match any of the above
to within a given Lewenstein distance.

You create a trie using `make-trie', create an association using
`trie-insert', retrieve an association using `trie-lookup', and map over a
trie using `trie-map', `trie-mapc', `trie-mapcar', or `trie-mapf'. You can
find completions of a prefix sequence using `trie-complete', search for
keys matching a regular expression using `trie-regexp-search', find fuzzy
matches within a given Lewenstein distance (edit distance) of a string
using `trie-fuzzy-match', and find completions of prefixes within a given
distance using `trie-fuzzy-complete'.

Using `trie-stack', you can create an object that allows the contents of
the trie to be used like a stack, useful for building other algorithms on
top of tries; `trie-stack-pop' pops elements off the stack one-by-one, in
\"lexicographic\" order, whilst `trie-stack-push' pushes things onto the
stack. Similarly, `trie-complete-stack', `trie-regexp-stack',
`trie-fuzzy-match-stack' and `trie-fuzzy-complete-stack' create
\"lexicographicly-ordered\" stacks of query results.

Very similar to trie-stacks, `trie-iter', `trie-complete-iter',
`trie-regexp-iter', `trie-fuzzy-match-iter' and `trie-fuzzy-complete-iter'
generate iterator objects, which can be used to retrieve successive
elements by calling `iter-next' on them.

Note that there are two uses for a trie: as a lookup table, in which case
only the presence or absence of a key in the trie is significant, or as an
associative array, in which case each key carries some associated
data. Libraries for other data structure often only implement lookup
tables, leaving it up to you to implement an associative array on top of
this (by storing key+data pairs in the data structure's keys, then defining
a comparison function that only compares the key part). For a trie,
however, the underlying data structures naturally support associative
arrays at no extra cost, so this package does the opposite: it implements
associative arrays, and leaves it up to you to use them as lookup tables if
you so desire, by ignoring the associated data.


Different Types of Trie
-----------------------
There are numerous ways to implement trie data structures internally, each
with its own time- and space-efficiency trade-offs. By viewing a trie as a
tree whose nodes are themselves lookup tables for key elements, this
package is able to support all types of trie in a uniform manner. This
relies on there existing (or you writing!) an Elisp implementation of the
corresponding type of lookup table. The best type of trie to use will
depend on what trade-offs are appropriate for your particular
application. The following gives an overview of the advantages and
disadvantages of various types of trie. (Not all of the underlying lookup
tables have been implemented in Elisp yet, so using some of the trie types
described below would require writing the missing Elisp package!)


One of the most effective all-round implementations of a trie is a ternary
search tree, which can be viewed as a tree of binary trees. If basic binary
search trees are used for the nodes of the trie, we get a standard ternary
search tree. If self-balancing binary trees are used (e.g. AVL or red-black
trees), we get a self-balancing ternary search tree. If splay trees are
used, we get yet another self-organising variant of a ternary search
tree. All ternary search trees have, in common, good space-efficiency. The
time-efficiency of the various trie operations is also good, assuming the
underlying binary trees are balanced. Under that assumption, all variants
of ternary search trees described below have the same asymptotic
time-complexity for all trie operations.

Self-balancing trees ensure the underlying binary trees are always close to
perfectly balanced, with the usual trade-offs between the different the
types of self-balancing binary tree: AVL trees are slightly more efficient
for lookup operations than red-black trees, at a cost of slightly less
efficienct insertion operations, and less efficient deletion
operations. Splay trees give good average-case complexity and are simpler
to implement than AVL or red-black trees (which can mean they're faster in
practice), at the expense of poor worst-case complexity.

If your tries are going to be static (i.e. created once and rarely
modified), then using perfectly balanced binary search trees might be
appropriate. Perfectly balancing the binary trees is very inefficient, but
it only has to be done when the trie is first created or modified. Lookup
operations will then be as efficient as possible for ternary search trees,
and the implementation will also be simpler (so probably faster) than a
self-balancing tree, without the space and time overhead required to keep
track of rebalancing.

On the other hand, adding data to a binary search tree in a random order
usually results in a reasonably balanced tree. If this is the likely
scenario, using a basic binary tree without bothering to balance it at all
might be quite efficient, and, being even simpler to implement, could be
quite fast overall.


A digital trie is a different implementation of a trie, which can be viewed
as a tree of arrays, and has different space- and time-complexities than a
ternary search tree. Roughly speaking, a digital trie has worse
space-complexity, but better time-complexity. Using hash tables instead of
arrays for the nodes gives something similar to a digital trie, potentially
with better space-complexity and the same amortised time-complexity, but at
the expense of occasional significant inefficiency when inserting and
deleting (whenever a hash table has to be resized). Indeed, an array can be
viewed as a perfect hash table, but as such it requires the number of
possible values to be known in advance.

Finally, if you really need optimal efficiency from your trie, you could
even write a custom type of underlying lookup table, optimised for your
specific needs.

This package uses the AVL tree package avl-tree.el, the tagged NFA package
tNFA.el, and the heap package heap.el.")
    (license license:gpl3+)))

(define-public emacs-multi-line
  (package
    (name "emacs-multi-line")
    (version "20170822.226")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/multi-line-"
               version
               ".tar"))
        (sha256
          (base32
            "1gh6wfxlaa1a0yixzddxwzd96180kpgixm5szyz91f16glibm3nk"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-s" ,emacs-s)
        ("emacs-dash" ,emacs-dash)
        ("emacs-shut-up" ,emacs-shut-up)))
    (home-page
      "https://github.com/IvanMalison/multi-line")
    (synopsis "multi-line statements")
    (description
      "multi-line aims to provide a flexible framework for automatically
multi-lining and single-lining function invocations and definitions,
array and map literals and more. It relies on functions that are
defined on a per major mode basis wherever it can so that it operates
correctly across many different programming languages.
")
    (license #f)))

(define-public emacs-org-beautify-theme
  (package
    (name "emacs-org-beautify-theme")
    (version "20170908.2218")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/org-beautify-theme-"
               version
               ".el"))
        (sha256
          (base32
            "111zbfghk6h0f8w0zw4i99yg25y8z4r60blrh343vg9w4y6jhfa6"))))
    (build-system emacs-build-system)
    (home-page "unspecified")
    (synopsis
      "A sub-theme to make org-mode more beautiful.")
    (description
      "#+title: Read Me

[[./screenshot.png]]

* Making Org-mode Beautiful
** This theme is dedicated to my wife Shell
  Who—in her beauty, her love, and her love for beauty—has shown me
  that form can enhance function.
* Mission
  - Make org mode headlines easy to read.  In any theme.
  - Make it look more like a published book and/or desktop app, less
    like angry fruit salad.
  - Make it awesome to live in an org buffer.
* Usage
  Load this theme over top your existing theme, and you should be
  golden.  If you find any incompatibilities, let me know with what
  theme and I will try and fix it.

  When loading a whole new theme overtop, org-beautify-theme will
  still be active with the old theme.  Just unload org-beautify-theme
  and then reload it, and everything will be fine again.

  If you still get really ugly headlines, customize the
  ~org-beautify-theme-use-box-hack~ variable and set it to nil (false).

* Changelog
   - v0.4 :: [2017-09-08]
     - Add org-beautify-theme-use-box-hack to allow the user to
       fix ugly boxes.
   - v0.3.2 :: [2017-08-29]
     - Update License
   - v0.3.1 :: [2016-10-19]
     - Fix load path issues (Thanks PierreTechoueyres!)
     - reverse chronological changelog, because ah-doy!
   - v0.2 :: [2016-08-08]
     - Better repository Location
     - Fix so that you can load the theme properly.
   - v0.1.2 :: [2014-01-06]
     - Add Verdana font to fall back on
   - v0.1.1 :: [2014-01-06]
     - Fix checkboxes
   - v0.1 :: First Release
     - Make the colors suck a lot less, and the buffers look a lot nicer.
")
    (license #f)))

(define-public emacs-org-roam
  (package
   (name "emacs-org-roam")
   (version "0.0-guix1")
   (source (origin (method git-fetch)
                   (uri (git-reference (url "https://github.com/jethrokuan/org-roam.git")
                                       (commit "v1.0.0-rc1")))
                   (sha256 (base32 "10l0wafrqpskzn9mpbii0wkm4wpqsihryfbx5qnigld6gfpp420g"))
                   (file-name (git-file-name name version))))
   (build-system emacs-build-system)
   (propagated-inputs `(("emacs-dash" ,emacs-dash)
                        ("emacs-f" ,emacs-f)
                        ("emacs-s" ,emacs-s)
                        ("emacs-org" ,emacs-org)
                        ("emacs-emacsql" ,emacs-emacsql)
                       ))
   (description #f)
   (synopsis #f)
   (home-page "https://github.com/jethrokuan/org-roam")
   (license #f)
   ))

(define-public emacs-helm-fd
  (package
    (name "emacs-helm-fd")
    (version "20190923.48")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/helm-fd-"
               version
               ".el"))
        (sha256
          (base32
            "1mxfxrjk6mn6p32s4xa1hk97jh9hnzpgbd20inghrxw1gjjxyrij"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-helm" ,emacs-helm)))
    (home-page
      "https://github.com/lerouxrgd/helm-fd")
    (synopsis "Helm interface for fd command.")
    (description
      "Same as `helm-find' but using fd instead of find.  Also provides a
project scoped search function `helm-fd-project'.
")
    (license #f)))

(define-public emacs-icomplete-vertical
  (package
    (name "emacs-icomplete-vertical")
    (version "20200417.1755")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/icomplete-vertical-"
               version
               ".el"))
        (sha256
          (base32
            "01k4lqhzps4f0y97fynd0z6lnd6zi4c397zsrpbvi6b3js7p82y9"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/oantolin/icomplete-vertical")
    (synopsis
      "Display icomplete candidates vertically")
    (description
      "This package defines a global minor mode to display Icomplete
completion candidates vertically.  You could get a vertical display
without this package, by using (setq icomplete-separator \"\
\"), but
that has two problems which `icomplete-vertical-mode' solves:

1. Usually the minibuffer prompt and the text you type scroll off
   to the left!  This conceals the minibuffer prompt, and worse,
   the text you enter.

2. The first candidate appears on the same line as the one you are
   typing in.  This makes it harder to visually scan the candidates
   as the first one starts in a different column from the others.

For users that prefer the traditional horizontal mode for Icomplete
but want to define some commands that use vertical completion, this
package provides the `icomplete-vertical-do' macro.  For example to
define a command to yank from the kill-ring using completion:

(defun insert-kill-ring-item ()
  \"Insert item from kill-ring, selected with completion.\"
  (interactive)
  (icomplete-vertical-do (:separator \"\
··········\
\" :height 20)
    (insert (completing-read \"Yank: \" kill-ring nil t))))

Both the :separator and :height are optional and default to
icomplete-vertical-separator and to
icomplete-vertical-prospects-height, respectively.
If you omit both parts you still need to include the empty
parenthesis: (icomplete-vertical-do () ...)!.
")
    (license #f)))

(define-public emacs-modern-fringes
  (package
    (name "emacs-modern-fringes")
    (version "20200321.1817")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/modern-fringes-"
               version
               ".el"))
        (sha256
          (base32
            "12nqdi1hch8ndsh4a8bxvk89fx8c8ga5rip1b4j76wdl0cmqdpgw"))))
    (build-system emacs-build-system)
    (home-page
      "http://github.com/specialbomb/emacs-modern-fringes")
    (synopsis
      "Replaces default fringe bitmaps with better looking ones")
    (description
      "modern-fringes is meant to replace the arguably ugly default fringe bitmaps
with more modern, easier on the eyes ones.  They are very simple to use,
simply use the modern-fringes-mode.  As one knows, you may use customize to
make the mode permanent.  It is a global minor mode, so it will affect all
buffers.

It is suggested to use the following function in your init file to
use modern-fringes at intended.  It makes the truncation arrows appear
transparent, making a very easy on the eyes zipper-effect.

   (modern-fringes-invert-arrows)

Depending on your theme, it may not work
properly.  In that case, you can edit the bitmap faces as you wish in your
config.  modern-fringes was designed assuming the fringe width is 8 pixels
wide.  It will likely look strange if the width is any less or more.")
    (license #f)))
