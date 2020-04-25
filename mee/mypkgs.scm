;; mypkgs.scm
;; Copyright (C) 2019 Martin Edstrom

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

(define-module (mee mypkgs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system font)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system go)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system node)
  #:use-module (guix build-system r)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages video)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages check)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages node)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages autogen)
  
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages ncurses)
  )

;; Wishlist
;;
;; https://github.com/sharkdp/bat
;; https://github.com/Ventto/mons
;; https://sourceforge.net/projects/narocad/
;; http://solvespace.com/index.pl
;; https://github.com/lpereira/hardinfo
;; (define-public dracut)

(define-public tdm
  (package
    (name "tdm")
    (version "1.4.1")
    (source
     (origin
     (method git-fetch)
     (uri (git-reference (url "https://github.com/dopsi/console-tdm.git")
                         (commit (string-append "v" version))))
     (sha256 (base32 "0r6rcs6y5pw00nizdfrz00y9alpwrc0vj2xvd6snkqrzb97vfhdn"))
     (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs `(("xinit" ,xinit)
              ("ncurses" ,ncurses)
              ("bash" ,bash-minimal)))
    (arguments `(#:make-flags (list
                               (string-append "PREFIX=")
                               (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                               (string-append "SH=" (assoc-ref %build-inputs "bash")))
                 #:phases (modify-phases %standard-phases
                            (delete 'configure)
                            (delete 'check)
                            ;; the install phase fails, and needed for manpage
                            (delete 'install)
                            )))
    (synopsis "Also called console-tdm. A simple login manager forked from CDM.")
    (home-page "")
    (description "")
    (license license:gpl3)
    ))

(define-public duc
  (package
   (name "duc")
   (version "1.4.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference (url "https://github.com/zevv/duc.git")
                         (commit version)))
     (sha256 (base32 "1i7ry25xzy027g6ysv6qlf09ax04q4vy0kikl8h0aq5jbxsl9q52"))
     (file-name (git-file-name name version))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags '("--disable-ui" ;; needs ncursesw
                                    )))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("cairo" ,cairo)
      ("pango" ,pango)
      ("zlib" ,zlib)
      ("tokyocabinet" ,tokyocabinet)))
   (synopsis "Like du, but uses an index for speed and can generate graphs.")
   (home-page "")
   (description "Provides commands such as `duc ls' which is like `du -hs * | sort -hr',
and `duc gui' which is similar to baobab (the Gnome Disk Analyzer). Duc uses an
index to provide instant results, but `duc index' should be run periodically
for the results to be relevant. The use-case of this improved speed is clear in
emacs' dired-du-mode, for example.")
   (license license:expat)))

(define-public duc-no-x
  (package
   (inherit duc)
   (name "duc-no-x")
   (arguments '(#:configure-flags '("--disable-ui"
                                    "--disable-x11"
                                    "--disable-cairo"
                                    "--disable-opengl")))   
   (inputs
    `(("zlib" ,zlib)
      ("tokyocabinet" ,tokyocabinet)))))

;; Running agedu --version returns "revision 6.66". Assuming that's a version
;; number, not a git revision.
(define-public agedu
  (let ((commit "963bc9d923cec66711706a6f633a7443b8927259")
        (revision "0"))
    (package
     (name "agedu")
     (version (git-version "6.66" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference (url "https://git.tartarus.org/simon/agedu.git")
                           (commit commit)))
       (sha256 (base32 "1jmvgg2v6aqgbgpxbndrdhgfhlglrq4yv4sdbjaj6bsz9fb8lqhc"))
       (file-name (git-file-name name version))))
     (build-system gnu-build-system)
     (arguments
      '(#:phases
        (modify-phases %standard-phases
                       ;; ./agedu.1 is not in the git repo :/
                       ;; (it's in the tarball. change source?)
                       ;; Don't bother looking for agedu.1.
                       (add-after 'unpack 'remove-manpage-maker
                                  (lambda* _
                                    (substitute* "Makefile.am"
                                                 (("man1_MANS = agedu.1") ""))
                                    #t)))))   
     (native-inputs `(("autoconf" ,autoconf)
                      ("automake" ,automake)
                      ("gettext" ,gettext-minimal)
                      ("libtool" ,libtool)))
     (synopsis "Like du, but work from an index of sizes and access times.")
     (home-page "https://www.chiark.greenend.org.uk/~sgtatham/agedu/")
     (description "Agedu makes it easier to find forgotten large files, e.g. an
archive you unpacked once and haven't accessed since. It can also be used
simply as an indexed du.")
     (license license:expat))))

;; (define-public mons
;;   (package
;;    (name "mons")
;;    (version "0.8.2")
;;    (source (origin
;;             (method git-fetch)
;;             (uri (git-reference (url "https://github.com/Ventto/mons.git")
;;                                 (commit (string-append "v" version))))
;;             (sha256
;;              (base32 "1al3jcwarx3wh956h250glq88g9qbs04bkjsr2zxczy781dcvb9g"))
;;             (file-name (git-file-name name version)))))
;;   (build-system gnu-build-system)
;;   (home-page "https://github.com/Ventto/mons")
;;   (synopsis "Quickly manage monitors on X. Alternative to xrandr.")
;;   (description "")
;;   (license license:expat))

;; wip
(define-public maildir-guix
  (package
   (name "maildir-guix")
   (version "1.20200315")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "file:///home/keep/code/maildir-guix/.git")
                                (commit version)))
            (sha256
             (base32 "0pcl1p5m83kmmfxcjkj77jh3zck90l2jry3sd8c9dl2xvy5z3mrv"))
            (file-name (git-file-name name version))))
   (build-system copy-build-system)
   (arguments '(#:install-plan '(("Maildir" "share/Maildir-guix"))))
   (inputs `(("rename" ,rename)
             ("mb2md" ,mb2md)
             ;; ("rsync" ,rsync)
             ("wget" ,wget)))
   (synopsis #f)
   (description "Maildir containing the archives for the mailing lists
guix-devel, bug-guix and help-guix. To try it, execute something like the
following.

  guix install mu maildir-guix
  mkdir ~/Maildir
  cp -nt ~/Maildir ~/.guix-profile/share/Maildir-guix/*
  mu index")
   (home-page #f)
   (license license:gpl3+)))

;; Prolly should have the repository that includes the scraper as well as the
;; scraped items, for trust reasons
(define-public logs-freenode-guix
  (package
    (name "logs-freenode-guix")
    (version "2020.03.07")
    (source (origin
              (method url-fetch)
              (uri "file:///home/keep/guix/logs.guix.gnu.org.tar.gz")
              (sha256
               (base32 "0j92gl1vihygfwygpz9brrgl62d76vbx7p7b800i2cj0k7iyd2fh"))))
    (build-system copy-build-system)
    (arguments '(#:install-plan '(("." "share/logs-freenode-guix"))))
    (synopsis "Chat logs from logs.guix.gnu.org.")
    (description #f)
    (home-page #f)
    (license #f)))

;; (define-public rust-1.40
;;   (let ((base-rust
;;          ((@@ (gnu packages rust) rust-bootstrapped-package)
;;           rust-1.39 "1.40.0"
;;           "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx")))
;;     (package
;;       (inherit base-rust)
;;       (source
;;        (origin
;;          (inherit (package-source base-rust))
;;          (snippet '(begin
;;                      (delete-file-recursively "src/llvm-project")
;;                      (delete-file-recursively "vendor/jemalloc-sys/jemalloc")
;;                      #t))))
;;       (arguments
;;        (substitute-keyword-arguments (package-arguments base-rust)
;;          ((#:phases phases)
;;           `(modify-phases ,phases  
;;              ;; This phase was a fix for rust-1.26, upstream fixed it in 1.28.
;;              (delete 'disable-cargo-test-for-nightly-channel)
;;              ;; (replace 'disable-cargo-test-for-nightly-channel
;;              ;;   (lambda* _
;;              ;;     (substitute* "src/tools/cargo/crates/resolver-tests/tests/resolve.rs"
;;              ;;       (("fn test_resolving_minimum_version_with_transitive_deps")
;;              ;;        "#[ignore]\nfn test_resolving_minimum_version_with_transitive_deps"))
;;              ;;     #t))
;;              ;; File is gone now. Maybe it's src/test/run-make-fulldeps/stdin-non-utf8/non-utf8?
;;              (delete 'remove-unsupported-tests)
;;              ))))
;;       (native-inputs (append `(("perl" ,perl)
;;                                ("python" ,python)
;;                                ;;("ruby" ,ruby)
;;                                )
;;                              (package-native-inputs base-rust))
;;        ))))

;; (define-public rust-1.41
;;   (let ((base-rust
;;          ((@@ (gnu packages rust) rust-bootstrapped-package)
;;           rust-1.40 "1.41.0"
;;           "0jypz2mrzac41sj0zh07yd1z36g2s2rvgsb8g624sk4l14n84ijm")))
;;     (package
;;       (inherit base-rust)
;;       (arguments
;;        (substitute-keyword-arguments (package-arguments base-rust)
;;          ((#:phases phases)
;;           `(modify-phases ,phases
;;              (replace 'patch-command-exec-tests
;;                ,((@@ (gnu packages rust) patch-command-exec-tests-phase)
;;                  "src/test/ui/command/command-exec.rs"))
;;              (replace 'patch-command-uid-gid-test
;;                (lambda _
;;                  (substitute* "src/test/ui/command/command-uid-gid.rs"
;;                    (("/bin/sh") (which "sh"))
;;                    (("ignore-sgx") "ignore-sgx\n// ignore-tidy-linelength"))
;;                  #t))
;;              )))))))

;; (define-public rust-1.42
;;   (let ((base-rust
;;          ((@@ (gnu packages rust) rust-bootstrapped-package)
;;           rust-1.40 "1.42.0"
;;           "0x9lxs82may6c0iln0b908cxyn1cv7h03n5cmbx3j1bas4qzks6j")))
;;     (package
;;       (inherit base-rust)
;;       (source
;;        (origin
;;          (inherit (package-source base-rust))
;;          (snippet '(begin
;;                      (delete-file-recursively "src/llvm-project")
;;                      (delete-file-recursively "vendor/jemalloc-sys/jemalloc")
;;                      #t))))
;;       (arguments
;;        (substitute-keyword-arguments (package-arguments base-rust)
;;          ((#:phases phases)
;;           `(modify-phases ,phases
;;              (replace 'patch-command-exec-tests
;;                ,((@@ (gnu packages rust) patch-command-exec-tests-phase)
;;                  "src/test/ui/command/command-exec.rs"))
;;              (replace 'patch-command-uid-gid-test
;;                (lambda _
;;                  (substitute* "src/test/ui/command/command-uid-gid.rs"
;;                    (("/bin/sh") (which "sh"))
;;                    (("ignore-sgx") "ignore-sgx\n// ignore-tidy-linelength"))
;;                  #t))
;;              ;; This phase was a fix for rust-1.26, upstream fixed it in 1.28.
;;              (delete 'disable-cargo-test-for-nightly-channel)
;;              ;; (replace 'disable-cargo-test-for-nightly-channel
;;              ;;   (lambda* _
;;              ;;     (substitute* "src/tools/cargo/crates/resolver-tests/tests/resolve.rs"
;;              ;;       (("fn test_resolving_minimum_version_with_transitive_deps")
;;              ;;        "#[ignore]\nfn test_resolving_minimum_version_with_transitive_deps"))
;;              ;;     #t))
;;              ;; File is gone now. Maybe it's src/test/run-make-fulldeps/stdin-non-utf8/non-utf8?
;;              (delete 'remove-unsupported-tests)
;;              ))))
;;       (native-inputs (append `(("perl" ,perl)
;;                                ("python" ,python)
;;                                ;;("ruby" ,ruby)
;;                                )
;;                              (package-native-inputs base-rust))
;;        ))))

(define-public hawck
  (package
   (name "hawck")
   (version "0-guix0")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/snyball/Hawck")
                                (commit "d7ba7ae5f15f7b196e7afe33c308941e56bb7b16")))
            (file-name (git-file-name name version))
            (sha256 (base32 "0m96al056pw88lpcjhly167gaqvh73lkwz3l105pisxadbzx3hfg"))))
   (build-system gnu-build-system)
   (inputs `(;; meson
             ;; doxygen
             ;; gcc
             ;; g++
             ;; pkg-config
             ;; lua5.3
             ;; liblua5.3-dev
             ;; libgtk-3-dev
             ;; libgtksourceview-3.0-dev
             ;; libnotify-dev
             ;; python3
             ;; python3-setuptools
             ;; python3-pip
             ;; zenity
             ;; wget
             ;; gawk
             ;; gzip
             ;; console-data
             ;; python3-dill
             ))
   (synopsis "")
   (description "")
   (home-page "https://github.com/snyball/Hawck")
   (license license:bsd-2)))

(define-public opendoas
  (package
   (name "opendoas")
   (version "6.6.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/Duncaen/OpenDoas")
                                (commit (string-append "v" version))))
            (sha256 (base32 "07kkc5729p654jrgfsc8zyhiwicgmq38yacmwfvay2b3gmy728zn"))
            (file-name (git-file-name name version))))
   (build-system gnu-build-system)
   (native-inputs `(("glibc" ,glibc)
                    ("gcc-toolchain" ,gcc-toolchain)
                    ("libbsd" ,libbsd)
                    ))
   (propagated-inputs `(("glibc" ,glibc)))
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key build inputs outputs #:allow-other-keys)
            (let ((bash (which "bash"))
                  (out  (assoc-ref outputs "out")))
              (setenv "CONFIG_SHELL" bash)
              (setenv "SHELL" bash)
              (invoke bash "./configure"
                      (string-append "--prefix=" out)
                      (string-append "--sysconfdir=" out "/etc")
                      (string-append "--build=" build)))))
        )))
   (synopsis "")
   (description "")
   (home-page "https://github.com/Duncaen/OpenDoas")
   (license license:isc)))

(define-public signal
  (package
   (name "signal")
   (version "")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/signalapp/Signal-Desktop")
                                (commit version)))
            (sha256 #f)))
   ;; (properties `((upstream-name . "org.thoughtcrime.securesms"))) ;; or something
   (build-system gnu-build-system)
   (inputs `(("node" ,node)
             ("python" ,python)))
   (synopsis "")
   (home-page "https://signal.org/")
   (description "")
   (license #f)))

(define-public joplin
  (package
   (name "joplin")
   (version "1.0.176")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/laurent22/joplin.git")
                                (commit (string-append "v" version))))
            (sha256 #f)))
   (build-system gnu-build-system)
   (inputs `())
   ;; "@types/react": "^16.9.16",
   ;; "@types/react-dom": "^16.9.4",
   ;; "@typescript-eslint/eslint-plugin": "^2.10.0",
   ;; "@typescript-eslint/parser": "^2.10.0",
   ;; "eslint": "^6.1.0",
   ;; "eslint-plugin-react": "^7.14.3",
   ;; "husky": "^3.0.2",
   ;; "lint-staged": "^9.2.1",
   ;; "typescript": "^3.7.3"
   (home-page "joplinapp.org")
   (synopsis "Note-taking app similar to Evernote")
   (description "")
   (license license:expat)))

(define-public anonymouth
  (package
   (name "anonymouth")
   (version "0.5")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/psal/anonymouth.git")
                                (commit "7bd8e24b661f587facf8715a1de345dffde9734e")))
            (sha256 #f)))
   (build-system gnu-build-system)
   (home-page "")
   (synopsis "")
   (description "")
   (license #f)))

(define-public ink
  (package
   (name "ink")
   (version "0.9.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/inkle/ink.git")
                                (commit version)))
            (sha256 #f)))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-after 'configure 'build-clr-app
                                (lambda _ (invoke "sh" "xbuild ink.sln")))
                     (delete 'configure)
                     (delete 'make))))
   (inputs `(("mono" ,mono)))
   (home-page "https://www.inklestudios.com/ink/")
   (synopsis "")
   (description "")
   (license license:expat)))

(define-public go-github-com-radovskyb-watcher
  (package
   (name "go-github-com-radovskyb-watcher")
   (version "1.0.7")
   (source (origin (method git-fetch)
                   (uri (git-reference (url "https://github.com/radovskyb/watcher.git")
                                       (commit "v1.0.7")))
                   (file-name (git-file-name name version))
                   (sha256 (base32 "118m25h1nif274v7wzgjfs1i2acydndc226gvbcf9l9b5qaayl7w"))))
   (build-system go-build-system)
   (arguments `(#:import-path "github.com/radovskyb/watcher"))
   (synopsis #f)
   (home-page #f)
   (description #f)
   (license license:bsd-3)))

(define-public go-github-com-paulrosania-go-charset-charset
  (package
   (name "go-github-com-paulrosania-go-charset-charset")
   (version "0.0.0-20190326053356-55c9d7a5834c")
   (source (origin (method git-fetch)
                   (uri (git-reference (url "https://github.com/paulrosania/go-charset.git")
                                       (commit "55c9d7a5834c4d034b34bf042103dbc646887f4f")))
                   (file-name (git-file-name name version))
                   (sha256 (base32 "1qinvy7jblsngw4fbz2f05jzqrzlzpxha733j0g9ipzvwhzzjdiv"))))
   (build-system go-build-system)
   (arguments '(#:import-path "github.com/paulrosania/go-charset/charset"
                #:unpack-path "github.com/paulrosania/go-charset"))
   (home-page #f)
   (synopsis #f)
   (description #f)
   (license license:bsd-3)))

(define-public go-github-com-paulrosania-go-charset-data
  (package
   (name "go-github-com-paulrosania-go-charset-data")
   (version "0.0.0-20190326053356-55c9d7a5834c")
   (source (origin (method git-fetch)
                   (uri (git-reference (url "https://github.com/paulrosania/go-charset.git")
                                       (commit "55c9d7a5834c4d034b34bf042103dbc646887f4f")))
                   (file-name (git-file-name name version))
                   (sha256 (base32 "1qinvy7jblsngw4fbz2f05jzqrzlzpxha733j0g9ipzvwhzzjdiv"))))
   (build-system go-build-system)
   (arguments '(#:import-path "github.com/paulrosania/go-charset/data"
                #:unpack-path "github.com/paulrosania/go-charset"))
   (home-page #f)
   (synopsis #f)
   (description #f)
   (license license:bsd-3)))

(define-public go-github-com-masterminds-semver
  (package
   (name "go-github-com-masterminds-semver")
   (version "3.0.3")
   (source (origin (method git-fetch)
                   (uri (git-reference (url "https://github.com/Masterminds/semver.git")
                                       (commit (string-append "v" version))))
                   (file-name (git-file-name name version))
                   (sha256 (base32 "1b3956cdk0ps1vqjf0al05xzw8b61rvswk59qmjwqypkqp3a2n8c"))))
   (build-system go-build-system)
   (arguments '(#:import-path "github.com/Masterminds/semver"))
   (home-page #f)
   (synopsis #f)
   (description #f)
   (license license:bsd-3)))

(define-public tweego
  (package
   (name "tweego")
   (version "2.1.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference (url "https://github.com/tmedwards/tweego.git")
                         (commit (string-append "v" version))))
     (sha256 (base32 "130rcphw7icq2rx4wg0c380wwgkwx7d5wd1v2vp3wkvj42ckjkrc"))
     (file-name (git-file-name name version))))
   (build-system go-build-system)
   (arguments `(#:import-path "github.com/tmedwards/tweego"))
   (inputs `(("unzip" ,unzip)
             ("go-golang-org-x-text" ,go-golang-org-x-text)
             ("go-golang-org-x-net" ,go-golang-org-x-net)
             ("go-github-com-masterminds-semver" ,go-github-com-masterminds-semver)
             ("go-github-com-radovskyb-watcher" ,go-github-com-radovskyb-watcher)
             ("go-github-com-paulrosania-go-charset-charset" ,go-github-com-paulrosania-go-charset-charset)
             ("go-github-com-paulrosania-go-charset-data" ,go-github-com-paulrosania-go-charset-data)))
   ;; Need at least one story format, otherwise tweego --help won't be helpful.
   ;; The Sugarcube format comes from the same developer, not a stretch to ship.
   (propagated-inputs `(("sugarcube2" ,sugarcube2)))
   (synopsis "Compile interactive fiction stories from Twee markup language.")
   (home-page "https://www.motoslave.net/tweego/")
   (description "A compiler for Twine stories that does not require the
Twine graphical editor. Supports the notations of Twee v1, Twee v3 and the
unofficial Twee2.")
   (license license:bsd-2)))

;; Wishlist: convert the online documentation to an info manual, as I
;; constantly refer to it
(define-public sugarcube2
  (let ((story-formats-dir "share/storyformats"))
    (package
     (name "sugarcube2")
     (version "2.31.1")
     (source
      (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tmedwards/sugarcube-2/releases/download/v"
             version "/sugarcube-" version "-for-twine-2.1-local.zip"))
       (sha256
        (base32 "0vbzgjfh910qyjyc2s0hqaa163bzxdkgk2acsqvylfik7xqjp7xj"))))
     (inputs `(("unzip" ,unzip)))
     (build-system copy-build-system)
     (arguments `(#:install-plan
                  `(("." ,(string-append ,story-formats-dir "/sugarcube-2")))))
     (native-search-paths (list (search-path-specification
                                 (variable "TWEEGO_PATH")
                                 (files `(,story-formats-dir)))))
     (synopsis "The Sugarcube 2 story format for use by Twine >2.1 and Tweego.")
     (home-page "https://www.motoslave.net/sugarcube/2/")
     (description
      (string-append
       "A so-called story format is a collection of macros and stylesheets,
necessary to compile any story with Twine/Twee/Tweego. This will be made
available to Tweego instantly, but to add it to Twine, you must do manual
clicking: find the file in ~/.guix-profile/" story-formats-dir ", 
or the equivalent location if your Guix profile is elsewhere."))
     (license license:bsd-2))))

;; ;; Nope, needs Node.js!
;; (define-public tweego-harlowe
;;   (package
;;    (name "tweego-harlowe")
;;    (version "3.1.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (string-append "https://bitbucket.org/_L_/harlowe/get/v" version ".tar.gz"))
;;      (sha256 (base32 "0asbidpl0hgx4jkwymcz54rb5q334w2cfca16lp4cwaj18iqnvkr"))))
;;    (inputs `(("unzip" ,unzip)
;;              ("ruby" ,ruby)
;;              ("ruby-sass" ,ruby-sass)
;;              ("node" ,node)))
;;    (build-system gnu-build-system)
;;    (arguments
;;     `(#:phases (modify-phases %standard-phases
;;                               (delete 'configure)
;;                               (delete 'check) ;; no such step
;;                               )))
;;    (native-search-paths (list (search-path-specification
;;                                (variable "TWEEGO_PATH")
;;                                (files '("share/storyformats")))))
;;    (synopsis "The Harlowe 'story format' for use by Twine >2.1 and Tweego.")
;;    (home-page "https://twine2.neocities.org/")
;;    (description #f)
;;    (license license:zlib)))

(define-public emacs-rainbow-delimiters-e-c-d
  (package
   (inherit emacs-rainbow-delimiters)
   (name "emacs-rainbow-delimiters-e-c-d")
   (source (origin (method git-fetch)
                   (uri (git-reference (url "https://github.com/e-c-d/rainbow-delimiters.git")
                                       (commit "43b7fe7fe1df25703c83fc2ef880281a6ec67e80e")))
                   (sha256 #f)))))

;; Guix has node.js, but none of its packages. This package wants to pull in
;; "colors". It works in guix environment to do npm install colors, and npm
;; install in general (auto pulling in the dependency). But with guix build,
;; which also calls "npm install", it does not work. Perhaps it doesn't allow
;; network downloads (irreproducible). Solution: Put the source for "colors"
;; inside the build directory, along with the other bundled dependencies, and
;; change the reference in package.json to point to it.
;; Alternative: Purge all "colors/safe" references in the code, as it just has
;; to do with the console.
;; https://www.zotero.org/support/dev/client_coding/building_the_standalone_client
(define-public zotero
  (package
   (name "zotero")
   (version "5.0.74")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/zotero/zotero.git")
                                (commit "5.0.74-hotfix")))
            (file-name (git-file-name name version))
            (sha256 (base32 "1mjwkafp1d35lppmh9xm0l2c34600v40g5vyqpyfbmmbgf2qs6vs"))))
   (build-system node-build-system)
   (inputs `(("node" ,node)
             ("node-colors" ,node-colors)))
   (propagated-inputs `(("node" ,node)))
   (arguments
    '(#:phases
      (modify-phases %standard-phases
                     ;; (add-before 'build 'get-deps
                     ;;             (lambda _
                     ;;               (invoke "npm" "install" "yarn")
                     ;;               (invoke "yarn" "install" "colors")))
                     (add-after 'build 'fix-vulns
                                (lambda _ (invoke "sh" "npm audit fix"))))))
   (synopsis "Bibliography manager")
   (description "A manager for bibliography and references, used by researchers
or anyone who finds occasion to reuse citations. Can be used simply as a
book-library, but its raison d'etre is to easily track down all citations and
bookmarks you've ever made. Lets you store a copy of referenced works,
auto-populate metadata fields, and keep your own notes and bookmarks on each
work. Extensions let it talk to other apps.")
   (home-page "https://www.zotero.org/")
   (license license:agpl3)))

(define-public node-colors
  (package
   (name "node-colors")
   (version "1.4.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/Marak/colors.js.git")
                                (commit "v1.4.0")))
            (file-name (git-file-name name version))
            (sha256
             (base32 "1ih98ycxjprlxn72ygqgkgcp9wkpd20apndjd11270qyyifvkr8y"))))
   (build-system node-build-system)
   (properties `((upstream-name . "colors.js")))
   (arguments `(#:tests? #f))
   (inputs `(("node" ,node)))
   (propagated-inputs `(("node" ,node)))
   (synopsis "Colors.js")
   (description "")
   (home-page "https://github.com/Marak/colors.js")
   (license license:expat)))

(define-public ghc-terminal-progress-bar
  (package
   (name "ghc-terminal-progress-bar")
   (version "0.4.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/terminal-progress-bar/terminal-progress-bar-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "092yx9hal3xxcvpjwyqbfqww277l4gf83272pcnp3k6aj86a2756"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-terminal-size" ,ghc-terminal-size)))
   (native-inputs
    `(("ghc-hunit" ,ghc-hunit)
      ("ghc-test-framework" ,ghc-test-framework)
      ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
   (home-page
    "https://github.com/roelvandijk/terminal-progress-bar")
   (synopsis "A progress bar in the terminal")
   (description
    "A progress bar conveys the progress of a task. This package implements a progress bar that is displayed in a terminal. . See the module 'System.ProgressBar' to get started or look at the terminal-progress-bar-example package. . The animated progress bar depends entirely on the interpretation of the carriage return character (\\'\\\\r\\'). If your terminal interprets it as something else than \\\"move cursor to beginning of line\\\", the animation won't work.")
   (license license:bsd-3)))

(define-public arbtt
  (package
   (name "arbtt")
   (version "0.10.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/arbtt/arbtt-"
           version ".tar.gz"))
     (sha256
      (base32
       "02izfga7nv2saq4d1xwigq41hhbc02830sjppqsqw6vcb8082vs1"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (inputs
    `(("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-strict" ,ghc-strict)
      ("ghc-aeson" ,ghc-aeson)
      ("ghc-x11" ,ghc-x11)
      ("ghc-pcre-light" ,ghc-pcre-light)
      ("ghc-terminal-progress-bar" ,ghc-terminal-progress-bar)
      ("ghc-bytestring-progress" ,ghc-bytestring-progress)
      ("ghc-conduit" ,ghc-conduit)
      ("ghc-exceptions" ,ghc-exceptions)
      ("ghc-attoparsec" ,ghc-attoparsec)
      ("ghc-resourcet" ,ghc-resourcet)
      ("ghc-unliftio-core" ,ghc-unliftio-core)))
   (native-inputs
    `(("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-golden" ,ghc-tasty-golden)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-process-extras" ,ghc-process-extras)))
   (home-page "http://arbtt.nomeata.de/")
   (synopsis "Automatic Rule-Based Time Tracker")
   (description
    "arbtt is a background daemon that stores which windows are open, which one
has the focus and how long since your last action (and possbly more sources
later), and stores this. It is also a program that will, based on expressive
rules you specify, derive what you were doing, and what for. WARNING: The log
file might contain very sensitive private data. Make sure you understand the
consequences of a full-time logger and be careful with this data.")
   (license license:gpl2)))

(define-public ghc-bytestring-progress
  (package
   (name "ghc-bytestring-progress")
   (version "1.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/bytestring-progress/bytestring-progress-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "140dn6zyc1ka8vjxwd6zzc3mwd651zrawcwk3d5ipfxrgddf9bws"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-terminal-progress-bar"
       ,ghc-terminal-progress-bar)))
   (home-page
    "http://github.com/acw/bytestring-progress")
   (synopsis
    "A library for tracking the consumption of a lazy ByteString")
   (description
    "In some cases, it is useful to know how fast a ByteString is being consumed. Typically, this could be to report some measure of progress to a waiting user, but it could also be to perform some form of testing on input / consumption code.")
   (license license:bsd-3)))

;; (define-public python-pytorch
;;   (package
;;    (name "python-pytorch")
;;    (version "1.3.0")
;;    (source
;;     (origin
;;      (method git-fetch)
;;      (uri (git-reference (url "https://github.com/pytorch/pytorch")
;;                          (commit (string-append "v" version))))
;;      (sha256
;;       (base32
;;        "1i1sxiy7qkdcvrh6rs6s4m99492fsysfw1vcp2xn0f7j12w1irjm"))))
;;    (build-system python-build-system)
;;    (properties `((upstream-name . "pytorch")))
;;    (propagated-inputs
;;     `(("python-future" ,python-future)
;;       ("python-numpy" ,python-numpy)
;;       ("python-pyyaml" ,python-pyyaml)
;;       ("python-requests" ,python-requests)
;;       ("python-setuptools" ,python-setuptools)
;;       ("python-six" ,python-six)
;;       ("python-typing" ,python-typing)
;;       ))
;;    (home-page "")
;;    (synopsis "")
;;    (description "")
;;    (license license:bsd-3)))

;; (define-public renpy
;;   (package
;;     (name "renpy")
;;     (version "7.3.4.596")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference (url "https://github.com/renpy/renpy.git")
;;                            (commit version)))
;;        (sha256 #f)))
;;     (build-system python-build-system)
;;     (inputs
;;      `(
;;        ("python-virtualenv" ,python-virtualenv)
;;        ("python-pygame-sdl2" ,python-pygame-sdl2)
;;        ("python-cython" ,python-cython)
;;        ("python-sphinx" ,python-sphinx)
;;        ("libavc1394" ,libavc1394)
;;        ("sdl2" ,sdl2)
;;        ("sdl2-gfx" ,sdl2-gfx)
;;        ("sdl2-image" ,sdl2-image)
;;        ("sdl2-mixer" ,sdl2-mixer)
;;        ("sdl2-ttf" ,sdl2-ttf)
;;        ("fribidi" ,fribidi)
;;        ;; ("freetype" ,freetype)
;;        ("glew" ,glew)
;;        ("libjpeg-turbo" ,libjpeg-turbo)))
;;     (arguments
;;      '(#:phases
;;        (modify-phases %standard-phases
;;          (replace 'build
;;            (lambda _
;;              (invoke "sh" "python module/setup.py install")))
;;          (delete 'build))))
;;     (synopsis #f)
;;     (description #f)
;;     (home-page #f)
;;     (license #f)))

;; ;; see def of python-pygame
;; (define-public python-pygame-sdl2
;;   (package
;;     (name "python-pygame-sdl2")
;;     (version "renpy-7.3.4.596")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference (url "https://github.com/renpy/pygame_sdl2")
;;                            (commit version)))
;;        (sha256 #f)))
;;     (build-system python-build-system)
;;     (inputs
;;      `(
;;        ;; ("freetype" ,freetype)
;;        ("python-virtualenv" ,python-virtualenv)
;;        ("python-cython" ,python-cython)
;;        ("libavc1394" ,libavc1394)
;;        ("sdl2" ,sdl2)
;;        ("sdl2-gfx" ,sdl2-gfx)
;;        ("sdl2-image" ,sdl2-image)
;;        ("sdl2-mixer" ,sdl2-mixer)
;;        ("sdl2-ttf" ,sdl2-ttf)
;;        ("libjpeg" ,libjpeg)
;;        ("libpng" ,libpng)
;;        ("libX11" ,libx11)
;;        ("libsmpeg" ,libsmpeg)
;;        ("portmidi" ,portmidi)
;;        ("v4l-utils" ,v4l-utils)
;;        ))
;;     (synopsis #f)
;;     (description #f)
;;     (home-page #f)
;;     (license (list  zlib
;;                     bsd-2
;;                     expat
;;                     lgpl2.0+
;;                     lgpl2.1+
;;                     gpl3+
;;                     psfl
;;                     public-domain
;;                     lgpl2.1+))))



;; (define-public activitywatch
;;   (package
;;    (name "activitywatch")
;;    (version "v0.8.0b9")
;;    (source (origin
;;             (method git-fetch)
;;             (uri (git-reference (url "https://github.com/ActivityWatch/activitywatch")
;;                                 (commit version)
;;                                 (recursive? #t)))
;;             (sha256
;;              (base32 "1cqdyfpfmjl2a159zvrbxgx5n75794nvk9r91ng2afl6pidrdc4p"))
;;             (patches (list (plain-file "patch.diff" "commit 02c862de195ce85065c2e975b93d9928769e554f
;; Author: medstrom <medstrom@no.email>
;; Date:   Sat Oct 26 13:04:07 2019 +0200

;;     Patch for Guix packaging

;; diff --git a/Makefile b/Makefile
;; index 185ecbe..8b5bb13 100644
;; --- a/Makefile
;; +++ b/Makefile
;; @@ -29,11 +29,6 @@ SHELL := /usr/bin/env bash
;;  #    packages as user packages (same as `pip install --user <pkg>`). This makes
;;  #    it possible to install without using a virtualenv (or root).
;;  build:
;; -	if [ -e \"aw-core/.git\" ]; then \\
;; -		echo \"Submodules seem to already be initialized, continuing...\"; \\
;; -	else \\
;; -		git submodule update --init --recursive; \\
;; -	fi
;;  #
;;  	make --directory=aw-core build DEV=$(DEV)
;;  	make --directory=aw-client build DEV=$(DEV)")))))
;;    (build-system gnu-build-system)
;;    (arguments '(#:phases
;;                 (modify-phases %standard-phases
;;                                (delete 'configure))))
;;    (inputs `(("python" ,python)
;;              ("node" ,node)

;;              ("python-strict-rfc3339" ,python-strict-rfc3339)
;;              ("python-iso8601" ,python-iso8601)
;;              ("python-takethetime" ,python-takethetime)
;;              ("python-appdirs" ,python-appdirs)
;;              ("python-peewee" ,python-peewee)
;;              ("python-requests" ,python-requests)
;;              ("python-jsonschema" ,python-jsonschema)
;;              ("python-persist-queue" ,python-persist-queue)
;;              ("python-pyqt" ,python-pyqt)
;;              ("python-aniso8601" ,python-aniso8601)
;;              ("python-attrs" ,python-attrs)
;;              ("python-click" ,python-click)
;;              ("python-flask-cors" ,python-flask-cors)
;;              ("python-flask-restplus" ,python-flask-restplus)
;;              ("python-flask" ,python-flask)
;;              ("python-itsdangerous" ,python-itsdangerous)
;;              ("python-jinja2" ,python-jinja2)
;;              ("python-markupsafe" ,python-markupsafe)
;;              ("python-pyrsistent" ,python-pyrsistent)
;;              ("python-pytz" ,python-pytz)
;;              ("python-relativetimebuilder" ,python-relativetimebuilder)
;;              ("python-six" ,python-six)
;;              ("python-werkzeug" ,python-werkzeug)

;;              ;; darwin
;;              ("python-pyobjc-framework-Quartz" ,python-pyobjc-framework-Quartz)
;;              ("python-pyobjc" ,python-pyobjc)

;;              ;; win32
;;              ("python-pypiwin32" ,python-pypiwin32)
;;              ("python-wmi" ,python-wmi)

;;              ;; linux
;;              ("python-pyuserinput" ,python-pyuserinput)
;;              ("python-xlib" ,python-xlib)

;;              ;; dev
;;              ("python-pytest" ,python-pytest)
;;              ("python-pytest-cov" ,python-pytest-cov)
;;              ("python-mypy" ,python-mypy)
;;              ))
;;    (home-page "https://activitywatch.net/")
;;    (synopsis "ActivityWatch is an automatic time-tracking software which helps
;; you keep track of what you do.")
;;    (description "It's an attempt to replace and improve existing services like
;; RescueTime, ManicTime, WakaTime and others. It is different in that it's
;; completely open source, extendable, and you as a user have full and exclusive
;; control of your data.")
;;    (license license:mpl2.0)))

;; (define-public python-persist-queue
;;   (package
;;    (name "python-persist-queue")
;;    (version "0.3.5")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (pypi-uri "persist-queue" version))
;;      (sha256
;;       (base32
;;        "1bdz04ybjqczlp3x4wc4jq2dbr6r6zqbxak0l27av82irg91m2wn"))))
;;    (build-system python-build-system)
;;    (home-page
;;     "http://github.com/peter-wangxu/persist-queue")
;;    (synopsis
;;     "A thread-safe disk based persistent queue in Python.")
;;    (description
;;     "A thread-safe disk based persistent queue in Python.")
;;    (license license:bsd-3)))

;; (define-public sdl-sound
;;   (package
;;    (name "sdl-sound")
;;    (version "1.0.3")
;;    (source (origin
;;             (method url-fetch)
;;             (uri "https://icculus.org/SDL_sound/downloads/SDL_sound-1.0.3.tar.gz")
;;             (sha256 (base32 "1pz6g56gcy7pmmz3hhych3iq9jvinml2yjz15fjqjlj8pc5zv69r"))))
;;    (home-page "http://icculus.org/SDL_sound/")
;;    (build-system gnu-build-system)
;;    (synopsis #f)
;;    (description #f)
;;    (license license:lgpl2.1+)))

;; (define-public sdl-sound-ancurio
;;   (package
;;    (inherit sdl-sound)
;;    (name "sdl-sound-ancurio")
;;    (version "20170524")
;;    (source (origin (method git-fetch)
;;                    (uri (git-reference (url "https://github.com/Ancurio/SDL_sound")
;;                                        (commit "ab8c11a5093593f17fcfde2a9e115bdc48f77819")))
;;                    (sha256 #f)))
;;    (properties `((upstream-name . "SDL_sound")))
;;    (synopsis "Fork of sdl-sound.")))

;; (define-public sdl2-for-mkxp
;;   (package
;;    (inherit sdl2)
;;    (name "sdl2-for-mkxp")
;;    ;; apply this patch https://bugzilla.libsdl.org/show_bug.cgi?id=2745
;;    ;; as stated at https://github.com/ancurio/mkxp
;;    ))

;; (define-public mkxp
;;   (package
;;    (name "mkxp")
;;    (version "20190620")
;;    (source
;;     (origin
;;      (method git-fetch)
;;      (uri (git-reference
;;            (url "https://github.com/Ancurio/mkxp")
;;            (commit "9dc42914dea4e29fb2172ce130d313742ea08e6b")))
;;      (sha256
;;       (base32
;;        "161fk3xn6s4rig7di0rv5sb913d0jjly77naagfqabw25qdz1sp8"))))
;;    (inputs `(("physfs" ,physfs)
;;              ("openal" ,openal)
;;              ("sdl2-for-mkxp" ,sdl2-for-mkxp)
;;              ("sdl2-image" ,sdl2-image)
;;              ("sdl2-ttf"   ,sdl2-ttf)
;;              ("sdl-sound-ancurio"   ,sdl-sound-ancurio)
;;              ("libvorbis" ,libvorbis)
;;              ("libiconv"  ,libiconv)
;;              ("libsigc++"  ,libsigc++)
;;              ("pixman" ,pixman)
;;              ("zlib" ,zlib)
;;              ("boost" ,boost)
;;              ("mesa-headers" ,mesa-headers)
;;              ))
;;    (build-system gnu-build-system)
;;    (home-page "https://github.com/Ancurio/mkxp")
;;    (synopsis #f)
;;    (description "Open source implementation of the RGSS (Ruby Game Scripting
;; System) library, allowing you to play games made in RPG Maker XP/MV/VX/VX Ace,
;; at least in theory.")
;;    (license license:gpl2+)))

;; (define-public fonts-bitmap
;;   (package
;;    (name "fonts-bitmap")
;;    (version "5c101c91bf2ed0039aad02f9bf76ddb2018b1f21")
;;    (source (origin
;;             (method git-fetch)
;;             (uri (git-reference
;;                   (url "https://github.com/Tecate/bitmap-fonts.git")
;;                   (commit version)))
;;             (sha256
;;              (base32 "0s119zln3yrhhscfwkjncj72cr68694643009aam63s2ng4hsmfl"))))
;;    (properties '((upstream-name . "bitmap-fonts")))
;;    (build-system font-build-system)
;;    (home-page "https://github.com/Tecate/bitmap-fonts")
;;    (synopsis "Collection of monospaced bitmap fonts")
;;    (description "Collection of monospaced bitmap fonts on bdf or pcf format,
;; good for terminal use")
;;    (license #f)))

(define-public fonts-proggy
  (package
   (name "fonts-proggy")
   (version "15812db975e0ab0d810b0f4d2743811d8ef5a47e")
   (source
    (origin (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/bluescan/proggyfonts.git")
                  (commit "15812db975e0ab0d810b0f4d2743811d8ef5a47e")))
            (sha256
             (base32 "1dmgfn0gfanlfsy301yzk248x3zfv3m7m2pzil0za3yn1fj2h9wv"))))
   (properties '((upstream-name . "proggyfonts")))
   (build-system font-build-system)
   (home-page #f)
   (synopsis "Proggy monospace and Webby proportional fonts")
   (description #f)
   (license license:expat)))

(define-public fonts-oldschool-pc
  (package
   (name "fonts-oldschool-pc")
   (version "0.0-guix1")
   (source
    (origin (method url-fetch)
            (uri "https://int10h.org/oldschool-pc-fonts/download/ultimate_oldschool_pc_font_pack_v1.0.zip")
            (sha256 #f)))
   (properties '((upstream-name . "oldschool-pc-fonts")))
   (build-system font-build-system)
   (home-page #f)
   (synopsis #f)
   (description #f)
   (license #f)))
