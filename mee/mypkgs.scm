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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system node)
  #:use-module (guix build-system r)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cargo)
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
  #:use-module (gnu packages python-crypto)
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
  #:use-module (gnu packages time)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages crates-io)
  )

;; Wishlist
;;
;; https://www.nongnu.org/renameutils/
;; https://github.com/ngirard/lolcate-rs
;; https://github.com/sharkdp/bat
;; https://github.com/Ventto/mons
;; https://sourceforge.net/projects/narocad/
;; http://solvespace.com/index.pl
;; https://github.com/lpereira/hardinfo
;; https://github.com/matrix-org/pantalaimon to go with emacs-matrix-client
;; https://github.com/poljar/weechat-matrix to go with emacs-weechat

;; WIP
(define-public dual-function-keys
  (package
   (name "dual-function-keys")
   (version "1.4.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference (url "https://gitlab.com/interception/linux/plugins/dual-function-keys.git")
                         (commit version)))
     (sha256 (base32 "0ch6dvf172g29mj5rsa07wynkwmgkzl3ilbm0j0pvg0qa7namjk7"))
     (file-name (git-file-name name version))))
   (build-system cmake-build-system)
   (inputs
    `(("boost" ,boost)))
   (propagated-inputs
    `(("interception-tools" ,interception-tools)))
   (home-page "https://gitlab.com/interception/linux/plugins/dual-function-keys")
   (synopsis "")
   (description synopsis)
   (license license:expat)))

(define-public interception-tools
  (package
   (name "interception-tools")
   (version "0.6.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference (url "https://gitlab.com/interception/linux/tools")
                         (commit "0d9223e81333a8e1667eab96d1b7378d6578d214")))
     (sha256 (base32 "067c1f2iw6bdsy725js976vyllq9s63wh0x3y27kgh4vwlv8niqw"))
     (file-name (git-file-name name version))))
   (build-system cmake-build-system)
   (inputs
    `(("boost" ,boost)
      ("libevdev" ,libevdev)
      ("eudev" ,eudev)
      ("yaml-cpp" ,yaml-cpp)
      ("glibc" ,glibc)))
   (propagated-inputs
    `(("libevdev" ,libevdev)
      ("eudev" ,eudev)
      ("yaml-cpp" ,yaml-cpp)
      ("glibc" ,glibc)))
   (home-page "https://gitlab.com/interception/linux/tools")
   (synopsis "")
   (description synopsis)
   (license license:gpl3)))

(define-public svtplay-dl
  (let ((commit "81cb18f"))
    (package
     (name "svtplay-dl")
     (version (git-version "2.4" "0" commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/spaam/svtplay-dl.git")
                           (commit commit)))
       (sha256 (base32 "03l6r5rzjljrj81icqlgndfzvp9hbm04m56qi3aphlwym6diiab7"))
       (file-name (git-file-name name version))))
     (build-system python-build-system)
     (inputs `(("python-cryptography" ,python-cryptography)
               ("python-pyyaml" ,python-pyyaml)
               ("python-requests" ,python-requests)
               ("python-pysocks" ,python-pysocks)
               ("python-dateutil" ,python-dateutil)
               ("ffmpeg" ,ffmpeg)))
     (home-page "https://svtplay-dl.se/")
     (synopsis "Similar to youtube-dl, for a number of Scandinavian websites.")
     (description synopsis)
     (license license:expat))))

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
   (home-page "https://github.com/zevv/duc")
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
simply as an indexed du, like duc.")
     (license license:expat))))

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
   ;; The Sugarcube format comes from the same developer, natural to bundle it.
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
       "A collection of macros and stylesheets necessary to compile SugarCube2 stories
with Twine/Tweego. A Guix-installed Tweego should find it automatically, but to
add it to Twine, click your way to the file in ~/.guix-profile/" story-formats-dir ",
or equivalent location if your Guix profile is elsewhere."))
     (license license:bsd-2))))
