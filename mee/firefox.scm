;;; firefox.scm - NONFSDG Firefox package
;;; Copyright (C) 2017, 2018 ng0 <gillmann@infotropique.org>
;;; Copyright (C) 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Parts of this file originated from the (gnu packages gnuzilla) module of
;; GNU Guix which is GPL3 licensed.

(define-module (mee firefox)
  #:use-module (warrah-nonfsdg packages firefox)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages rust)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module ((srfi srfi-26))
  #:use-module (ice-9 match)
  #:use-module (guix utils)
  #:use-module (guix gexp) ;local-file
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages sqlite)  
  #:use-module (gnu packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages node)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages rust-apps))

;; patches location is relative to the top directory, if you see what i mean
;; alternatively (patches (list (local-file "patches/foo.patch")))

(define %iceweasel-version "72.0.2")

(define nspr4.23
  (package
    (inherit nspr)  
    (version "4.23")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "193p6i2r6wvpb4i04a9pxbqkxcn8rbcmwl81m4yp5xgs6w8857ab"))))))

(define nspr4.24
  (package
    (inherit nspr)  
    (version "4.24")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "1l0ksiny032jijgk0g76wf0kiq673i01izj7jrs2h5d1yq6rm9ch"))))))

(define nss3.48
  (package
    (inherit nss)  
    (version "3.48")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-join (string-split version #\.) "_")))
                     (string-append
                      "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                      "releases/NSS_" version-with-underscores "_RTM/src/"
                      "nss-" version ".tar.gz")))
              (sha256
               (base32
                "1b7qs1q7jqhw9dvkdznanzhc5dyq4bwx0biywszy3qx4hqm8571z"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-pkgconfig.patch"
                                       "nss-freebl-stubs.patch"
                                       "nss-increase-test-timeout.patch"))))
    (propagated-inputs `(("nspr" ,nspr4.24)))))

(define nss3.47
  (package
    (inherit nss)  
    (version "3.47.1")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-join (string-split version #\.) "_")))
                     (string-append
                      "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                      "releases/NSS_" version-with-underscores "_RTM/src/"
                      "nss-" version ".tar.gz")))
              (sha256
               (base32
                "1yx68a1ks63s8914zmiz9h1nh0261aqzcblgg1cb4ig33p5x3qqs"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-pkgconfig.patch"
                                       "nss-freebl-stubs.patch"
                                       "nss-increase-test-timeout.patch"))))))

(define sqlite3.30.1
  (package
   (inherit sqlite)  
   (version "3.30.1")
   (source (origin
            (method url-fetch)
            (uri (let ((numeric-version
                        (match (string-split version #\.)
                          ((first-digit other-digits ...)
                           (string-append first-digit
                                          (string-pad-right
                                           (string-concatenate
                                            (map (cut string-pad <> 2 #\0)
                                                 other-digits))
                                           6 #\0))))))
                   (string-append "https://sqlite.org/2019/sqlite-autoconf-"
                                  numeric-version ".tar.gz")))
            (sha256
             (base32
              "0q4f57a5995wz9c7dfiqy9zwl0kn0b900nxwinqa3llv13dm0nlc"))))))

(define-public firefox
  (package 
   (name "firefox")
   (version "75.0b11")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://archive.mozilla.org/pub/"
                         "firefox/releases/"
                         version "/source/firefox-" version
                         ".source.tar.xz"))
     (sha256
      (base32
       "0x0dgqaiq4jr1m8c6rx4lj7bs4ih8wsxsf1wy76z2gx0ckhr952v"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f          ; no check target
      ;;#:out-of-source? #t  ; must be built outside of the source directory

      ;; XXX: There are RUNPATH issues such as
      ;; $prefix/lib/icecat-31.6.0/plugin-container NEEDing libmozalloc.so,
      ;; which is not in its RUNPATH, but they appear to be harmless in
      ;; practice somehow.  See <http://hydra.gnu.org/build/378133>.
      #:validate-runpath? #f

      ;;#:parallel-build? #f

      #:imported-modules ,%cargo-utils-modules ;for `generate-checksums'

      #:modules ((ice-9 ftw)
                 (ice-9 rdelim)
                 (ice-9 match)
                 ,@%gnu-build-system-modules)
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'ensure-no-mtimes-pre-1980
                                (lambda _
                                  ;; Without this, the 'source/test/addons/packed.xpi' and
                                  ;; 'source/test/addons/simple-prefs.xpi' targets fail while trying
                                  ;; to create zip archives.
                                  (let ((early-1980 315619200)) ; 1980-01-02 UTC
                                    (ftw "." (lambda (file stat flag)
                                               (unless (<= early-1980 (stat:mtime stat))
                                                 (utime file early-1980 early-1980))
                                               #t))
                                    #t)))         
                     
                                        ; see https://bugzilla.mozilla.org/show_bug.cgi?id=1490673
                                        ;(add-before 'configure 'patch-SymbolTable_autogen
                                        ;   (lambda _
                                        ;     (substitute* "gfx/angle/checkout/src/compiler/translator/SymbolTable_autogen.cpp"
                                        ;       (("#include \"compiler/translator/SymbolTable.h\"") "#include <cmath>\n#include \"compiler/translator/SymbolTable.h\""))
                                        ;     #t))
                     
                     (add-before 'configure 'augment-CPLUS_INCLUDE_PATH
                                 (lambda* (#:key build inputs #:allow-other-keys)
                                   ;; Here, we add additional entries to CPLUS_INCLUDE_PATH, to work
                                   ;; around a problem that otherwise occurs when attempting to
                                   ;; build Stylo, which requires Rust and Clang.  Without these
                                   ;; additional entries, errors occur during the build indicating
                                   ;; that the <cstddef> and "c++config.h" headers cannot be found.
                                   ;; Note that the 'build' keyword argument contains the GNU
                                   ;; triplet, e.g. "x86_64-unknown-linux-gnu".
                                   (let ((gcc (assoc-ref inputs "gcc")))
                                     (setenv "CPLUS_INCLUDE_PATH"
                                             (string-append gcc "/include/c++" ":"
                                                            gcc "/include/c++/" build)))
                                   #t))
                     
                     (add-after 'patch-source-shebangs 'patch-cargo-checksums
                                (lambda _
                                  (use-modules (guix build cargo-utils))
                                  (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                                    (substitute* '("Cargo.lock" "gfx/wr/Cargo.lock")
                                                 (("(\"checksum .* = )\".*\"" all name)
                                                  (string-append name "\"" null-hash "\"")))
                                    (for-each
                                     (lambda (filename)
                                       (delete-file filename)
                                       (let ((dir (dirname filename)))
                                         (display (string-append
                                                   "patch-cargo-checksums: generate-checksums for "
                                                   dir "\n"))
                                         (generate-checksums dir)))
                                     (find-files "third_party/rust" ".cargo-checksum.json")))
                                  #t))
                     
                                        ; Fixes issue where each installation directory generates its own profile.
                                        ; See e.g. https://trac.torproject.org/projects/tor/ticket/31457
                     (add-after 'patch-source-shebangs 'fix-profile-setting
                                (lambda _
                                  (let ((mozconfigure "browser/moz.configure"))
                                    (substitute* mozconfigure
                                                 (("'MOZ_DEDICATED_PROFILES', True") "'MOZ_DEDICATED_PROFILES', False")))
                                  ))
                     
                     (replace 'configure
                              (lambda* (#:key outputs configure-flags #:allow-other-keys)
                                (let* ((out (assoc-ref outputs "out"))
                                       (bash (which "bash"))
                                       (abs-srcdir (getcwd))
                                       (srcdir (string-append "../" (basename abs-srcdir)))
                                       (flags `(,(string-append "--prefix=" out)
                                                ,@configure-flags))
                                       (mozconfig (string-append (getcwd) "/.mozconfig")))
                                  (setenv "SHELL" bash)
                                  (setenv "AUTOCONF"
                                          (string-append (assoc-ref %build-inputs
                                                                    "autoconf")
                                                         "/bin/autoconf"))
                                  (setenv "CONFIG_SHELL" bash)
                                  (setenv "QA_CONFIGURE_OPTIONS" ".*")
                                  (setenv "MOZBUILD_STATE_PATH"
                                          (string-append (getcwd) "/mach_state"))
                                  (setenv "MOZCONFIG"
                                          (string-append (getcwd) "/.mozconfig"))
                                  (setenv "CC" "gcc")                       
                                  (setenv "MOZ_NOSPAM" "1")
                                  (setenv "PYTHON"
                                          (string-append (assoc-ref %build-inputs
                                                                    "python2")
                                                         "/bin/python"))
                                  ;; (setenv "builddir" (string-append (getcwd) "/build"))
                                  ;; (mkdir-p (getenv "MOZBUILD_STATE_PATH"))
                                  (mkdir-p (string-append (getcwd) "/builddir"))
                                  (with-output-to-file mozconfig
                                    (lambda ()
                                      (display
                                       (string-append
                                        "ac_add_options --enable-application=browser\n"
                                        "ac_add_options --prefix=" out "\n"
                                        "ac_add_options --enable-pulseaudio\n"
                                        "ac_add_options --with-system-jpeg\n"
                                        "ac_add_options --with-system-zlib\n"
                                        "ac_add_options --with-system-bz2\n"
                                        "ac_add_options --with-system-nspr\n"
                                        ;"ac_add_options --with-system-nss\n" ; see https://bugs.gnu.org/32833
                                        "ac_add_options --with-system-libevent\n"
                                        "ac_add_options --with-system-icu\n"
                                        "ac_add_options --enable-system-ffi\n"
                                        "ac_add_options --enable-system-pixman\n"
                                        ;; "ac_add_options --enable-system-sqlite\n" ;; not in firefox 75 (renamed?)
                                        "ac_add_options --enable-startup-notification\n"
                                        "ac_add_options --disable-crashreporter\n"
                                        "ac_add_options --disable-tests\n"
                                        "ac_add_options --disable-necko-wifi\n"
                                        "ac_add_options --disable-updater\n"
                                        "ac_add_options --disable-gconf\n"
                                        "ac_add_options --enable-default-toolkit=\"cairo-gtk3\"\n"
                                        "ac_add_options --disable-debug\n"
                                        "ac_add_options --disable-debug-symbols\n"
                                        "ac_add_options --enable-optimize\n"
                                        "ac_add_options --enable-release\n"
                                        "ac_add_options --enable-strip\n"
                                        "ac_add_options --with-distribution-id=org.gnu\n"
                                        "ac_add_options --disable-elf-hack\n"
                                        "ac_add_options --disable-ion\n"
                                        "ac_add_options --disable-webrtc\n"
                                        "ac_add_options --disable-official-branding\n"
                                        "ac_add_options --with-clang-path=" (assoc-ref %build-inputs "clang") "/bin/clang\n"
                                        "ac_add_options --with-libclang-path=" (assoc-ref %build-inputs "clang") "/lib\n"
                                        "ac_add_options --with-user-appdir=\\.firefox\n"))))
                                  (newline)
                                  (display "mach configure")
                                  (delete-file-recursively "obj-x86_64-pc-linux-gnu")
                                  (zero? (system* "./mach" "configure")))
                                #t))
                     
                     (replace 'build
                              (lambda _
                                (zero? (system* "./mach" "build"))))
                     
                     (replace 'install
                              (lambda _
                                (zero? (system* "./mach" "install"))))
                     
                     (add-after 'install 'wrap-program
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (lib (string-append out "/lib"))
                                         (gtk (assoc-ref inputs "gtk+"))
                                         (gtk-share (string-append gtk "/share"))
                                         (pulseaudio (assoc-ref inputs "pulseaudio"))
                                         (pulseaudio-lib (string-append pulseaudio "/lib")))
                                    (wrap-program (car (find-files lib "^firefox$"))
                                                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                                                  `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib)))
                                    #t)))
                     
                     )))
   (inputs
    `(("alsa-lib" ,alsa-lib)
      ("bzip2" ,bzip2)
      ("cairo" ,cairo)
      ("cups" ,cups)
      ("dbus-glib" ,dbus-glib)
      ("gdk-pixbuf" ,gdk-pixbuf)
      ("glib" ,glib)
      ("gtk+" ,gtk+)
      ("gtk+-2" ,gtk+-2)
      ("pango" ,pango)
      ("freetype" ,freetype)
      ("hunspell" ,hunspell)
      ("libcanberra" ,libcanberra)
      ("libgnome" ,libgnome)
      ("libjpeg-turbo" ,libjpeg-turbo)
      ("libpng-apng" ,libpng-apng)
      ("libxft" ,libxft)
      ("libevent" ,libevent)
      ("libxinerama" ,libxinerama)
      ("libxscrnsaver" ,libxscrnsaver)
      ("libxcomposite" ,libxcomposite)
      ("libxt" ,libxt)
      ("libffi" ,libffi)
      ("ffmpeg" ,ffmpeg)
      ("libvpx" ,libvpx)
      ("icu4c" ,icu4c)
      ("pixman" ,pixman)
      ("pulseaudio" ,pulseaudio)
      ("mesa" ,mesa)
      ("mit-krb5" ,mit-krb5)
      ("nspr" ,nspr4.24)
      ("nss" ,nss)
      ("sqlite" ,sqlite3.30.1)
      ("startup-notification" ,startup-notification)
      ("unzip" ,unzip)
      ("zip" ,zip)
      ("zlib" ,zlib)
      ))
   (native-inputs
    `(("perl" ,perl)
      ("python" ,python)
      ("python2" ,python-2.7)
      ("python2-pysqlite" ,python2-pysqlite)
      ("yasm" ,yasm)
      ("nasm" ,nasm)
      ("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf-2.13)
      ("which" ,which)
      ("rust" ,rust)
      ("cargo" ,rust "cargo")
      ("llvm" ,llvm)
      ("clang" ,clang)
      ("rust-cbindgen" ,rust-cbindgen)
      ("node" ,node)
      ))
   (home-page "https://mozilla.org/firefox/")
   (synopsis "Trademarkless version of Firefox")
   (description
    "Full-featured browser client built from Firefox source tree.")
   (license license:mpl2.0))) ;and others, see toolkit/content/license.html

firefox
