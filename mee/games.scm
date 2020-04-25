;; games.scm
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


(define-module (mee games)
  #:use-module (games build-system mojo)
  #:use-module (games gog-download)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (nonguix licenses))

(define-public gog-sunless-sea
  (let ((buildno "29003")
        (binary (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux" "Sunless Sea.x86_64")
                  ("i686-linux" "Sunless Sea.x86")
                  (_ "")))
        (arch (match (or (%current-target-system)
                         (%current-system))
                ("x86_64-linux" "x86_64")
                ("i686-linux" "x86")
                (_ ""))))
    (package
      (name "gog-sunless-sea")
      (version "2.2.7.3165")
      ;; (source (local-file "/home/Downloads/sunless_sea_2_2_7_3165_29003.sh"))
      (source
       (origin
        (method gog-fetch)
        (uri "gogdownloader://sunless_skies/en3installer0")
        (file-name (string-append "sunless_skies_"
                                  (string-replace-substring version "." "_")
                                  "_" buildno ".sh"))
        (sha256
         (base32
          "1j6mdjy4x6xra2bx1f6iz7ywhg9cz340kfvlihcnmll66jxqrrk7"))))
      (supported-systems '("x86_64-linux"))
      (build-system mojo-build-system)
      (arguments
       `(#:patchelf-plan
         `((,,binary
            ("libc" "gcc:lib" "gdk-pixbuf" "glib" "gtk+-2" "libx11" "libxcursor"
             "libxext" "libxi" "libxinerama" "libxrandr" "libxscrnsaver"
             "libxxf86vm" "mesa" "pulseaudio" "zlib")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'link-libraries
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; libudev is not loaded from RUNPATH, so link it in the expected
               ;; directory.
               (let* ((out (assoc-ref outputs "out"))
                      (mono (string-append (car (find-files out "Sunless Skies_Data" #:directories? #t))
                                           "/Mono/" ,arch))
                      (udev (assoc-ref inputs "eudev")))
                 (symlink
                   (string-append udev "/lib/libudev.so.1")
                   (string-append mono "/libudev.so.0"))
                 (symlink
                   (string-append udev "/lib/libudev.so")
                   (string-append mono "/libudev.so")))
               #t)))))
      (inputs
       `(("eudev" ,eudev)
         ("gcc:lib" ,gcc "lib")
         ("gdk-pixbuf" ,gdk-pixbuf)
         ("glib" ,glib)
         ("gtk+-2" ,gtk+-2)
         ("libx11" ,libx11)
         ("libxcursor" ,libxcursor)
         ("libxext" ,libxext)
         ("libxi" ,libxi)
         ("libxinerama" ,libxinerama)
         ("libxrandr" ,libxrandr)
         ("libxscrnsaver" ,libxscrnsaver)
         ("libxxf86vm" ,libxxf86vm)
         ("mesa" ,mesa)
         ("pulseaudio" ,pulseaudio)
         ("zlib" ,zlib)))
      (home-page "https://www.failbettergames.com/sunless-skies")
      (synopsis "Gothic horror role-playing game")
      (description "Sail the stars.  Betray your queen.  Murder a sun.  Sunless
Skies is a Gothic Horror roleplay game with a focus on exploration and exquisite
storytelling.")
      (license (undistributable
                (string-append "file://data/noarch/docs/"
                               "End User License Agreement.txt"))))))
