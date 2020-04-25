;; irc.scm
;; Copyright (C) 2019 Martin Edström

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

(define-module (mee irc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  )

(define-public olm
  (package
   (name "olm")
   (version "3.1.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://gitlab.matrix.org/matrix-org/olm.git")
                                (commit version)))
            (file-name (git-file-name name version))
            (sha256 #f)))
   (build-system cmake-build-system)
   (home-page "")
   (synopsis "Implementations of Olm and Megolm cryptographic ratchets.")
   (description
    "An implementation of the Double Ratchet Algorithm first popularised by
Signal, but extended to support encryption to rooms containing thousands of
devices.")
   (license license:asl2.0)))

(define-public weechat-matrix
  (package
   (name "weechat-matrix")
   (version "master20191222")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/poljar/weechat-matrix.git")
                                (commit "c1b8fe9")))
            (sha256 (base32 "15n9wha8pzawy6d5lj7z2892fvddms3g4nzf4mrk0rbvl8ss0bjj"))))
   (build-system gnu-build-system)
   ;; FIXME: Figure out what to do to make this package have an effect. From
   ;; the README: make install installs the main python file (main.py) into
   ;; ~/.weechat/python/ (renamed to matrix.py) along with the other python
   ;; files it needs (from the matrix subdir)./
   (inputs
    `(("olm" ,olm)))
   (propagated-inputs
    `(;; python-logbook
      ;; python-matrix-nio[e2e]>=0.6
      ;; python-webcolors
      ("python-pyopenssl" ,python-pyopenssl)
      ("python-typing" ,python-typing) ; python_version < "3.5"
      ("python-future" ,python-future)
      ("python-atomicwrites" ,python-atomicwrites)
      ("python-attrs" ,python-attrs)
      ("python-pygments" ,python-pygments)
      ("python-aiohttp" ,python-aiohttp) ; python_version >= "3.5"
      ("python-magic" ,python-magic)
      ("python-requests" ,python-requests)))
   (home-page "")
   (synopsis "")
   (description "")
   (license #f)))
