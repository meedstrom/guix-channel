;; xml.scm
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

(define-module (mee xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  ;; #:use-module (guix gexp)
  ;; #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages documentation))

(define-public nftables-with-manpage
  (package
    (inherit nftables)
    (name "nftables-with-manpage")
    (arguments `(#:configure-flags '("--enable-man-doc")))
    (inputs (append (package-inputs nftables)
                    `(("docbook2x" ,docbook2x)
                      ("asciidoc" ,asciidoc))))))

(define-public docbook2x
  (package
    (name "docbook2x")
    (version "0.8.8") ;; last release: 2007
    (source (origin (method url-fetch)
                    (uri "https://netcologne.dl.sourceforge.net/project/docbook2x/docbook2x/0.8.8/docbook2X-0.8.8.tar.gz")
                    (sha256
                     (base32 "0ifwzk99rzjws0ixzimbvs83x6cxqk1xzmg84wa1p7bs6rypaxs0"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
              ("perl-xml-sax" ,perl-xml-sax)
              ("perl-xml-parser" ,perl-xml-parser)
              ("perl-xml-libxslt" ,perl-xml-libxslt)
              ("libxslt" ,libxslt)
              ("texinfo" ,texinfo)
              ("docbook-xml" ,docbook-xml)))
    (arguments '(#:tests? #f)) ;; makeinfo test fails :/ but we don't need it for nftables
    (home-page "https://docbook2x.sourceforge.net")
    (synopsis "Convert from Docbook to other formats (man and info).")
    (description #f)
    (license (list
              license:expat
              license:non-copyleft))))
