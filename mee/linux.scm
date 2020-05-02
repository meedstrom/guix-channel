;; linux.scm
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

(define-module (mee linux)
  :use-module (guix packages)
  :use-module (guix gexp) ;; file-append
  ;; :use-module (guix utils) ;; substitute-keyword-arguments
  :use-module (gnu packages linux)
  :use-module (nongnu packages linux)
  :use-module (gnu packages vpn)
  ;; :use-module (gnu)
  :use-module (srfi srfi-1)) ;; alist-delete

(define kern-build-config "/home/kept/Guix channel/mee/kconf-merged-5.4.config")
;; (define kern-build-config "kconf-merged-5.4.config")

(define-public linux-meedstrom
  (package
   (inherit linux-libre-5.4)
   (name "linux-meedstrom")
   (source
    (origin
     (inherit (package-source linux-libre-5.4))
     (patches (append (list (local-file "/home/me/.guix-profile/wireguard.patch"))
                      (origin-patches (package-source linux-libre-5.4))))))
   (native-inputs
    `(("kconfig" ,kern-build-config)
      ,@(alist-delete "kconfig"
                      (package-native-inputs linux-libre-5.4))))))

;; There is a wireguard:kernel-patch, but I cannot refer to it because
;; 1. file-append does not handle outputs
;; 2. using #~(string-append) gets me "wrong type argument: gexp, expected string"
;; So here's a new package.
;; (define wireguard-patch
;;   (package
;;    (inherit wireguard)
;;    (name "wireguard-patch")
;;    (outputs '("out"))
;;    ;; Source expression failed to match any pattern
;;    (arguments
;;     (substitute-keyword-arguments (package-arguments wireguard)
;;      (#:phases
;;       ;; Copied from gnu/packages/vpn with minor change
;;       (modify-phases
;;        %standard-phases
;;        (delete 'configure)
;;        (add-after 'patch-source-shebangs 'make-patch
;;                   (lambda* (#:key outputs #:allow-other-keys)
;;                     (let* ((output (string-append (assoc-ref outputs "out")
;;                                                   "/wireguard.patch"))
;;                            (patch-builder "./contrib/kernel-tree/create-patch.sh")
;;                            (port (open-input-pipe patch-builder))
;;                            (str (get-string-all port)))
;;                       (close-pipe port)
;;                       (mkdir-p (dirname output))
;;                       (call-with-output-file output
;;                         (lambda (port)
;;                           (format port "~a" str))))
;;                     #t))))))))

;; Can someone on #guix please debug this
;; (define-public linux-meedstrom-b
;;   (package
;;    (inherit linux-libre)
;;    (name "linux-meedstrom-b")
;;    (source
;;     (origin
;;      (inherit (package-source linux-libre))
;;      (patches (append (list (file-append wireguard-linux-compat "/wireguard.patch"))
;;                       (origin-patches (package-source linux-libre))))))
;;    (native-inputs
;;     `(("kconfig" ,kern-build-config)
;;       ,@(alist-delete "kconfig"
;;                       (package-native-inputs linux-libre))))))

;; (define-public linux-meedstrom-c
;;   (package
;;    (inherit linux-libre-5.4)
;;    (name "linux-meedstrom-b")
;;    (source
;;     (origin
;;      (inherit (package-source linux-libre-5.4))
;;      (patches (cons (file-append wireguard-linux-compat "/wireguard.patch")
;;                       (origin-patches (package-source linux-libre-5.4))))))
;;    (native-inputs
;;     `(("kconfig" ,kern-build-config)
;;       ,@(alist-delete "kconfig"
;;                       (package-native-inputs linux-libre-5.4))))))

;; (define-public linux-meedstrom
;;   (package
;;    (inherit linux-libre)
;;    (name "linux-meedstrom")
;;    (source
;;     (origin
;;      (inherit (package-source linux-libre))
;;      (patches (cons (file-append
;;                      (specification->package+output "wireguard:kernel-patch")
;;                      "wireguard.patch")
;;                     (origin-patches (package-source linux-libre))))))
;;    (native-inputs
;;     `(("kconfig" ,kern-build-config)
;;       ,@(alist-delete "kconfig"
;;                       (package-native-inputs linux-libre))))))

(define-public linux-nonfree
  (package
   (inherit linux-5.4)
   (name "linux-nonfree")
   (source
    (origin
     (inherit (package-source linux-5.4))
     (patches (cons (local-file "/home/me/.guix-profile/wireguard.patch")
                    (origin-patches (package-source linux-5.4))))))
   (native-inputs
    `(("kconfig" ,kern-build-config)
      ,@(alist-delete "kconfig"
                      (package-native-inputs linux-5.4))))))
