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
  :use-module (gnu packages linux)
  :use-module (nongnu packages linux)
  :use-module (gnu packages vpn)
  :use-module (srfi srfi-1)) ;; alist-delete

(define kern-build-config "/home/kept/Guix channel/mee/kconf-merged-5.4.config")

(define-public linux-with-wireguard
  (package
   (inherit linux-5.4)
   (name "linux-with-wireguard")
   (source
    (origin
     (inherit (package-source linux-5.4))
     ;; dirty path to hardcode
     (patches (cons (local-file "/home/me/.guix-profile/wireguard.patch")
                    (origin-patches (package-source linux-5.4))))))
   (native-inputs
    `(("kconfig" ,kern-build-config)
      ,@(alist-delete "kconfig"
                      (package-native-inputs linux-5.4))))))
