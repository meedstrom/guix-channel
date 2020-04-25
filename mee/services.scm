;; services.scm
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
(define-module (mee services)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages vpn)
  #:use-module (guix gexp))
;; (define (wireguard-shepherd-service config)
;;   (list (shepherd-service
;;          (documentation "Set up the Wireguard interface")
;;          (provision '(wireguard))
;;          (requirement '(networking))
;;          (start #~(lambda _
;;                     (let ((wg (string-append #$wireguard "/bin/wg-quick")))
;;                       (system* wg "up" "/root/mullvadse7se8.conf"))))
;;          (stop #~(lambda _
;;                    (let ((wg (string-append #$wireguard "/bin/wg-quick")))
;;                      (system* wg "down" "/root/mullvadse7se8.conf")))))))

;; (define wireguard-service-type
;;   (service-type (name 'wireguard)
;;                 (extensions
;;                  (list
;;                   (service-extension shepherd-root-service-type
;;                                      wireguard-shepherd-service)))
;;                 (description "")
;;                 (default-value '())))

;; https://lists.gnu.org/archive/html/help-guix/2018-11/msg00216.html
;; (define (hello-shepherd-service config)
;;           (list (shepherd-service
;;                  (provision '(hello))
;;                  (requirement '())
;;                  (start #~(make-forkexec-constructor
;;                            (list "touch" "/tmp/hello")))
;;                  (one-shot? #t)
;;                  (auto-start? #f)
;;                  (respawn? #f))))

;;         (define hello-service-type
;;           (service-type
;;            (name 'hello)
;;            (extensions
;;             (list (service-extension shepherd-root-service-type
;;                                      hello-shepherd-service)))
;;            (default-value '())))


(define %wireguard-service
  (simple-service 'wireguard network-manager-service-type
		  #~(zero? (system* #$(file-append wireguard "/bin/wg-quick")
                                    "up"
				    "/root/mullvadse7se8.conf"))))

(define %macchanger-service
  (simple-service 'macchanger activation-service-type
		  #~(zero? (system* #$(file-append macchanger "/bin/macchanger")
                                    "-a"
                                    "wlp2s0"))))
