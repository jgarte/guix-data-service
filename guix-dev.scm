;;; guix-data-service -- Information about Guix over time
;;; Copyright © 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;;
;;; This file is part of guix-data-service.
;;;
;;; guix-data-service is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; guix-data-service is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with the guix-data-service.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Run the following command to enter a development environment for
;;; the guix-data-service:
;;;
;;;  $ guix environment -l guix-dev.scm

(use-modules ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix utils)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages databases)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages package-management)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages ruby)
             (srfi srfi-1))

(define guile3.0-email
  (package
    (inherit guile-email)
    (name "guile3.0-email")
    (inputs `(("guile" ,guile-3.0)
              ,@(alist-delete "guile" (package-inputs guile-email))))))

(define guile3.0-squee
  (package
   (inherit guile-squee)
   (name "guile3.0-squee")
   (native-inputs `(("guile" ,guile-3.0)
                    ,@(alist-delete "guile" (package-native-inputs guile-squee))))))

(define guile3.0-readline
  (if (defined? 'guile3.0-readline
        (resolve-interface '(gnu packages guile)))
      guile3.0-readline
      guile-readline))

(package
  (name "guix-data-service")
  (version "0.0.0")
  (source #f)
  (build-system gnu-build-system)
  (inputs
   `(("guix" ,guile3.0-guix)
     ("guile-email" ,guile3.0-email)
     ("guile-json" ,guile3.0-json)
     ("guile-squee" ,guile3.0-squee)
     ("guile-fibers" ,guile3.0-fibers)
     ("guile-gcrypt" ,guile3.0-gcrypt)
     ("guile-readline" ,guile3.0-readline)
     ("guile" ,guile-next)
     ("sqitch" ,sqitch)))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("ephemeralpg" ,ephemeralpg)
     ("pkg-config" ,pkg-config)
     ("ruby-rerun" ,ruby-rerun)))
  (synopsis "TODO")
  (description "TODO")
  (home-page "TODO")
  (license license:gpl3+))
