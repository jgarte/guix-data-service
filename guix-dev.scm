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

(package
  (name "guix-data-service")
  (version "0.0.0")
  (source #f)
  (build-system gnu-build-system)
  (inputs
   `(("guix" ,guix)
     ("guile-email" ,guile-email)
     ("guile-json" ,guile-json-4)
     ("guile-squee" ,guile-squee)
     ("guile-fibers" ,guile-fibers)
     ("guile-gcrypt" ,guile-gcrypt)
     ("guile-lzlib" ,guile-lzlib)
     ("guile-readline" ,guile-readline)
     ("guile-prometheus" ,guile-prometheus)
     ("guile" ,guile-3.0-latest)
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
