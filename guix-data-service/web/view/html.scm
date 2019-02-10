;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (guix-data-service web view html)
  #:use-module (guix-data-service config)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (index
            compare
            compare-unknown-commit
            error-page))

(define* (header)
  `(nav
    (@ (id "header") (class "navbar navbar-default"))
    (div
     (@ (class "container-fluid"))
     (div
      (@ (class "navbar-header"))
      (div (@ (class "navbar-brand"))
           (a (@ (href "/") (class "logo"))))))))

(define* (layout #:key
                 (head '())
                 (body '())
                 (title "Guix Data Service")
                 (extra-headers '()))
  `(#:sxml ((doctype "html")
            (html
             (head
              (title ,title)
              (meta (@ (http-equiv "Content-Type")
                       (content "text/html; charset=UTF-8")))
              (meta (@ (http-equiv "Content-Language") (content "en")))
              (meta (@ (name "author") (content "Christopher Baines")))
              (meta (@ (name "viewport")
                       (content "width=device-width, initial-scale=1")))
              (link
               (@ (rel "stylesheet")
                  (media "screen")
                  (type "text/css")
                  (href "/css/reset.css")))
              (link
               (@ (rel "stylesheet")
                  (media "screen")
                  (type "text/css")
                  (href "/css/bootstrap.css")))
              ,@head
              (link
               (@ (rel "stylesheet")
                  (media "screen")
                  (type "text/css")
                  (href "/css/screen.css"))))
             (body ,@body
                   (footer
                    (p "Copyright © 2016—2019 by the GNU Guix community."
                       (br)
                       "Now with even more " (span (@ (class "lambda")) "λ") "! ")
                    (p "This is free software.  Download the "
                       (a (@ (href "https://git.cbaines.net/guix/data-service/"))
                          "source code here") ".")))))
    #:extra-headers ,extra-headers))

(define (index guix-revisions)
  (layout
   #:extra-headers
   '((cache-control . ((max-age . 60))))
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (h1 "Guix Data Service")
      (form (@ (id "compare")
               (action "/compare"))
            (div
             (@ (class "form-group"))
             (label (@ (for "base_commit"))
                    "Base commit")
             (input (@ (type "text")
                       (class "form-control")
                       (id   "base_commit")
                       (name "base_commit")
                       (placeholder "base commit"))))
            (div
             (@ (class "form-group"))
             (label (@ (for "target_commit"))
                    "Target commit")
             (input (@ (type "text")
                       (class "form-control")
                       (id   "target_commit")
                       (name "target_commit")
                       (placeholder "target commit"))))
            (button
             (@ (type "submit")
                (class "btn btn-lg btn-primary"))
             "Compare"))
      (h3 "Recent fetched revisions")
      ,(if (null? guix-revisions)
           '(p "No revisions")
           `(table
             (@ (class "table"))
             (thead
              (tr
               (th (@ (class "col-md-6")) "Source Repository URL")
               (th (@ (class "col-md-6")) "Commit")))
             (tbody
              ,@(map
                 (match-lambda
                   ((id url commit store_path)
                    `(tr
                      (td ,url)
                      (td (samp ,commit)))))
                 guix-revisions))))))))

(define (compare base-commit
                 target-commit
                 new-packages
                 removed-packages
                 version-changes
                 other-changes)
  (layout
   #:extra-headers
   '((cache-control . ((max-age . 60))))
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (h1 "Comparing "
          (samp ,(string-take base-commit 8) "…")
          " and "
          (samp ,(string-take target-commit 8) "…"))
      (h3 "New packages")
      ,(if (null? new-packages)
           '(p "No new packages")
           `(table
             (@ (class "table"))
             (thead
              (tr
               (th (@ (class "col-md-3")) "Name")
               (th (@ (class "col-md-9")) "Version")))
             (tbody
              ,@(map
                 (match-lambda
                   ((name . version)
                    `(tr
                      (td ,name)
                      (td ,version))))
                 new-packages))))
      (h3 "Removed packages")
      ,(if (null? removed-packages)
           '(p "No removed packages")
           `(table
             (@ (class "table"))
             (thead
              (tr
               (th (@ (class "col-md-3")) "Name")
               (th (@ (class "col-md-9")) "Version")))
             (tbody
              ,@(map
                 (match-lambda
                   ((name . version)
                    `(tr
                      (td ,name)
                      (td ,version))))
                 removed-packages))))
      (h3 "Version changes")
      ,(if (null? version-changes)
           '(p "No version changes")
           `(table
             (@ (class "table"))
             (thead
              (tr
               (th (@ (class "col-md-3")) "Name")
               (th (@ (class "col-md-9")) "Versions")))
             (tbody
              ,@(map
                 (match-lambda
                   ((name . versions)
                    `(tr
                      (td ,name)
                      (td (ul
                           ,@(map (match-lambda
                                    ((type . version)
                                     `(li (@ (class ,(if (eq? type 'base)
                                                         "text-danger"
                                                         "text-success")))
                                          ,version
                                          ,(if (eq? type 'base)
                                               " (old)"
                                               " (new)"))))
                                  versions))))))
                 version-changes))))
      (h3 "Other changed packages")
      ,@(if (null? other-changes)
           '((p "No other changes"))
           `((p "The metadata or derivation for these packages has changed.")
             (table
              (@ (class "table"))
              (thead
               (tr
                (th (@ (class "col-md-3")) "Name")
                (th (@ (class "col-md-9")) "Version")))
              (tbody
               ,@(map
                  (match-lambda
                    (((name . version) . (metadata-id derivation-id))
                     `(tr
                       (td ,name)
                       (td ,version))))
                  other-changes)))))))))

(define (compare-unknown-commit commit)
  (layout
   #:body
   `(,(header)
     (div (@ (class "container"))
          (h1 "Unknown commit")
          (p "No known revision with commit  " (strong (samp ,commit)))))))

(define (error-page message)
  (layout
   #:body
   `(,(header)
     (div (@ (class "container"))
          (h1 "Error")
          (p "An error occurred.  Sorry about that!")
          ,message
          (p (a (@ (href "/")) "Try something else?"))))))