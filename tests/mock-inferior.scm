(define-module (tests mock-inferior)
  #:use-module (guix records)
  #:use-module (guix tests)
  #:export (<mock-inferior-package>
            mock-inferior-package
            mock-inferior-package?
            mock-inferior-package-name
            mock-inferior-package-version
            mock-inferior-package-synopsis
            mock-inferior-package-description
            mock-inferior-package-home-page

            with-mock-inferior-packages))

(define-record-type* <mock-inferior-package>
  mock-inferior-package make-mock-inferior-pacakge
  mock-inferior-package?
  (name        mock-inferior-package-name)
  (version     mock-inferior-package-version)
  (synopsis    mock-inferior-package-synopsis)
  (description mock-inferior-package-description)
  (home-page   mock-inferior-package-home-page))

(define (with-mock-inferior-packages f)
  (mock
   ((guix inferior)
    inferior-package-name
    mock-inferior-package-name)
   (mock
    ((guix inferior)
     inferior-package-version
     mock-inferior-package-version)
    (mock
     ((guix inferior)
      inferior-package-synopsis
      mock-inferior-package-synopsis)
     (mock
      ((guix inferior)
       inferior-package-description
       mock-inferior-package-description)
      (mock
       ((guix inferior)
        inferior-package-home-page
        mock-inferior-package-home-page)
       (f)))))))
