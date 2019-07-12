(define-module (tests model-license)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix utils)
  #:use-module (guix tests)
  #:use-module (guix-data-service database)
  #:use-module (tests mock-inferior)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service jobs load-new-guix-revision))

(test-begin "jobs-load-new-guix-revision")

(with-postgresql-connection
 "test-jobs-load-new-guix-revision"
 (lambda (conn)
   (test-equal "select-job-for-commit works"
     '()
     (select-job-for-commit conn "does not exist"))

   (test-equal "test job success"
     #t
     (mock
      ((guix-data-service jobs load-new-guix-revision)
       store-item-for-git-repository-id-and-commit
       (lambda (conn git-repository-id commit)
         "/gnu/store/test"))

      (mock
       ((guix-data-service jobs load-new-guix-revision)
        extract-information-from
        (lambda (conn git-repository-id commit store-path)
          #t))

       (enqueue-load-new-guix-revision-job
        conn
        (git-repository-url->git-repository-id conn "test-url")
        "test-commit"
        "test-source")

       (process-next-load-new-guix-revision-job conn))))

   (test-equal "test build store item failure"
     #f
     (mock
      ((guix-data-service jobs load-new-guix-revision)
       store-item-for-git-repository-id-and-commit
       (lambda (conn git-repository-id commit)
         #f))

      (enqueue-load-new-guix-revision-job
       conn
       (git-repository-url->git-repository-id conn "test-url")
       "test-commit"
       "test-source")

      (process-next-load-new-guix-revision-job conn)))

   (test-equal "test extract information failure"
     #f
     (mock
      ((guix-data-service jobs load-new-guix-revision)
       store-item-for-git-repository-id-and-commit
       (lambda (conn git-repository-id commit)
         "/gnu/store/test"))

      (mock
       ((guix-data-service jobs load-new-guix-revision)
        extract-information-from
        (lambda (conn git-repository-id commit store-path)
          #f))

       (enqueue-load-new-guix-revision-job
        conn
        (git-repository-url->git-repository-id conn "test-url")
        "test-commit"
        "test-source")

       (process-next-load-new-guix-revision-job conn))))))

(test-end)
