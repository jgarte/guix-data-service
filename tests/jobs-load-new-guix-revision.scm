(define-module (tests jobs-load-new-guix-revision)
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

       (match (enqueue-load-new-guix-revision-job
               conn
               (git-repository-url->git-repository-id conn "test-url")
               "test-commit"
               "test-source")
         ((id)
          (process-load-new-guix-revision-job id))))))

   (exec-query conn "TRUNCATE load_new_guix_revision_jobs CASCADE")

   (test-equal "test build store item failure"
     #f
     (mock
      ((guix-data-service jobs load-new-guix-revision)
       store-item-for-git-repository-id-and-commit
       (lambda (conn git-repository-id commit)
         #f))

      (match (enqueue-load-new-guix-revision-job
              conn
              (git-repository-url->git-repository-id conn "test-url")
              "test-commit"
              "test-source")
        ((id)
         (process-load-new-guix-revision-job id)))))

   (exec-query conn "TRUNCATE load_new_guix_revision_jobs CASCADE")

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

       (match (enqueue-load-new-guix-revision-job
               conn
               (git-repository-url->git-repository-id conn "test-url")
               "test-commit"
               "test-source")
         ((id)
          (process-load-new-guix-revision-job id))))))

   (exec-query conn "TRUNCATE load_new_guix_revision_jobs CASCADE")

   (test-assert "test duplicate job handling"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (enqueue-load-new-guix-revision-job
         conn
         (git-repository-url->git-repository-id conn "test-url")
         "test-commit"
         "test-source")
        (enqueue-load-new-guix-revision-job
         conn
         (git-repository-url->git-repository-id conn "test-url")
         "test-commit"
         "test-source")
        #t)
      #:always-rollback? #t))))


(test-end)
