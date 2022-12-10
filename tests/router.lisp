(in-package #:cl-user)
(uiop:define-package #:nx-router/tests
  (:use #:cl #:lisp-unit2)
  (:import-from :nx-router))

(in-package #:nx-router/tests)
(nyxt:use-nyxt-package-nicknames)

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system :nx-router))))
  (asdf:oos 'asdf:load-op :nx-router/tests)
  (let ((*package* (find-package :nx-router/tests)))
    (uiop:symbol-call :lisp-unit2 :run-tests
                      :package :nx-router/tests
                      :name :nx-router
                      :run-context (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))

(defvar *url* "https://example.org/")

(defparameter *redirector-with-list-rule*
  (make-instance 'router:redirector
                 :trigger (nyxt:match-domain *url*)
                 :redirect-url "atlas.engineer"
                 :redirect-rule '(("/about/" . (not "/" "/v/"))
                                  ("/contact" . "/c/"))))

(defparameter *redirector-with-regexp-rule*
  (make-instance 'router:redirector
                 :trigger (nyxt:match-domain *url*)
                 :redirect-url "https://atlas.engineer/\\1/\\2"
                 :redirect-rule "https://(\\w+)\\.atlas.engineer/(.*)"))

(defparameter *blocker-with-list-blocklist*
  (make-instance 'router:blocker
                 :trigger (nyxt:match-domain *url*)
                 :blocklist '(:path (:contains ("work"))
                              :host (:starts ("/nyxt")))))

(defparameter *blocker-with-regexp-blocklist*
  (make-instance 'router:blocker
                 :trigger (nyxt:match-domain *url*)
                 :blocklist "(^/nyxt)|work"))

(define-test redirector-with-list-rule ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/")
                   (nx-router::compute-route *redirector-with-list-rule* (quri:uri *url*)))
  (assert-equality #'quri:uri=
                   (quri:uri"https://atlas.engineer/articles")
                   (nx-router::compute-route *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/articles")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/about")
                   (nx-router::compute-route *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/example")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/contact/1234")
                   (nx-router::compute-route *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/c/1234")))
  (assert-equality #'quri:uri=
                   (quri:uri "http://atlas.engineer/v/1234")
                   (nx-router::compute-route *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/v/1234"))))

(define-test redirector-with-regexp-rule ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/nyxt")
                   (nx-router::compute-route *redirector-with-regexp-rule*
                                             (quri:make-uri :defaults *url*
                                                            :host "nyxt.atlas.engineer"
                                                            :path "/contact"))))

(define-test blocker-with-list-rule ()
  (assert-true (nx-router::compute-route *blocker-with-list-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :path "/work")))
  (assert-true (nx-router::compute-route *blocker-with-list-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :path "/nyxt")))
  (assert-false (nx-router::compute-route *blocker-with-list-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :path "/contact"))))

(define-test blocker-with-regexp-rule ()
  (assert-true (nx-router::compute-route *blocker-with-regexp-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :path "/work")))
  (assert-true (nx-router::compute-route *blocker-with-regexp-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :path "/nyxt")))
  (assert-false (nx-router::compute-route *blocker-with-regexp-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :path "/contact"))))
