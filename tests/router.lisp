(in-package #:nx-router/tests)
(nyxt:use-nyxt-package-nicknames)

(defvar *url* "https://example.org/")

(defparameter *redirector-with-host-redirect*
  (make-instance 'router:redirector
                 :route (match-domain *url*)
                 :redirect "atlas.engineer"))

(defparameter *redirector-with-quri-redirect*
  (make-instance 'router:redirector
                 :route (match-domain *url*)
                 :redirect (quri:uri "http://atlas.engineer:8080")))

(defparameter *redirector-with-regexp-redirect*
  (make-instance 'router:redirector
                 :route "https://(\\w+)\\.atlas.engineer/(.*)"
                 :redirect "https://atlas.engineer/\\1/\\2"))

(defparameter *redirector-with-list-redirect*
  (make-instance 'router:redirector
                 :route (match-domain *url*)
                 :redirect
                 '(("https://atlas.engineer/community/\\1" . ".*/c/(.*)")
                   ("https://atlas.engineer/" . ".*/$")
                   ("https://atlas.engineer/about/" . (not ".*/v/.*")))))

(defparameter *redirector-with-list-redirect-regexp-interpolation*
  (make-instance 'router:redirector
                 :route (match-domain *url*)
                 :redirect
                 '(("https://atlas.engineer/\\1/\\2" . "https://(\\w+)\\.atlas.engineer/(.*)"))))

(define-test redirector-basic ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/articles")
                   (nx-router::compute-router *redirector-with-host-redirect*
                                              (quri:make-uri :defaults *url* :path "/articles")))
  (assert-equality #'quri:uri=
                   (quri:uri "http://atlas.engineer:8080/articles")
                   (nx-router::compute-router *redirector-with-quri-redirect*
                                              (quri:make-uri :defaults *url* :path "/articles"))))

(define-test redirector-regexp-interpolation ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/nyxt/contact")
                   (nx-router::compute-router *redirector-with-regexp-redirect*
                                             (quri:make-uri :defaults *url*
                                                            :host "nyxt.atlas.engineer"
                                                            :path "/contact")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/nyxt/contact")
                   (nx-router::compute-router *redirector-with-list-redirect-regexp-interpolation*
                                             (quri:make-uri :defaults *url*
                                                            :host "nyxt.atlas.engineer"
                                                            :path "/contact"))))

(define-test redirector-list-redirect ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/")
                   (nx-router::compute-router *redirector-with-list-redirect* (quri:uri *url*)))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/")
                   (nx-router::compute-router *redirector-with-list-redirect*
                                              (quri:make-uri :defaults *url* :path "/")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/about/example")
                   (nx-router::compute-router *redirector-with-list-redirect*
                                              (quri:make-uri :defaults *url* :path "/example")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/community/1234")
                   (nx-router::compute-router *redirector-with-list-redirect*
                                              (quri:make-uri :defaults *url* :path "/c/1234"))))

(defparameter *blocker-with-list-blocklist*
  (make-instance 'router:blocker
                 :route (match-domain *url*)
                 :blocklist (list ".*/about.*/work$" ".*://nyxt.*atlas.*")))

(defparameter *blocker-with-regexp-blocklist*
  (make-instance 'router:blocker
                 :route (match-domain *url*)
                 :blocklist "/(^nyxt)|work"))

(defparameter *blocker-with-list-exception-blocklist*
  (make-instance 'router:blocker
                 :route (match-domain *url*)
                 :blocklist '(or "://.*/.*" (not ".*/about.*" ".*://nyxt.*atlas.*"))))

(define-test blocker-with-list-blocklist ()
  (assert-false (nx-router::compute-router *blocker-with-list-blocklist*
                                           (quri:make-uri :defaults *url*
                                                          :path "/about/work")))
  (assert-false (nx-router::compute-router *blocker-with-list-blocklist*
                                           (quri:make-uri :defaults *url*
                                                          :host "nyxt.atlas.engineer")))
  (assert-true (nx-router::compute-router *blocker-with-list-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :host "nyxt.atlas.engineer"
                                                         :path "/about/work")))
  (assert-true (nx-router::compute-router *blocker-with-list-exception-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :path "/work/")))
  (assert-true (nx-router::compute-router *blocker-with-list-exception-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :host "nyxt.atlas.engineer")))
  (assert-false (nx-router::compute-router *blocker-with-list-exception-blocklist*
                                           (quri:make-uri :defaults *url*
                                                          :host "nyxt.atlas.engineer"
                                                          :path "/about/work"))))

(define-test blocker-with-regexp-rule ()
  (assert-true (nx-router::compute-router *blocker-with-regexp-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :path "/work")))
  (assert-false (nx-router::compute-router *blocker-with-regexp-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :path "/nyxt")))
  (assert-false (nx-router::compute-router *blocker-with-regexp-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :path "/contact"))))
