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

(defparameter *blocker-with-list-blocklist*
  (make-instance 'router:blocker
                 :route (match-domain *url*)
                 :blocklist '(:path (:starts "/about" :ends "/work")
                              :host (:starts "nyxt" :contains "atlas"))))

(defparameter *blocker-with-list-blocklist-or-rules*
  (make-instance 'router:blocker
                 :route (match-domain *url*)
                 :blocklist '(:or
                              (:path (:or (:starts "/about") (:ends "/work")))
                              (:host (:or (:starts "nyxt") (:contains "atlas"))))))

(defparameter *blocker-with-regexp-blocklist*
  (make-instance 'router:blocker
                 :route (match-domain *url*)
                 :blocklist "/(^nyxt)|work"))

(define-test redirector-with-list-rule ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/")
                   (nx-router::compute-router *redirector-with-list-rule* (quri:uri *url*)))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/")
                   (nx-router::compute-router *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/about/example")
                   (nx-router::compute-router *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/example")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/community/1234")
                   (nx-router::compute-router *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/c/1234")))
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/v/1234")
                   (nx-router::compute-router *redirector-with-list-rule*
                                             (quri:make-uri :defaults *url* :path "/v/1234"))))

(define-test redirector-with-nonstandard-port-and-scheme ()
  (assert-equality #'quri:uri=
                   (quri:uri "http://atlas.engineer:8080/articles")
                   (nx-router::compute-router *redirector-with-nonstandard-port-and-scheme*
                                             (quri:make-uri :defaults *url* :path "/articles"))))

(define-test redirector-with-regexp-route ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/nyxt/contact")
                   (nx-router::compute-router *redirector-with-regexp-route*
                                             (quri:make-uri :defaults *url*
                                                            :host "nyxt.atlas.engineer"
                                                            :path "/contact"))))

(define-test redirector-with-regexp-rule ()
  (assert-equality #'quri:uri=
                   (quri:uri "https://atlas.engineer/nyxt/contact")
                   (nx-router::compute-router *redirector-with-regexp-rule*
                                             (quri:make-uri :defaults *url*
                                                            :host "nyxt.atlas.engineer"
                                                            :path "/contact"))))

(define-test blocker-with-list-rule ()
  (assert-false (nx-router::compute-router *blocker-with-list-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :path "/about/work")))
  (assert-false (nx-router::compute-router *blocker-with-list-blocklist*
                                          (quri:make-uri :defaults *url*
                                                         :host "nyxt.atlas.engineer")))
  (assert-true (nx-router::compute-router *blocker-with-list-blocklist*
                                         (quri:make-uri :defaults *url*
                                                        :host "nyxt.atlas.engineer"
                                                        :path "/about/work"))))

(define-test blocker-with-list-rule-or-rules ()
  (assert-true (nx-router::compute-router *blocker-with-list-blocklist-or-rules*
                                          (quri:make-uri :defaults *url*
                                                         :path "/about/work")))
  (assert-true (nx-router::compute-router *blocker-with-list-blocklist-or-rules*
                                         (quri:make-uri :defaults *url*
                                                        :host "nyxt.atlas.engineer")))
  (assert-true (nx-router::compute-router *blocker-with-list-blocklist-or-rules*
                                         (quri:make-uri :defaults *url*
                                                        :host "nyxt.atlas.engineer"
                                                        :path "/about/work")))
  (assert-false (nx-router::compute-router *blocker-with-list-blocklist-or-rules*
                                          (quri:make-uri :defaults *url*))))

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
