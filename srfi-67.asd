;;;; srfi-67.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-67
  :version "20200326"
  :description "SRFI 67 for CL: Compare Procedures"
  :long-description "SRFI 67 for CL: Compare Procedures
https://srfi.schemers.org/srfi-67"
  :author "Sebastian Egner and Jens Axel S{\o}gaard"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :mbe
               :srfi-5
               :srfi-16
               :srfi-23
               :srfi-27
               :srfi-42)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-67")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-67))))
  (let ((name "https://github.com/g000001/srfi-67")
        (nickname :srfi-67))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-67))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-67#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-67)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
