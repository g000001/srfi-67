;;;; srfi-67.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-67
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

(defmethod perform ((o test-op) (c (eql (find-system :srfi-67))))
  (load-system :srfi-67)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-67.internal :srfi-67))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
