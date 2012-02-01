;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-67
  (:use)
  (:export
   :boolean-compare :char-compare :char-compare-ci :string-compare
   :string-compare-ci :symbol-compare :integer-compare :rational-compare
   :real-compare :complex-compare :number-compare :vector-compare
   :vector-compare-as-list :list-compare :list-compare-as-vector
   :pair-compare-car :pair-compare-cdr :pair-compare :default-compare
   :refine-compare :select-compare :cond-compare :if3 :if=? :if<? :if>? :if<=?
   :if>=? :if-not=? :=? :<? :>? :<=? :>=? :not=? :</<? :</<=? :<=/<? :<=/<=?
   :>/>? :>/>=? :>=/>? :>=/>=? :chain=? :chain<? :chain>? :chain<=? :chain>=?
   :pairwise-not=? :min-compare :max-compare :kth-largest :compare-by<
   :compare-by> :compare-by<= :compare-by>= :compare-by=/< :compare-by=/>
   :debug-compare ))

(defpackage :srfi-67.internal
  (:use :srfi-67 :cl :fiveam :mbe
        :srfi-5 :srfi-23 :srfi-16 :srfi-27 :srfi-42)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :loop :map :member :assoc :lambda))
