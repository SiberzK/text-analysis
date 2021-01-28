;;;; PARAGRAPHS

(defpackage :paragraphs
  (:use :cl)
  (:export #:tokenize))
(in-package :paragraphs)

(defclass paragraph ()
  ((text :initform (error "Requires TEXT.")
         :initarg :text
         :reader text
         :documentation "A string.
E.g 'Let's stop immigrants'")
   (tokens :initform nil
           :initarg :tokens
           :accessor tokens
           :documentation "An array of TOKEN objects.
E.g #(:LETS :STOP :IMMIGRANTS)")
   (results :initform (make-hash-table :test #'eq)
            :initarg :results
            :accessor results
            :documentation "Hash-table keyed by :KEYWORDS.

;; TODO
E.g :TENDENCIES is a hash-table keyed by keywords, themselves being
hash-tables mapping BIGRAM objects to an integer; the number of times
the BIGRAM appears in the text:

                           -- # (:STOP . :IMMIGRANT) => 1
            -- :POPULIST ---- # (:TAKE . :BACK) => 2
                           -- # (:NATIONAL . :SECURITY) => 1
 TENDENCIES -

            -- :NON-POPULIST ...")
   (properties :initform nil
               :initarg properties
               :accessor properties
               :documentation "Information.
E.g :STOPWORDS-REMOVED-P, :STEMMED-P")))

(defmethod tokenize ((paragraph paragraph))
  (unless (tokens paragraph)
    (setf (tokens paragraph)
          (tokenizer:tokenize (text paragraph)))))
