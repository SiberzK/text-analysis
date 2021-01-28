;;;; TOKENIZER

;;;; This package is for parsing text and returning an array of tokens
;;;; that are EQ for the same pattern match.

(defpackage :tokenizer
  (:use :cl)
  (:export #:tokenize))
(in-package :tokenizer)

;; TODO: Proper system
(push "~/git/text-analysis/libs/lexer/" asdf:*central-registry*)
(asdf:load-system :lexer)

(lex:define-lexer *lexer* (state)
  ("%s+" (values :$whitespace))
  ("%a+" (values :$word $$))
  ("%d+" (values :$integer (parse-integer $$)))
  ("%p+" (values :$punctuation $$)))

(defvar *tokens* (make-hash-table :test #'equal
                                  :size 500000)
  "Registry of unique tokens.

Maps STRING -> TOKEN.")

(defparameter +token-types+ '(:$whitespace
                              :$punctuation
                              :$integer
                              :$word)
  "Only these types allowed in TOKEN.")

(defstruct token
  name ; a string
  type ; see +TOKEN-TYPES+
  )

(defmethod print-object ((token token) stream)
  (format stream "#:~a" (string-upcase (token-name token))))

(defun token (string type)
  "Return the unique TOKEN associated with STRING.

TYPE must be in +TOKEN-TYPES+.

Calling this twice with an EQUAL STRING will return an EQ TOKEN."
  (assert (member type +token-types+) nil
          "TYPE must be in +TOKEN-TYPES+!")
  (let ((token (gethash string *tokens*)))
    (or token
        (setf (gethash string *tokens*)
              (make-token :name string
                          :type type)))))

(defmethod tokenize ((string string))
  "Return a TOKEN array."
  (loop
    with tokens = (lex:tokenize '*lexer* string)
    with array = (make-array (length tokens))
    for token in tokens
    for i from 0
    do (setf (aref array i)
             (token (lex:token-lexeme token)
                    (lex:token-class  token)))
    finally (return array)))
