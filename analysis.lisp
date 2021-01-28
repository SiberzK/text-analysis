;;;; TEXT ANALYSIS

;;;; This package analyses text. It uses the TOKENIZER package to
;;;; compare textual representations in an efficient way.

(defpackage :text-analysis
  (:nicknames :sis)
  (:use :cl)
  (:export #:replace-names
           #:remove-stopwords
           #:step
           #:bigrams))
(in-package :text-analysis)


;;;; ALIASES

(defvar *aliases* nil
  "Hash-table of alias to term mappings.")

(defmethod replace-names ((tokens array)
                          &key (aliases *aliases*))
  "Find and replace aliases with their canonical names."
  (declare (ignore aliases))
  tokens)

(defmethod replace-names ((string string)
                          &key (aliases *aliases*))
  "Find and replace aliases with their canonical names."
  (replace-names (tokenizer:tokenize string)
                 :aliases aliases))


;;;; STOPWORDS

(defvar *stopwords* nil
  "Hash-table of stopwords.")

(defmethod remove-stopwords ((tokens array)
                             &key (stopwords *stopwords*))
  "Remove words like 'the'."
  (declare (ignore stopwords))
  tokens)

(defmethod remove-stopwords ((string string)
                             &key (stopwords *stopwords*))
  "Remove words like 'the'."
  (remove-stopwords (tokenizer:tokenize string)
                    :stopwords stopwords))


;;;; STEMMING

(defmethod stem ((tokens array))
  "Stem words. E.g 'running' -> 'run'"
  tokens)

(defmethod stem ((string string))
  "Stem words. E.g 'running' -> 'run'"
  (stem (tokenizer:tokenize string)))


;;;; BIGRAMS

(defmethod bigrams ((tokens array))
  "Return an array, the entire set of bigrams for each paragraph.

The index of the array corresponds to the paragraph number (starting
from 0), and the values are lists of pairs of strings."
  tokens)

(defmethod bigrams ((string string))
  "Return an array, the entire set of bigrams for each paragraph.

The index of the array corresponds to the paragraph number (starting
from 0), and the values are lists of pairs of strings."
  (bigrams (tokenizer:tokenize string)))

(defun prune-bigrams (bigrams %)
  "Remove the bottom % percentage."
  (declare (ignore %))
  bigrams)
