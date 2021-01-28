;;;; BIGRAMS

(defpackage :bigram-classifier
  (:use :cl)
  (:export #:classify))
(in-package :bigrams)


;;;; ASSOCIATE BIGRAMS

(defun associate-bigrams (bigrams association-fn)
  "Fill in TENDENCY slot of each BIGRAM in BIGRAMS.

According to ASSOCIATION-FN."
  (mapcar association-fn bigrams))

(defun process-bigrams (bigrams processing-fn)
  "Process array BIGRAMS according to PROCESSING-FN."
  (mapcar processing-fn bigrams))


;;;; OPINION

(defun bigrams/opinion-per-paragraph (paragraphs opinions)
  "Return HASH-TABLE of BIGRAMS -> OPINIONS.

Maps each BIGRAM to a plist whose keys are opinions as keywords, and
whose values are integers, counting the number of times the bigram has
been associated with that opinion."
  (loop
    with registry = (make-hash-table :test #'equal)
    for paragraph in paragraphs
    for opinion in opinions
    do (loop
         for bigram in (bigrams paragraph)
         do (incf (getf (gethash bigram registry)
                        opinion
                        0)))
    finally (return registry)))


;;;;
(defclass bigram-classifier ()
  ((bigrams :initarg :bigrams
            :reader bigrams
            :documentation "Hash-table of CONS -> TENDENCY.

CONS of TOKENS:
 (#:STOP . #:IMMIGRANT)

TENDENCY is a KEYWORD, e.g :POPULIST.")))

(defmethod classify (paragraphs (classifier bigram-classifier))
  "Analyze PARAGRAPHS using BIGRAM tendency mappings.

Fill in the TENDENCIES slot of each PARAGRAPH in PARAGRAPHS."
  (loop
    for paragraph across paragraphs
    for i from 0
    collect (sb-thread:make-thread
             #'match-bigrams
             :name (format nil "Attribute Tendenices (~a)" i)
             :arguments `(,paragraph
                          ,(bigrams bigram-classifier)))
      into threads
    finally (mapcar #'sb-thread:join-thread threads))
  paragraphs)

(defun match-bigrams (tokens bigram-mappings)
  "Return a HASH-TABLE of opinions.

Keys are opinions as keywords, values lists of bigrams that counted
towards that opinion."
  (let ((bigrams (bigrams tokens)))
    (loop with registry = (make-hash-table)
          for bigram in bigrams
          for opinion = (gethash bigram bigram-mappings)
          when opinion
            do (push bigram
                     (gethash opinion registry))
          finally (return registry))))


(defun do-it-all (paragraph bigrams
                  &key (remove-stopwords t)
                    (stem t))
  (tokenizer:tokenize paragraph)
  (when remove-stopwords
    (sis:remove-stopwords paragraph))
  (when stem
    (sis:stem paragraph))
  (match-bigrams (tokens paragraph)
                 bigrams))
