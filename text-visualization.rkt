#lang racket

(require 2htdp/image)
(require rackunit/text-ui)
(require csc151)
(require rackunit)
(provide (all-defined-out))
(require csc151/rex)

;;;CSC-151-01 (Fall 2021)
;;;Name: David Quintana
;;;Lab: text-visualization.rkt
;;;Date: October 7, 2021
;;;Acknowledgments: Jio Hong, Ben Cerrato & Mini Project 3 (language.rkt)
;;;Title: The Odyssey (the-odyssey.txt) ||| Author: Homer & Title: Little Women (the-iliad.txt) ||| Author: Louisa May Alcott

;;;Regular Expression

;;; (total-letters (txt))->integetr?
;;; txt: file text
;;;takes a file text, turns it to a list, filters only the alphabetically
;;;characters and outputs the sum off characters
(define total-letters
  (lambda (txt)
    (length (filter char-alphabetic? (file->chars txt)))))

;;; (total-integers (txt))->integetr?
;;; txt: file text
;;;takes a file text, turns it to a list, filters only the integer
;;;characters and outputs the sum off characters
(define total-integers
  (lambda (txt)
    (length (filter char-numeric? (file->chars txt)))))

;;; (total-integers (txt))->integetr?
;;; txt: file text
;;;takes a file text, turns it to a list, filters only the integer
;;;characters and outputs the sum off characters
(define total-whitespace
  (lambda (txt)
    (length (filter char-whitespace? (file->chars txt)))))


(define test-my-regular-expressions
  (test-suite
   "Test of Length Regular Expressions"
   (test-case
    "Total Lettrs"
    (check-equal? (total-letters "the-odyssey.txt") 541729)
    "The Odyssey has 541729 total letters."
    (check-equal? (total-letters "the-iliad.txt") 869701)
    "The Iliad has 869701 total letters.")
   (test-case
    "Total Integers"
    (check-equal? (total-integers "the-odyssey.txt") 1636)
    "The Odyssey has 1636 total integers."
    (check-equal? (total-integers "the-iliad.txt") 2477)
    "The Iliad has 2477 total integers.")
   (test-case
    "Total White Space"
    (check-equal? (total-whitespace "the-odyssey.txt") 146828)
    "The Odyssey has 146828 total whitespace."
    (check-equal? (total-whitespace "the-iliad.txt") 220426)
    "The Iliad has 220426 total whitespace.")))

;;;Text Analysis

;;;(easy-words)->file txt?
;;;a file of strings that are "easy words"
(define easy-words
  (string-split (file->string "easy-words.txt")))

;;;(num-difficult-words (txt) ) -> positive integer?
;;;txt: file text
;;;Returns the number of "hard words", words not in "readability-formula.txt"
(define num-difficult-words
  (lambda (txt)
    (length (remove* easy-words(string-split (file->string txt))))))

;;;(total-wrds (txt) ) -> positive integer?
;;;txt: file text
;;;returns the number of words in the text file

(define total-words
  (lambda (txt)
    (length (string-split (file->string txt)))))

;;;(num-sentences (txt) ) -> positive integer?
;;;txt: file text
;;;Returns the number of sentences in the text file

(define num-sentences
  (lambda (txt)
    (length (string-split (file->string txt) "."))))

;;;(compute-dale-chall-score (num-difficult-words total-words total-sentences) ) -> positive integer?
;;;num-difficult-words: returns the number of "hard words", words not in "readability-formula.txt"
;;;total-words: returns the number of words in the text file
;;;num-sentences: returns the number of sentences in the text file
;;;returns the dale chall score for a given text file
(define compute-dale-chall
  (lambda (num-difficult-words total-words num-sentences)
    (let ([PDW (/ num-difficult-words total-words)]
          [ASD (/ total-words num-sentences)])
      (cond
        [(<= PDW .05)
         (+ (* 0.1579 (* PDW 100)) (* ASD 0.496) ) ]
        [else
         (+ 3.6365 (+ (* 0.1579 (* PDW 100) ) (* ASD 0.0496)))]))))

;;;(score->grade (score) ) -> string?
;;;n: list returned by compute-dale-chall
;;;returns a string stating the grade level based on the readability
(define score->grade
  (lambda (n)
    (cond
      [(<= n 4.)
       "4th grade or lower"]
      [(and (< 4.9 n) (<= n 5.9))
       "5th-6th Grade"]
      [(and ( < 5.9 n) (<= n 6.9))
       "7th-8th Grade"]
      [(and (< 6.9 n) (<= n 7.9))
       "9th-10th Grade"]
      [(and (< 7.9 n) (<= n 8.9))
       "11th-12th Grade"]
      [(< 8.9 n)
       "13th-15th Grade"])))


(define test-my-text-anylsis
  (test-suite
   "Test of Text Analysis Functions"
   (test-case
    "Dale Chall Score"
    (check-equal? (compute-dale-chall 38744 132691 4635) 9.666920289319199)
    "The Odyssey has a Dale Chall Score of 9.666920289319199."
    (check-equal? (compute-dale-chall 102105 192197 7050) 13.377160237334351)
    "The Iliad has a Dale Chall Score of 13.377160237334351.")
   (test-case
    "Reading Level"
    (check-equal? (score->grade 9.666920289319199) "13th-15th Grade"))
   "The Odyssey has a 13th-15th Grade reading level."
   (check-equal? (score->grade 13.377160237334351) "13th-15th Grade")
   "The Iliad has a 13th-15th Grade reading level."))

;;;Text Analysis

;;;(head)->image?
;;;returns a blue square head
(define male-head
  (overlay (square 100 'solid "blue") (square 120 'solid "black")))

;;;(head)->image?
;;;returns a pink square head
(define female-head
  (overlay (square 100 'solid "pink") (square 120 'solid "black")))

;;;(head)->image?
;;;returns a yellow square head
(define neutral-head
  (overlay (square 100 'solid "pink") (square 120 'solid "black")))

;;;(eyes)->image?
;;;returns eyes
(define neutral-eyes
  (above (beside (square 20 'solid "black") (square 20 'solid "yellow") (square 20 'solid "black")) (rectangle 40 10 'solid "yellow")))

;;;(smile)->image?
;;;returns a postive smile
(define neutral-smile
  (above (beside (square 10 'solid "black") (rectangle 40 10 'solid "yellow") (square 10 'solid "black")) (rectangle 40 10 'solid "black")))

;;;(eyes)->image?
;;;returns eyes
(define male-eyes
  (above (beside (square 20 'solid "black") (square 20 'solid "blue") (square 20 'solid "black")) (rectangle 40 10 'solid "blue")))

;;;(smile)->image?
;;;returns a postive smile
(define male-smile
  (above (beside (square 10 'solid "black") (rectangle 40 10 'solid "blue") (square 10 'solid "black")) (rectangle 40 10 'solid "black")))

;;;(eyes)->image?
;;;returns eyes
(define female-eyes
  (above (beside (square 20 'solid "black") (square 20 'solid "pink") (square 20 'solid "black")) (rectangle 40 10 'solid "pink")))

;;;(smile)->image?
;;;returns a postive smile
(define female-smile
  (above (beside (square 10 'solid "black") (rectangle 40 10 'solid "pink") (square 10 'solid "black")) (rectangle 40 10 'solid "black")))

;;;(positive-face)->image?
;;;returns a positive face
(define male-face
  (overlay (above male-eyes male-smile) male-head))

;;;(positive-face)->image?
;;;returns a positive face
(define female-face
  (overlay (above female-eyes female-smile) female-head))

;;;(positive-face)->image?
;;;returns a positive face
(define neutral-face
  (overlay (above neutral-eyes neutral-smile) neutral-head))

;;;(extract words (str) ) -> list of strings?
;;;str: string?
;;;returns a list of strings that match the rex expression
(define extract-words
  (let* ([letter (rex-char-range #\A  #\z)]
         [apostrophe (rex-char-set "'")]
         [word (rex-concat (rex-repeat letter) (rex-repeat-0 apostrophe) (rex-repeat-0 letter))])
    (lambda (str)
      (rex-find-matches word str))))

(define male
  (lambda (str)
    (- (length (file->words str))(length (remove* (file->words "male.txt")(map string-downcase (file->words str)))))))

(define female
  (lambda (str)
    (- (length (file->words str))(length (remove* (file->words "female.txt")(map string-downcase (file->words str)))))))

;;;(pos-or-neg-story(txt))-> image?
;;;str: file text
;;;returns a smiling face if positive, sad face if negative and neutral face if neutral
(define male-or-female
  (lambda (txt)
    (cond
      [( > (male txt) (female txt))
       male-face]
      [( > (female txt) (male txt))
       female-face]
      [else
       neutral-face])))

(define analysis-maker
  (above (male-or-female "the-odyssey.txt") (male-or-female "little-women.txt")))




