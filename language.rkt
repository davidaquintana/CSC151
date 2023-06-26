#lang racket
(require csc151)
(require rackunit)
(provide (all-defined-out))
(require csc151/rex)

;;;CSC-151-01 (Fall 2021)
;;;David Quintana
;;;Lab: language.rkt
;;;Date: October 24, 2021
;;;Acknowledgments: Jio Hong & Ben Cerrato
;;;Title: The Odyssey ||| Author: Homer
;;positive-words.txt & neg-words.txt
;;; Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
;       Proceedings of the ACM SIGKDD International Conference on Knowledge 
;       Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
;       Washington, USA, 
;   Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing 
;       and Comparing Opinions on the Web." Proceedings of the 14th 
;       International World Wide Web conference (WWW-2005), May 10-14, 
;       2005, Chiba, Japan.


;Problem was easy-words was not cited or submitted and that there should be a smarter way to count sentnces
;I submitted and refren

;;;Part the First: Word Counts

;;;(count-words (lst txt) ) -> list?
;;;lst: list of strings 
;;;txt: file text 
;;;Returns a list that states how many times a certain word appears in the text file

(define count-words
  (lambda (lst txt)
    (let ( [all-words (string-split (file->string txt) ) ] )
      (cond
        [(null? lst)
         '()]
        [else
         (cons (list (car lst)
                     (tally-value (map string-downcase all-words) (string-downcase (car lst) ) ) )
               (count-words (cdr lst) txt) ) ] ) ) ) )

;;;Part the Second: Readability

(define easy-words
  (string-split (file->string "easy-words.txt") ) )

;;;(num-difficult-words (txt) ) -> positive integer?
;;;txt: file text
;;;Returns the number of "hard words", words not in "readability-formula.txt"

(define num-difficult-words
  (lambda (txt)
    (length (remove* easy-words(string-split (file->string txt) ) ) ) ) )

; ->38744

;;;(total-wrds (txt) ) -> positive integer?
;;;txt: file text
;;;returns the number of words in the text file

(define total-words
  (lambda (txt)
    (length (string-split (file->string txt) ) ) ) )

; ->132691

;;;(num-sentences (txt) ) -> positive integer?
;;;txt: file text
;;;Returns the number of sentences in the text file by adding all the periods and semicolons

(define num-periods
  (lambda (txt)
    (length (string-split (file->string txt) ".") ) ) )

(define num-semicolons
  (lambda (txt)
    (length (string-split (file->string txt) ";"))))

(define num-sentences
  (lambda (txt)
    (+ num-periods num-semicolons)))

; -> 4635

;;;(compute-dale-chall-score (num-difficult-words total-words total-sentences) ) -> positive integer?
;;;num-difficult-words: returns the number of "hard words", words not in "readability-formula.txt"
;;;total-words: returns the number of words in the text file
;;;num-sentences: returns the number of sentences in the text file
;;;returns the dale chall score for a given text file
(define compute-dale-chall-score
  (lambda (num-difficult-words total-words num-sentences)
    (let* ([PDW (/ num-difficult-words total-words)]
          [ASD (/ total-words num-sentences)])
      (cond
        [(<= PDW .05)
         (+ (* 0.1579 (* PDW 100)) (* ASD 0.496) ) ]
        [else
         (+ 3.6365 (+ (* 0.1579 (* PDW 100) ) (* ASD 0.0496) ) ) ] ) ) ) )

; -> 9.666920289319199

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
      [(and ( < 5.9 n) (<= n 6.9) )
       "7th-8th Grade"]
      [(and (< 6.9 n) (<= n 7.9) )
       "9th-10th Grade"]
      [(and (< 7.9 n) (<= n 8.9) )
       "11th-12th Grade"]
      [(< 8.9 n)
       "13th-15th Grade"] ) ) )

; -> "13th-15th Grade"

;;;(extract words (str) ) -> list of strings?
;;;str: string?
;;;returns a list of strings that match the rex expression

(define extract-words
  (let* ([letter (rex-char-range #\A  #\z)]
         [apostrophe (rex-char-set "'")]
         [word (rex-concat (rex-repeat letter) (rex-repeat-0 apostrophe) (rex-repeat-0 letter) ) ]
         )
    (lambda (str)
      (rex-find-matches word str) ) ) )

;;;(dale-chall-score (str) ) -> positive real number?
;;;str: string?
;;;returns the dale chall score of a string

(define dale-chall-score
  (lambda (str)
    (let* ([num-difficult-words (length (remove* (file->words "WordList.txt")(map string-downcase (file->words str))))]
           [num-sentences (length (string-split (file->string str) "."))]
           [total-words (length (file->words str))]
           [ASL (/ num-difficult-words num-sentences)]
           [PDW (/ num-difficult-words total-words)])
      (if (> .05 PDW)
          (+(*(* PDW 100) .0496)(* ASL .0496))
          (+(+(*(* PDW 100) .1579)(* ASL .0496))3.6365)))))

;;;Part the Third: Sentiment Analysis

(define pos-words
  (lambda (str)
    (- (length (file->words str))(length (remove* (file->words "positive-words.txt")(map string-downcase (file->words str)))))))

  (define neg-words
    (lambda (str)
    (- (length (file->words str))(length (remove* (file->words "negative-words.txt")(map string-downcase (file->words str)))))))

  ;;;(posneg (str) ) -> string?
  ;;;str: string?
  ;;;returns "positive" if more positive, "negative" if more negative

  (define posneg
    (lambda (str)
      (cond
        [( < (pos-words str) (neg-words str) )
         "positive"]
        [( < (neg-words str)  (pos-words str) )
         "negative"]
        [else
         "neutral"] ) ) )

  ;Part the Fourth: Basic Gender Analysis

  (define male
    '( "he" "his" "him") )

  (define female
    '( "she" "her" "hers") )

  ;;; (count-male-pronouns str) -> integer?
  ;;; str: string
  ;;; Count how many times that male pronouns appear as individual words in str

  (define count-male-pronouns
    (lambda (str)
      (- (length (file->words str))(length (remove* male (map string-downcase (file->words str)))))))

  ;;; (count-female-pronouns str) -> integer?
  ;;; str: string
  ;;; Count how many times that female pronouns appear as individual words in str
  (define count-female-pronouns
    (lambda (str)
      (- (length (file->words str))(length (remove* female (map string-downcase (file->words str)))))))
  ;;;Part the Fifth: Freestyle

  ;;;(letter-count (txt) ) -> integer?
  ;;;txt: file text
  ;;;outputs the amount of letters in the file text
  (define letter-count
    (lambda (txt)
      (length (rex-split-string (rex-repeat (rex-char-antiset (list->string (map integer->char (append (range (char->integer #\a) (+ (char->integer #\z) 1) ) (char->integer #\a) (+ (char->integer #\z) 1 ) ) )) ) ) ) ) ) )

  ;;;(character-count (txt) ) -> integer?
  ;;;txt: file text
  ;;;outputs the total amount of characters
  (define character-count
    (lambda (txt)
      (length (file->chars txt) ) ) )
     
                        
             

  