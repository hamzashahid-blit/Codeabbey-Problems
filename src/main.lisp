(defpackage cl-codeabbey
  (:use :cl))
(in-package :cl-codeabbey)

;; Use these 2 Helper functions taking in a string to parse to a list or a list of lists.
(defun parse-list (input)
  (mapcar #'parse-integer (str:words input)))

(defun parse-lists (input)
  (mapcar (lambda (line)
            (mapcar 'parse-integer (str:words line)))
          (str:lines input)))

;; ---------------

;; Problem 1
(defun sum (a b)
  (+ a b))

;; Problem 2
(defun sum-in-loop (data)
  (reduce #'+ data))

;; Problem 3
(defun sums-in-loop (pairs)
  (loop :for pair :in pairs
    :collect (apply #'+ pair)))

;; Problem 4
(defun minimum-of-two (pairs)
  (mapcar (lambda (pair) (apply #'min pair)) pairs))

;; Problem 5
(defun minimum-of-three (pairs)
  (minimum-of-two pairs))

(defun round-to-infinity (number)
  (multiple-value-bind (integral fractional)
      (truncate number)
    (if (< (abs fractional) 0.5)
      integral
      (+ (signum number) integral))))

;; Problem 6
(defun rounding (pairs)
  (mapcar (lambda (pair) (round-to-infinity (apply #'/ pair))) pairs))

;; Problem 7
(defun fahrenheit-to-celsius (list)
  (mapcar (lambda (n) (round-to-infinity (* (- n 32) 5/9))) list))

;; Problem 8
(defun arithmetic-progression (triplets)
  (mapcar (lambda (triplet)
            (destructuring-bind (n increment steps) triplet
              (loop :for i :from 0 :to (1- steps)
                    :do (format t "+ ~a,~a,~a " n increment steps) 
                    :sum (+ n (* i increment)))))
          triplets))

;; Problem 9
(defun triangles (triplets)
  (mapcar (lambda (triplet)
            (destructuring-bind (a b c) triplet
              (if (and (> (+ a b) c)
                       (> (+ a c) b)
                       (> (+ b c) a)) 1 0)))
          triplets))

;; Problem 10
(defun linear-function (values)
  (mapcar (lambda (quadruplet)
            (destructuring-bind (x1 y1 x2 y2) quadruplet
              (let* ((gradient (/ (- y2 y1) (- x2 x1)))
                     (intercept (- y1 (* gradient x1)))) ;; (y = mx + c) Where x=0 and c=gradient
                (list gradient intercept))))
          values))

;; Problem 11
(defun sum-of-digits (triplets)
  (mapcar (lambda (triplet)
            (destructuring-bind (a b c) triplet
              (loop :for digit :across (write-to-string (+ (* a b) c))
                :sum (parse-integer (string digit)))))
          triplets))

;; Problem 12
(defun modulo-and-time-difference (octuplets)
  (mapcar (lambda (octuplet)
            (destructuring-bind (day1 hour1 min1 sec1 day2 hour2 min2 sec2) octuplet
              (let* ((+day+ (* 60 60 24)) (+hour+ (* 60 60)) (+min+ 60) (+sec+ 1)
                     (time1 (+ sec1 (* min1 +min+) (* hour1 +hour+) (* day1 +day+)))
                     (time2 (+ sec2 (* min2 +min+) (* hour2 +hour+) (* day2 +day+)))
                     (time-diff (- time2 time1))
                     (days    (/ time-diff +day+))
                     (hours   (/ (setf time-diff (mod time-diff +day+)) +hour+))
                     (minutes (/ (setf time-diff (mod time-diff +hour+)) +min+))
                     (seconds (/ (setf time-diff (mod time-diff +min+)) +sec+)))
                (mapcar #'truncate (list days hours minutes seconds)))))
            octuplets))

;; Problem 13
(defun weighted-sum-of-digits (numbers)
  (mapcar (lambda (number)
            (loop :for digit :across (write-to-string number)
                  :for i :from 1
                  :sum (* (parse-integer (string digit)) i)))
          numbers))

;; Problem 14
(defun modular-calculator (calculations)
  (let* ((calculations (mapcar #'str:words (str:lines calculations)))
         (initial-number (parse-integer (car (car calculations))))
         (operations (cdr calculations))
         (result initial-number))
    (loop :for operation :in operations
          :do (let ((operator (car operation))
                    (operand (parse-integer (second operation))))
                (cond ((equalp "+" operator) (incf result operand))
                      ((equalp "*" operator) (setf result (* result operand)))
                      ((equalp "%" operator) (setf result (mod result operand))))))
    result))

;; Problem 15
(defun maximum-of-array (list)
  (list (apply #'max list) (apply #'min list)))

;; Problem 16
(defun average-of-an-array (lists)
  (mapcar (lambda (list)
            (let ((list (butlast list)))
              (round (/ (apply #'+ list) (length list)))))
          lists))

;; Problem 17
(defun array-checksum (numbers)
  (let ((result 0))
    (loop :for n :in numbers
          :do (setf result (mod (* (+ result n) 113) 10000007)))
    result))

;; Problem 18
(defun square-root (pairs)
  (mapcar (lambda (pair)
            (let ((result 1))
              (loop :repeat (second pair)
                :do (let ((x (/ (first pair) result)))
                      (setf result (/ (+ result x) 2))))
              (coerce result 'double-float)))
          pairs))

;; Problem 19 [FAILED]
;; (defun matching-brackets (input)
;;   (let* ((delimiters "()[]{}<>")
;;          (input (remove-if-not (lambda (c) (find c delimiters)) input)))
;;     (loop :for char :across input
;;           :do (format t "~a" char))))
;; (defun all-zero (&rest args)
;;   (every (lambda (n) (= 0 n)) args))
;; (defun matching-brackets (input)
;;   (mapcar (lambda (string)
;;             (block func
;;               (let ((round 0) (square 0) (curly 0) (angle 0))
;;                 (loop :for char :across string
;;                   :do (cond ((char= char #\() (incf round))
;;                         ((char= char #\))
;;                           (if (all-zero square curly angle) (decf round) (return-from func 0)))
;;                         ((char= char #\[) (incf square))
;;                         ((char= char #\])
;;                           (if (all-zero round curly angle) (decf square) (return-from func 0)))
;;                         ((char= char #\{) (incf curly))
;;                         ((char= char #\})
;;                           (if (all-zero round square angle) (decf curly) (return-from func 0)))
;;                         ((char= char #\<) (incf angle))
;;                         ((char= char #\>)
;;                           (if (all-zero round square curly) (decf angle) (return-from func 0)))))
;;                 (format t "~a | (): ~a, []: ~a, {}: ~a, <>: ~a~%" string round square curly angle)
;;                 (if (all-zero round square curly angle) 1 0))))
;;           (str:lines input)))

;; Problem 20
(defun vowel-count (input)
  (mapcar (lambda (string)
            (loop :for vowel :across "aeiouy"
                  :sum (count vowel string)))
          (str:lines input)))

;; Problem 21
(defun array-counters (input)
  (let* ((n-counters (second input))
         (counters (make-array n-counters)))
    (loop :for value :in (cdr (cdr input))
          :do (incf (elt counters (1- value))))
    counters))

;; Problem 23
(defun bubble-in-array (input)
  (let ((result (copy-list input))
        (swaps 0))
    (loop :for i :from 0
          :for (a b) :on result :while b 
          :do (when (> a b)
                (incf swaps)
                (swap i (1+ i) result))
              (format t "~a, ~a | ~a~%" a b swaps))
    (format t "~a" result)
    (list swaps (array-checksum result))))

(defun swap (a-pos b-pos list)
  (let ((a (elt list a-pos))
        (b (elt list b-pos)))
    (setf (elt list a-pos) b
          (elt list b-pos) a)))

;; Problem 24
(defun p24 (input)
  (let ((output '()))
    (loop :for num :in input
          :do (push (neumanns-random-generator num) output))
    (reverse output)))

(defun neumanns-random-generator (seed-number)
  (let ((num seed-number)
        (steps 0)
        (sequence '()))
    (loop :until (find num sequence)
          :do (incf steps)
              (push num sequence)
              (setf num (generate-next-random num)))
    (format t "We began with ~a, looped after ~a steps, with ~a~%~%" seed-number steps num)
    steps))

(defun generate-next-random (num)
  (let ((str-num (make-n-digit-string 4 num))
        (square (make-n-digit-string 8 (* num num))))
    (format t "~a ^2 = ~a~%" str-num square)
    (middle-4-digits (parse-integer square))))

(defun middle-4-digits (num)
  (mod (truncate (/ num 100)) 10000))

(defun make-n-digit-string (n num)
  (let ((str-num (write-to-string num)))
    (str:concat (str:repeat (- n (length str-num)) "0") str-num)))

;; (defun middle-4-digits (num)
;;   (let* ((str-num (write-to-string num))
;;          (result (make-array 4 :adjustable t :fill-pointer 0)))
;;     (when (<= (length str-num) 4)
;;       (return-from middle-4-digits str-num))
;;     (loop :for digit :across str-num
;;           :for i :from 0 
;;           :do (when (and (> i 2) (< i (- (length str-num) 3)))
;;                 (vector-push-extend digit result)))
;;     (setf result (coerce result 'string))))

;; Problem 25
(defun p25 (input)
  (mapcar (lambda (l) (apply #'linear-congruential-generator l)) input))

(defun linear-congruential-generator (a c m initial n)
  (labels ((next (a c m num) (mod (+ (* a num) c) m)))
    (let ((current (next a c m initial)))
      (loop :for i :below (1- n)
            :do (format t "(~a * ~a + ~a) % ~a = " a current c m)
                (setf current (next a c m current))
                (format t "~a~%" current))
      (format t "FINAL: ~a~%~%" current)
      current)))

;; Problem 26
(defun p26 (input)
  (mapcar (lambda (args) (list (apply 'greatest-common-divisor args)
                               (apply 'least-common-multiple args)))
          input))

(defun greatest-common-divisor (a b)
  (loop :until (= a b)
        :do (if (> a b)
              (setf a (- a b))
              (setf b (- b a))))
  a)

(defun least-common-multiple (a b)
  (/ (* a b) (gcd a b)))

;; Problem 28
(defun bmi ())
