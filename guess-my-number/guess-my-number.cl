;; Land of Lisp
;; Chapter 2
;; The Guess-My-Number Game

;; define small and big variables
;; keep track of smallest and biggest numbers the guess can be
(defparameter *small* 1)
(defparameter *big* 100)

;; generates a guess of the player's number
;; add high and low limits, halve the sum and shorten the result
;; this is call the arithmetic shift, allowing us to do binary search
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller()
  ;; change value of global var big to 1 less than the last guess
  (setf *big* (1- (guess-my-number)))
  ;; show a new guess, calculated using updated value of big
  (guess-my-number))

(defun bigger()
  ; change value of global var small to 1 more than the last guess
  (setf *small* (1+ (guess-my-number)))
  ;; show a new guess, calculated using updated value of small
  (guess-my-number))

;; resets the global variables and restarts the game
(defun start-over()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))