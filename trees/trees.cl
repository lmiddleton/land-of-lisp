(defparameter *velvet-sky* '(22 34 82))

(defun composition ()
  (with-open-file (comp-stream "composition.svg" :direction :output)
    (print (draw-comp) comp-stream)))

(defun draw-comp ()
  (svg
   (fresh-line)
   (velvet-sky 500 300 250)
   (fresh-line)
   (green-grid 25 35 10 100)
   (fresh-line)))

(defun 500-square (color)
  (let ((points (list  '(0 . 0) '(500 . 0) '(500 . 500) '(0 . 500))))
    (polygon points color)))

(defun 10-square (color)
  (let ((points (list  '(0 . 0) '(10 . 0) '(10 . 10) '(0 . 10))))
    (polygon points color)))

(defun square (color length center)
  (let* ((centerx (car center))
         (centery (cadr center))
         (radius (/ length 2))
         (points (list  
                  (cons (- centerx radius) (- centery radius)) 
                  (cons (+ centerx radius) (- centery radius)) 
                  (cons (+ centerx radius) (+ centery radius)) 
                  (cons (- centerx radius) (+ centery radius)))))
    (polygon points color)))

(defun stars (num fieldsize offset)
  (loop for i below num
      do
        (let ((center (list (random-from-range offset (+ fieldsize offset)) (random-from-range offset (+ fieldsize offset)))))
          (fresh-line)
          (circle center (random 5) '(255 255 255)))))

(defun draw-velvet-sky (size starnum offset)
  (svg
   (fresh-line)
   (velvet-sky size starnum offset)))

(defun velvet-sky (size starnum offset)
  (let ((center (list (+ offset (/ size 2)) (+ offset (/ size 2)))))
    (square *velvet-sky* size center))
  (fresh-line)
  (stars starnum size offset))

(defun draw-green-grid (height width size offset)
  (svg
   (fresh-line)
   (green-grid height width size offset)))

(defun green-grid (height width size offset)
  (let ((square-len size)
        (start-green (random-green)))
    (loop for i below width
        do
          (loop for j below height
              do
                (let ((center (list (+ offset (* i square-len)) (+ offset (* j square-len)))))
                  (square (adjust-green start-green) square-len center))
                (fresh-line)))))

(defun square-points (center length)
  (let* ((centerx (car center))
         (centery (cadr center))
         (point1 '(centerx . nil)))
    (list '() )))

(defun adjust-green (color)
  (list (+ (car color) (random 30)) (+ (cadr color) (random 30)) (+ (caddr color) (random 30))))

(defun draw-green-circles (num)
  (svg
   (loop for i below num
       do 
         (let* ((radius 5)
                (center (list (+ (* i (* radius 2)) radius) radius)))
           (fresh-line)
           (circle center radius (random-green))))))

(defun random-r ()
  (random 100))

(defun random-g ()
  (random 256))

(defun random-b ()
  (random 256))

(defun random-green ()
  (list (random-r) (random-g) (random-b)))

(defun random-green-2 ()
  (let ((first (random 3)))
    (print first)
    (fresh-line)
    (case first
      ((0) (adjust-rgb (list (random-r) 0 0)))
      ((1) (print '(it's green)))
      ((2) (print '(it's blue)))
      )
    ))

(defun adjust-rgb (rgb)
  (let ((r (car rgb))
        (g (cadr rgb))
        (b (caddr rgb)))
    (print r)
    (print g)
    (print b)
    (if (> r 100)
        (adjust-rgb '(100 g b))
      (if ()))))

;; from land of lisp
(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ,@body))

;; from land of lisp
(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                               (pairs atts)))
                     nil)
     ,@body
     (print-tag ',name nil t)))

;; from land of lisp
(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
    alst)
  (princ #\>))

;; from land of lisp
(defun circle (center radius color)
  (fresh-line)
  (tag circle (cx (car center)
                  cy (car (cdr center))
                  r radius
                  style (svg-style color))))

;; from land of lisp
(defun polygon (points color)
  (tag polygon (points (format nil
                           "~{~a,~a ~}"
                         (mapcan (lambda (tp)
                                   (list (car tp) (cdr tp)))
                           points))
                       style (svg-style-2 color))))

;; from land of lisp
(defun svg-style (color)
  (format nil
      "~{fill:rgb(~a, ~a, ~a);stroke:rgb(~a,~a,~a)~}"
    (append color
            (brightness color -100))))

;; modified from land of lisp
(defun svg-style-2 (color)
  (format nil
      "~{fill:rgb(~a, ~a, ~a);stroke:rgb(~a,~a,~a)~}"
    (append color
            color)))

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

;; from land of lisp
(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                     (if tail
                         (f (cdr tail) (cons (cons head (car tail)) acc))
                       (reverse acc))
                     (reverse acc))))
    (f lst nil)))

;; from land of lisp
(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                 ,no))))

;; from land of lisp
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; from land of lisp
(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
    col))