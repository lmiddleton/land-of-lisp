(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
    (lazy-cons (list nil
                     (game-tree (add-new-dice board player
                                              (1- spare-dice))
                                (mod (1+ player) *num-players*)
                                0
                                t))
               moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
                   (car (aref board pos)))
           (dice (pos)
                 (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
           (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst)
                                cur-player))
                       (> (dice src) (dice dst)))
                  (make-lazy
                   (list (list (list src dst)
                               (game-tree (board-attack board
                                                        cur-player
                                                        src
                                                        dst
                                                        (dice src))
                                          cur-player
                                          (+ spare-dice (dice dst))
                                          nil))))
                (lazy-nil)))
            (make-lazy (neighbors src)))
         (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
                    collect n)))))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
                          (unless (lazy-null moves)
                            (let* ((move (lazy-car moves))
                                   (action (car move)))
                              (fresh-line)
                              (format t "~a. " n)
                              (if action
                                  (format t "~a -> ~a" (car action) (cadr action))
                                (princ "end turn")))
                            (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
          (lazy-mapcar (lambda (move)
                         (list (car move)
                               (limit-tree-depth (cadr move) (1- depth))))
                       (caddr tree)))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

(defun score-board (board player)
  (loop for hex across board
      for pos from 0
      sum (if (eq (car hex) player)
              (if (threatened pos board)
                  1
                2)
            -1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
        do (let* ((nhex (aref board n))
                  (nplayer (car nhex))
                  (ndice (cadr nhex)))
             (when (and (not (eq player nplayer)) (> ndice dice))
               (return t))))))

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                 #'min)
               (get-ratings tree player))
      (score-board (cadr tree) player))))

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
              (unless (lazy-null moves)
                (let ((x (ab-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                  (if (>= x upper-limit)
                      (list x)
                    (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
              (unless (lazy-null moves)
                (let ((x (ab-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                  (if (<= x lower-limit)
                      (list x)
                    (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (if (eq (car tree) player)
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
          (apply #'min (ab-get-ratings-min tree
                                           player
                                           upper-limit
                                           lower-limit)))
      (score-board (cadr tree) player))))

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (car tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defparameter *cur-game-tree* nil)
(defparameter *from-tile* nil)

(defun dod-request-handler (path header params)
  (if (equal path "game.html")
      (progn (princ "<!doctype html>")
        (tag center ()
             (princ "Welcome to DICE OF DOOM!")
             (tag br ())
             (let ((chosen (assoc 'chosen params)))
               (when (or (not *cur-game-tree*) (not chosen))
                 (setf chosen nil)
                 (web-initialize))
               (cond ((lazy-null (caddr *cur-game-tree*))
                      (web-announce-winner (cadr *cur-game-tree*)))
                     ((zerop (car *cur-game-tree*))
                      (web-handle-human
                       (when chosen
                         (read-from-string (cdr chosen)))))
                     (t (web-handle-computer))))
             (tag br ())
             (draw-dod-page *cur-game-tree* *from-tile*)))
    (princ "Sorry... I don't know that page.")))

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))

(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
       (princ " play again")))

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from:"))
        ((eq pos 'pass) (setf *cur-game-tree*
                          (cadr (lazy-car (caddr *cur-game-tree*))))
         (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
              (princ "continue")))
        ((not *from-tile*) (setf *from-tile* pos)
         (princ "Now choose a destination:"))
        ((eq pos *from-tile*) (setf *from-tile* nil)
         (princ "Move cancelled."))
        (t (setf *cur-game-tree*
             (cadr (lazy-find-if (lambda (move)
                                   (equal (car move)
                                          (list *from-tile* pos)))
                                 (caddr *cur-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
                (princ "pass"))
           (princ " or make another move:"))))

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
       (princ
        "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))

(defun draw-dod-page (tree selected-tile)
  (svg *board-width*
       *board-height*
       (draw-board-svg (cadr tree)
                       selected-tile
                       (take-all (if selected-tile
                                     (lazy-mapcar
                                      (lambda (move)
                                        (when (eql (caar move)
                                                   selected-tile)
                                          (cadar move)))
                                      (caddr tree))
                                   (lazy-mapcar #'caar (caddr tree)))))))