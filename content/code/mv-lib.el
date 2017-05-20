;;; -*- lexical-binding: t -*-

(defconst mv--max-count 10) ;; Arbitrary limit

(defun mv--var (index)
  "Get return value variable symbol by INDEX"
  (when (>= index mv--max-count)
    (error "Index %d is too high (%d is max)" index (1- mv--max-count)))
  (intern (format "mv--%d" index)))

(dotimes (i mv--max-count)
  (eval `(defvar ,(mv--var i) nil)))

(defmacro mv-ret (&rest xs)
  "Return multiple values from a function. 
Results can be used using `mv-let' macro."
  (let ((forms nil)
        (values (cdr xs))
        (i 0))
    (dolist (value values)
      (push `(setq ,(mv--var i) ,value) forms)
      (setq i (1+ i)))
    `(progn
       ,@(nreverse forms)
       ,(car xs))))

(defmacro mv-let (name-list mv-expr &rest body)
  "Call MV-EXPR and bind each returned value to the corresponding
symbol in NAME-LIST. Bound variables are visible for each form inside BODY."
  (declare (indent 2))
  (let ((forms nil)
        (i 0))
    (push `(,(pop name-list) ,mv-expr) forms)
    (dolist (name name-list)
      (push `(,name ,(mv--var i)) forms)
      (setq i (1+ i)))
    `(let ,(nreverse forms)
       ,@body)))

;; (defun test-3 (a b c)
;;   (mv-ret c b a))

;; (mv-let (a b c) (test-3 1 2 3)
;;   (format "%d %d %d" a b c)) ;; => "3 2 1"

;; (let ((lexical-binding t))
;;   (benchmark-run-compiled 1000000
;;     (mv-let (a b c) (mv-ret 1 2 3)
;;       (ignore a b c))))
