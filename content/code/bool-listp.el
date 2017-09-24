;;; -*- lexical-binding: t -*-

;; Requires `%return' intrinsic that is described in
;; https://quasilyte.github.io/blog/post/writing-emacs-lisp-compiler-intrinsics/

(defun bool-listp/ret (xs)
  (dolist (x xs)
    (unless (booleanp x)
      (%return nil)))
  t)

(disassemble 'bool-listp/ret)
;; 0       dup       
;; 1:1     dup       
;; 2       goto-if-nil 3
;; 5       dup       
;; 6       car       
;; 7       constant  booleanp
;; 8       stack-ref 1
;; 9       call      1
;; 10      goto-if-not-nil 2
;; 13      constant  nil
;; 14      return    
;; 15:2    stack-ref 1
;; 16      cdr       
;; 17      discardN-preserve-tos 2
;; 19      goto      1
;; 22:3    discard   
;; 23      constant  t
;; 24      return    


(defun bool-listp (xs)
  (let ((x nil)
        (ret t))
    (while (and (setq x (pop xs))
                ret)
      (unless (booleanp x)
        (setq ret nil)))
    ret))

(disassemble 'bool-listp)
;; 0       constant  nil
;; 1       constant  t
;; 2:1     stack-ref 2
;; 3       dup       
;; 4       cdr       
;; 5       stack-set 4
;; 7       car-safe  
;; 8       dup       
;; 9       stack-set 3
;; 11      goto-if-nil 2
;; 14      dup       
;; 15      goto-if-nil 2
;; 18      constant  booleanp
;; 19      stack-ref 2
;; 20      call      1
;; 21      goto-if-not-nil 1
;; 24      constant  nil
;; 25      stack-set 1
;; 27      goto      1
;; 30:2    return    
