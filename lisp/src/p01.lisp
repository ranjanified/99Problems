(in-package :99-lisp)

;; P23 Extract a given number of randomly selected elements from a list
;; The selected items shall be returned in a list
;; (defun extract (l n)
;;   "Extract a given number of randomly selected elements from a list"
;;   )

;; P22 Create a list containing all integers within a given range
(defun range (m n)
  "Create a list containing all integers within a given range"
  (cond
    ((< n m) (reverse-list (range n m)))
    ((= m n) (cons m '()))
    ((cons m (range (1+ m) n)))))

;; P21 Insert an element at a given position into a list
(defun insert-at (item into at)
  "Insert an element at a given position into a list"
  (cond
    ((null into) (cons item nil))
    ((= at 1) (cons item into))
    ((cons (car into) (insert-at item (cdr into) (1- at))))))

;; P20 Remove the K'th element from a list
(defun remove-at (l n)
  "Remove the K'th element from a list"
  (cond
    ((null l) nil)
    ((= n 1) (cdr l))
    ((cons (car l) (remove-at (cdr l) (1- n))))))

;; P19 Rotate a list N places to the left
(defun rotate (l n)
  "Rotate a list N places to the left"
  (if (> n 0)
      (let ((s (split l n))) (append (cadr s) (car s)))
      (let ((rs (split (reverse-list l) (abs n)))) (append (reverse-list (car rs)) (reverse-list (cadr rs))))))

;; P18 Extract a slice from a list.
(defun slice (l i k)
  "Extract a slice from a list."
  (cond
    ((null l) nil)
    ((= i 1) (car (split l k)))
    ((slice (cdr l) (1- i) (1- k)))))

;; P17 Split a list into two parts; the length of the first part is given
(defun split (l n)
  "Split a list into two parts; the length of the first part is given"
  (cond
    ((null l) nil)
    ((= n 0) (cond ((atom (car l)) l)
		   ((list (reverse-list (car l)) (cdr l)))))
    ((atom (car l)) (split (cons (cons (car l) '()) (cdr l)) (1- n)))
    ((cond
       ((null (cadr l)) (list (reverse-list (car l)) '()))
       ((split (cons (cons (cadr l) (car l)) (cddr l)) (1- n)))))))

;; P16 Drop every N'th element from a list
(defun drop (l n)
  "Drop every N'th element from a list"
  (labels ((drop-nth (l c)
	     (cond
	       ((null l) nil)
	       ((= c 1) (drop-nth (cdr l) n))
	       ((cons (car l) (drop-nth (cdr l) (1- c)))))))
    (drop-nth l n)))

;; P15 Replicate the elements of a list a given number of times
(defun replicate (l n)
  "Replicate the elements of a list a given number of times"
  (labels ((rep-n (a i c)
	     (cond
	       ((= c 0) a)
	       ((rep-n (cons i a) i (1- c))))))
    (apply #'append (mapcar (lambda (x) (rep-n '() x n)) l))))

;; P14 Duplicate the elements of a list
(defun duplicate (l)
  (replicate l 2)
  ;; (cond
  ;;   ((null l) nil)
  ;;   ((cons (car l) (cons (car l) (duplicate (cdr l))))))
  )

;; P13 Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X
(defun run-encode-direct (l)
  "Run-length encoding of a list (direct solution)"
  (cond
    ((null l) nil)
    ((null (cdr l)) (reverse-list (car l)))
    ((atom (car l)) (run-encode-direct (cons (list (car l)) (cdr l))))
    ((cond
       ((atom (caar l)) (if (equal (caar l) (cadr l))
				   (run-encode-direct (cons (cons (list 2 (caar l)) (cdar l)) (cddr l)))
				   (run-encode-direct (cons (cons (cadr l) (car l)) (cddr l)))))
       ((if (equal (cadr (caar l)) (cadr l))
		 (run-encode-direct (cons (cons (list (1+ (caaar l)) (cadr l)) (cdar l)) (cddr l)))
		 (run-encode-direct (cons (cons (cadr l) (car l)) (cddr l)))))))))

;; P12 Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
(defun run-decode (l)
  "Decode a run-length encoded list"
  (cond
    ((null l) nil)
    ((atom (car l)) (cons (car l) (run-decode (cdr l))))
    ((= (caar l) 0) (run-decode (cdr l)))
    ((run-decode (cons (cadr (car l))
		       (cons (list (1- (caar l))
				   (cadr (car l)))
			     (cdr l)))))))

;; P11 Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
(defun run-encode-modified (l)
  "Modified run-length encoding"
  (mapcar (lambda (x)
	    (cond
	      ((> (count-items x) 1) (list (count-items x) (car x)))
	      ((car x))))
	  (pack l)))


;; P10 Run-length encoding of a list
(defun run-encode (l)
  "Run-length encoding of a list"
  (mapcar (lambda (x) (list (count-items x) (car x))) (pack l)))

;; P09 Pack consecutive duplicates of list elements into sublists
(defun pack (l)
  "Pack consecutive duplicates of list elements into sublists"
  (labels ((pack-aux (l acc)
	     (cond
	       ((null l) (reverse-list acc))
	       ((equal (car l) (caar acc)) (pack-aux (cdr l) (cons (cons (car l) (car acc)) (cdr acc))))
	       ((pack-aux (cdr l) (cons (list (car l)) acc))))))
    (pack-aux l '())))

;; P08 Eliminate consecutive duplicates of list elements
(defun compress (l)
  "Eliminate consecutive duplicates of list elements"
  (cond
    ((null l) nil)
    ((equal (car l) (cadr l)) (compress (cdr l)))
    ((cons (car l) (compress (cdr l))))))

;; P07 Flatten a nested list structure.
(defun flatten (l)
  "Flatten a nested list structure."
  (cond
    ((null l) nil)
    ((atom (car l)) (cons (car l) (flatten (cdr l))))
    ((append (flatten (car l)) (flatten (cdr l))))))

;; P06 Find out whether a list is a palindrome.
(defun palindrome (l)
  "Find out whether a list is a palindrome."
  (labels ((palindrome-eq (l r-l)
	     (cond
	       ((null l) (null r-l))
	       ((equal (car l) (car r-l)) (palindrome-eq (cdr l) (cdr r-l)))
	       (nil))))
    (palindrome-eq l (reverse-list l))))

;; P05 Reverse a list
(defun reverse-list (l) 
  (labels ((rev-aux (l aux)
	     (cond
	       ((null l) aux)
	       ((rev-aux (cdr l)(cons (car l) aux))))))
    (rev-aux l (list))))

;; P04 Find the number of elements of a list.
(defun count-items (l)
  (cond
    ((null l) 0)
    ((1+ (count-items (cdr l))))))

;; P03 Find the K'th element of a list
(defun k-th (l k)
  "Find the K'th element of a list"
  (cond
    ((null l) nil)
    ((= k 1) (car l))
    ((k-th (cdr l) (1- k)))))

;; P02 Find the last but one box of a list.
(defun last-but-one-box (l)
  "Find the last but one box of a list."
  (cond
    ((null l) nil)
    ((null (cdr (cdr l))) l)
    ((last-but-one-box (cdr l)))))

;; P01
(defun last-box (l)
  "Find the last box of a list."
  (cond
    ((null l) nil)
    ((null (cdr l)) l)
    ((last-box (cdr l)))))
