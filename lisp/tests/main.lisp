(defpackage 99-lisp/tests/main
  (:use :cl
        :99-lisp
        :fiveam))
(in-package :99-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :99-lisp)' in your Lisp.

(test combination
  (is-true (null (combination 8 '())))
  (is-true (equal (combination 1 '(a b c d e f g))
		  '((a) (b) (c) (d) (e) (f) (g))))
  (is-true (equal (combination 2 '(a b c d e f g))
		  '((A B) (A C) (A D) (A E) (A F) (A G) (B C) (B D) (B E) (B F) (B G) (C D) (C E)
						     (C F) (C G) (D E) (D F) (D G) (E F) (E G) (F G))))
  (is-true (equal (combination 3 '(a b c d e f g))
		  '((A B C) (A C D) (A D E) (A E F) (A F G) (B C D) (B D E) (B E F) (B F G)
						     (C D E) (C E F) (C F G) (D E F) (D F G) (E F G))))
  (is-true (equal (combination 4 '(a b c d e f g))
		  '((A B C D) (A C D E) (A D E F) (A E F G) (B C D E) (B D E F) (B E F G)
						     (C D E F) (C E F G) (D E F G))))
  (is-true (equal (combination 5 '(a b c d e f g))
		  '((A B C D E) (A C D E F) (A D E F G) (B C D E F) (B D E F G) (C D E F G))))
  (is-true (equal (combination 6 '(a b c d e f g))
		  '((A B C D E F) (A C D E F G) (B C D E F G))))
  (is-true (equal (combination 7 '(a b c d e f g)) '((A B C D E F G))))
  (is-true (null (combination 8 '(a b c d e f g)))))

(test range
  (is-true (equal (range 1 1) '(1)))
  (is-true (equal (range 4 9) '(4 5 6 7 8 9)))
  (is-true (equal (range 9 4) '(9 8 7 6 5 4))))

(test insert-at
  (is-true (equal (insert-at 'xyz '() 3) '(xyz)))
  (is-true (equal (insert-at 'alfa '(a b c d) 2) '(a alfa b c d))))

(test remove-at
  (is-true (null (remove-at '() 4)))
  (is-true (equal (remove-at '(a b c d) 2) '(a c d))))

(test rotate
  (is-true (null (rotate '() 3)))
  (is-true (equal (rotate '(a b c d e f g h) 3) '(d e f g h a b c)))
  (is-true (equal (rotate '(a b c d e f g h) -2) '(g h a b c d e f))))

(test slice
  (is-true (null (slice '() 3 5)))
  (is-true (equal (slice '(a b c d e f g h i k) 3 7) '(c d e f g))))

(test split
  (is-true (null (split '() 3)))
  (is-true (equal (split '(a b c d e f g h i k) 3) '((a b c) (d e f g h i k)))))

(test drop
  (is-true (null (drop '() 5)))
  (is-true (equal (drop '(a b c d e f g h i k) 3) '(a b d e g h k) )))

(test replicate
  (is-true (null (replicate '() 5)))
  (is-true (equal (replicate '(a b c) 3) '(a a a b b b c c c))))

(test duplicate
  (is-true (null (duplicate '())))
  (is-true (equal (duplicate '(a b c c d)) '(a a b b c c c c d d))))

(test run-encode-direct
  (is-true (null (run-encode-direct '())))
  (is-true (equal (run-encode-direct '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))))

(test run-decode
  (is-true (null (run-decode '())))
  (is (equal (run-decode '((4 a) b (2 c) (2 a) d (4 e))) '(a a a a b c c a a d e e e e) )))

(test run-encode-modified
  (is-true (null (run-encode-modified '())))
  (is-true (equal (run-encode-modified '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))))

(test run-encode
  (is-true (null (run-encode '())))
  (is-true (equal (run-encode '(a a a a b c c a a d e e e e)) '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))))

(test pack
  (is-true (null (pack '())))
  (is-true (equal (pack '(a a a b b c c c)) '((a a a) (b b) (c c c))))
  (is-true (equal (pack '(a a a a b c c a a d e e e e)) '((a a a a) (b) (c c) (a a) (d) (e e e e)))))

(test compress
  (is (null (compress '())))
  (is-true (equal (compress '(1)) '(1)))
  (is-true (equal (compress '(1 1)) '(1)))
  (is-true (equal (compress '(a a a a b c c a a d e e e e)) '(a b c a d e))))

(test flatten
  (is (null (flatten '())))
  (is (equal (flatten '(1 2 3)) '(1 2 3)))
  (is (equal (flatten '(a (b (c d) e))) '(a b c d e))))

(test palindrome
  (is-true (palindrome '()))
  (is-true (palindrome '(x a m a x)))
  (is-false (palindrome '(c a t))))

(test reverse-list
  (is (null (reverse-list '())))
  (is (equal (reverse-list '(1 2 3)) '(3 2 1)))
  (is (equal (reverse-list '(1 (2 3) 4)) '(4 (2 3) 1))))

(test last-but-one-box
  (is (null (last-but-one-box '())))
  (is (equal (last-but-one-box '(1 2 3 4 5)) '(4 5))))

(test last-box
  (is (null (last-box '())))
  (is (equal (last-box '(1 2 3)) '(3))))

(test k-th
  (is (null (k-th '() 1)))
  (is (equal (k-th '(a b c d e) 3) 'c))
  (is (null (k-th '(a b nil c) 3))))

(test count-items
  (is (= (count-items '()) 0))
  (is (= (count-items '(1 2 3)) 3))
  (is (= (count-items '(1 nil 2)) 3)))
