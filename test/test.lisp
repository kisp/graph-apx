;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :graph-apx-test)

(defsuite* :graph-apx-test)

(deftest dummy
  (is (= 1 1)))

(defun lines (string)
  (with-input-from-string (in string)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun sequal (a b)
  (set-equal a b :test #'equal))

(defun apx-equal (a b)
  (is (sequal (lines a) (lines b))))

(deftest test.1
  (let ((graph (read-apx (format nil "arg(1).~%"))))
    (is (equal '(1) (nodes graph)))
    (is (equal nil (edges graph)))))

(deftest test.2
  (let ((graph (read-apx (format nil "arg(1).~%arg(2).~%"))))
    (is (sequal '(1 2) (nodes graph)))
    (is (sequal nil (edges graph)))))

(deftest test.3
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "arg(a).~%arg(b).~%arg(c).~%"))))
    (is (sequal '(a b c) (nodes graph)))
    (is (sequal nil (edges graph)))))

(deftest test.4
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "arg(a).~%arg(b).~%arg(c).~%att(a,b).~%"))))
    (is (sequal '(a b c) (nodes graph)))
    (is (sequal '((a b)) (edges graph)))))

(deftest test.5
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "arg(a).~%arg(b).~%arg(c).~%att(b,a).~%"))))
    (is (sequal '(a b c) (nodes graph)))
    (is (sequal '((b a)) (edges graph)))))

(deftest test.6
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "arg(a).~%arg(b).~%arg(c).~%att(b, a).~%att(a, c).~%"))))
    (is (sequal '(a b c) (nodes graph)))
    (is (sequal '((b a) (a c)) (edges graph)))))

(deftest test.7
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "arg(a).~%arg(b).~%att(b, a).~%att(a, c).~%"))))
    (is (sequal '(a b c) (nodes graph)))
    (is (sequal '((b a) (a c)) (edges graph)))))

(deftest test.8
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "att(a, b).~%att(a, c).~%"))))
    (is (sequal '(a b c) (nodes graph)))
    (is (sequal '((a b) (a c)) (edges graph)))))

(deftest test.9
  (signals error (read-apx (format nil "1~%"))))

(deftest test.9b
  (signals error (read-apx (format nil "att(1)~%"))))

(deftest test.9c
  (signals error (read-apx (format nil "att(1)x"))))

(deftest test.10
  (let* ((*package* (find-package :graph-apx-test))
         (graph (read-apx (format nil "att(a, c).~%att(b, c).~%att(c, d).~%"))))
    (is (equal '(b a) (precedents graph 'c)))))

(deftest test.11
  (let ((graph (populate (make-instance 'digraph) :edges '((1 2) (2 3)))))
    (is (apx-equal (format nil "arg(1).~%arg(2).~%arg(3).~%att(1,2).~%att(2,3).~%")
                   (write-apx-to-string graph)))))

(deftest test.12
  (let ((graph (populate (make-instance 'digraph) :nodes '(1 2 3))))
    (is (apx-equal (format nil "arg(1).~%arg(2).~%arg(3).~%")
                   (write-apx-to-string graph)))))

(deftest test.13
  (let ((graph (populate (make-instance 'digraph))))
    (is (equal (format nil "")
               (write-apx-to-string graph)))))
