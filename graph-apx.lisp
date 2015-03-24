;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :graph-apx)

(defgeneric read-apx (input))

(defmethod read-apx ((input string))
  (with-input-from-string (stream input)
    (read-apx stream)))

(defmethod read-apx ((input pathname))
  (with-open-file (stream input)
    (read-apx stream)))

(defun myr ()
  (let ((readtable (copy-readtable nil)))
    (set-macro-character #\. (lambda (stream char)
                               (declare (ignore stream char))
                               (values #\.)) t readtable)
    (set-macro-character #\, (lambda (stream char)
                               (declare (ignore stream char))
                               (values)) nil readtable)
    readtable))

(defun add-edge* (graph edge)
  (declare (optimize speed))
  (flet ((edge-eql (a b)
           (declare (list a b))
           (flet ((check (a b)
                    (typecase a
                      (symbol (eql a b))
                      (fixnum (eql a b))
                      (t (locally
                             (declare (optimize (speed 0)))
                           (eql a b))))))
             (and (check (first a) (first b))
                  (check (second a) (second b))))))
    (let ()
      (flet ((add (node)
               (add-node graph node)
               (let* ((hash (graph::node-h graph))
                      (edges (the list (gethash node hash))))
                 (locally (declare (inline member))
                   (unless (dolist (x edges nil)
                             (when (edge-eql edge x)
                               (return t)))
                     (push edge (gethash node hash)))))))
        (add (first edge))
        (add (second edge)))))
  (setf (gethash edge (graph::edge-h graph)) nil)
  edge)

(defmethod read-apx ((input stream))
  (let ((graph (make-instance 'digraph)))
    (let ((*readtable* (myr))
          (package (find-package :graph-apx)))
      (loop for form = (let ((*package* package))
                         (read input nil))
            while form
            do (let ((predicate form)
                     (args (read input)))
                 (ecase predicate
                   (arg (populate graph :nodes args))
                   (att (add-edge* graph args))))
            do (unless (eql #\. (read input))
                 (error "expected `.'"))))
    graph))

(defun write-apx-to-string (graph)
  (with-output-to-string (output)
    (write-apx graph output)))

(defun write-apx-to-file (graph pathname-designator
                          &key (if-exists :error))
  (with-open-file (stream pathname-designator
                          :direction :output
                          :if-exists if-exists)
    (write-apx graph stream)))

(defun write-apx (graph &optional stream)
  (let ((stream (cond
                  ((null stream) *standard-output*)
                  ((eql stream t) *terminal-io*)
                  (t stream))))
    (dolist (node (nodes graph))
      (format stream "arg(~A).~%" node))
    (dolist (edge (edges graph))
      (format stream "att(~A,~A).~%" (first edge) (second edge)))))
