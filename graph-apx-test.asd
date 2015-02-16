;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :graph-apx-test
  :name "graph-apx-test"
  :description "Tests for graph-apx"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :depends-on (:graph-apx :myam :alexandria))

(defmethod perform ((op test-op)
                    (system (eql (find-system :graph-apx-test))))
  (perform op (find-system :graph-apx)))
