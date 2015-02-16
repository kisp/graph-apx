;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :graph-apx
  :name "graph-apx"
  :description "Read and write graphs in Aspartix format."
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
               (:file "package")
               (:file "graph-apx" :depends-on ("package"))
               )
  :depends-on (:alexandria :graph))

(defmethod perform ((op test-op)
                    (system (eql (find-system :graph-apx))))
  (oos 'load-op :graph-apx-test)
  (funcall (intern "RUN!" "MYAM") :graph-apx-test))
