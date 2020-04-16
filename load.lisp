;; for early debugging of .asd file

(uiop:run-program "rm -rf ~/.cache/common-lisp ; rm -rf */*.fasl ; rm -rf */*~")
(ql:quickload :alexandria)
(ql:quickload :loops )
(ql:quickload :parsing-assembler)
(ql:quickload :stack-dsl)
