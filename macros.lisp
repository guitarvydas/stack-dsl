(in-package :stack-dsl)

;;
;; There is no need to read the actual Lisp macros below.
;; The macros, in effect, rewrite text into other text (over-simplicfication).
;;
;; I list the effect of each macro, in loose terms, below...:

;; there are 2 stacks for every type
;;  [one stack is called "input" and the other is called "output" (corresponding to the input parameter list and return value)]

;; 6 operations on the 2 kinds of stacks:
;;
;; output
;; newscope
;; replace-top
;; append
;; pop-output
;; set-field

;; loosey-goosey meanings...
;;
;; output(type) : return value given by type of stack "input"
;; newscope(type) : push <nothing-ness> onto input stack of type
;; replace-top(x,y) : replace top x item with y
;; append(x,y) : append y to top x
;; pop-ouput(x) : kill top x item
;; set-field (x f val) : assign val to x.f

;; loosey-goosey semantics...
;;
;; output(type) : move top input item to top of output stack ; move top(type) from input-type to output-type, pop input-type
;; newscope(type) : push <nothing-ness> onto input stack of type
;; replace-top(x,y) : replace top x item with y ; set top(x) to be top(y) (replace, not push), pop top(y), check that replacement type is the same as the replacee
;; append(x,y) : append y to top x ; append top(y) to top(x), check that type of top(y) is append-able to top(x)
;; pop-ouput(x) : kill top x item ; return top(x), pop x
;; set-field (x f val) : top(x).f <- val where top(x) must have field f, val must be of type compatible with type top(x).f


;; I hard-code the symbols "self" and "env" ...
;; I use ~ and % as name prefixes for names.
;; ~ is used to prefix macro names.
;; % is used to prefix support routines.
;; (I could have used Common Lisp packages to qualify names instead of prefixes, but I thought that prefixes are less scary and show (visually) my design intentions).

(defun ~in(name) `,(intern (string-upcase (format nil "input-~a" name))))
(defun ~out(name) `,(intern (string-upcase (format nil "output-~a" name))))

(defmacro ~output (ty)
  `(progn 
     (%output (,(~in ty) (env self)) (,(~out ty) (env self)))
     (%pop (,(~out ty) (env self)))))

(defmacro ~newscope (ty)
  `(%push-empty (,(~in ty) (env self))))

(defmacro ~replace-top (ty1 ty2)
  `(let ((val (pop (,(~out ty2) (env self)))))
     (%check-type val (%type ,ty1))
     (%replace-top ,ty1 ,ty2)
     (%pop (,(~in ty2) (env self)))))

(defmacro ~append (stack1 stack2)
  `(progn
     (%check-appendable-type ,stack1)
     (%check-type (first ,stack2) (%type ,stack1))
     (stack-dsl::%append 
      (,(~in stack1) (env self))
      (,(~out stack2) (env self)))
     (%pop (,~out stack2) (env self))))

(defmacro ~set-field (to field-name from)
  ;; set to.f := from, pop from
  `(let ((val (%pop (,~out from) (env self))))
     (%check-type 
      val 
      (%type (%get-field ',field-name ,(~in to) (env self))))
     (%set-field ',field-name ,(~in to)(env self)) val))

