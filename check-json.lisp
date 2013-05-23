;;;; check-json.lisp

(in-package #:check-json)

;;; Number - :number
;;; String - :string
;;; Boolean - :bool
;;; Array - (:array (:exact t) type...)
;;; Object - (:hash (:exact t) (key1 . type1)...)

(defun read-bool (str)
  (check-type str string)
  (cond
    ((equal str "false") 'json-bool:false)
    ((equal str "true") 'json-bool:true)
    (t (error "Unknown boolean token ~S" str))))

(defun check-typespec (obj spec)
  (check-type spec (or keyword list))
  (if (keywordp spec)
      (check-obj-type obj spec '())
      (check-obj-type obj (first spec) (rest spec))))

(defgeneric check-obj-type (json-obj type type-args)
  (:documentation "Return T if JSON-OBJ is of the type described by SCHEMA."))

(defmethod check-obj-type (json-obj (type (eql :number)) type-args)
  (check-type type-args null)
  (numberp json-obj))

(defmethod check-obj-type (json-obj (type (eql :string)) type-args)
  (check-type type-args null)
  (stringp json-obj))

(defmethod check-obj-type (json-obj (type (eql :bool)) type-args)
  (check-type type-args null)
  (or (eq json-obj 'json-bool:true)
      (eq json-obj 'json-bool:false)))

(defmethod check-obj-type (json-obj (type (eql :array)) type-args)
  (check-type type-args list)
  (destructuring-bind ((&key exact) &rest item-types) type-args
    (and (typep json-obj 'vector)
         (every #'check-typespec json-obj item-types)
         (if exact
             (= (length item-types) (length json-obj))
             t))))



;;; "check-json" goes here. Hacks and glory await!
