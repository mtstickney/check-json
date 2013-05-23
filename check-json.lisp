;;;; check-json.lisp

(in-package #:check-json)

;;; Number - :number
;;; String - :string
;;; Boolean - :bool
;;; Array - (:array (:exact t) type...)
;;; Object - (:hash (:exact t) (key1 . type1) key2...)

(defun read-bool (str)
  (check-type str string)
  (cond
    ((equal str "false") 'json-bool:false)
    ((equal str "true") 'json-bool:true)
    (t (error "Unknown boolean token ~S" str))))

(defmacro values-of (call)
  `(apply #'values (multiple-value-list ,call)))

(defun check-typespec (obj spec path)
  (check-type spec (or keyword list))
  (check-type path list)
  (if (keywordp spec)
      (values-of (check-obj-type obj spec '() path))
      (values-of (check-obj-type obj (first spec) (rest spec) path))))

(defun dict (&rest entries)
  (let* ((i (length entries))
         (m (make-hash-table :size i :test 'equal)))
    (loop for e on entries by #'cddr
       do (setf (gethash (first e) m)
                (second e)))
    m))

(defgeneric check-obj-type (json-obj type type-args path)
  (:documentation "Return T if JSON-OBJ is of the type described by SCHEMA."))

(defmethod check-obj-type :around (json-obj type type-args path)
  (check-type path list)
  (call-next-method json-obj type type-args path))

(defmethod check-obj-type (json-obj (type (eql :number)) type-args path)
  (check-type type-args null)
  (or (numberp json-obj)
      (values nil (list (dict :path path
                              :expected-type :number
                              :value json-obj)))))

(defmethod check-obj-type (json-obj (type (eql :string)) type-args path)
  (check-type type-args null)
  (or (stringp json-obj)
      (values nil (list (dict :path path
                              :expected-type :string
                              :value json-obj)))))

(defmethod check-obj-type (json-obj (type (eql :bool)) type-args path)
  (check-type type-args null)
  (or (eq json-obj 'json-bool:true)
      (eq json-obj 'json-bool:false)
      (values nil (list (dict :path path
                              :expected-type :bool
                              :value json-obj)))))

(defun map-if (func seq &rest seqs)
  (check-type func function)
  (check-type seq sequence)
  (loop for s in seqs do (check-type s sequence))

  (let ((args (apply #'mapcar #'list seq seqs))
        (results '()))
    (loop for a in args
       do (let ((result (apply func a)))
            (when result
              (push result results))))
    (nreverse results)))

(defmethod check-obj-type (json-obj (type (eql :array)) type-args path)
  (check-type type-args list)
  (destructuring-bind ((&key exact) &rest item-types) type-args
    (let ((errors '()))
      ;; Argh, this is getting into heuristic-land
      (unless (and (typep json-obj 'vector)
                   (not (typep json-obj 'string)))
        ;; Stop checking on a top-level type error
        (return-from check-obj-type
          (values nil
                  (list (dict :path path
                              :expected-type :array
                              :value json-obj)))))
      (loop for i from 0
         for error-lst in (map 'list
                               (lambda (elt type)
                                 (nth-value 1 (check-typespec elt type (cons i path))))
                               json-obj
                               item-types)
         do (when (not (endp error-lst))
              (setf errors (nconc error-lst errors))))
      (when exact
        (unless (= (length json-obj) (length item-types))
          (push (dict :path path
                      :expected-type `(:array ,(length item-types))
                      :value json-obj)
                errors)))
      (if (endp errors)
          t
          (values nil errors)))))

(defun validate-key-spec (obj key-spec)
  (flet ((find-key (key obj)
           (find-if (lambda (cell)
                      (equal (car cell) key))
                    obj)))
    (typecase key-spec
      (cons (let ((cell (find-key (car key-spec) obj)))
              (and cell
                   (check-typespec (cdr cell) (cdr key-spec)))))
      (otherwise (find-key key-spec obj)))))

(defmethod check-obj-type (json-obj (type (eql :hash)) type-args)
  (check-type type-args list)
  (destructuring-bind ((&key exact) &rest key-types) type-args
    (and (typep json-obj 'list)
         (every #'consp json-obj)
         (every (lambda (key-type)
                  (validate-key-spec json-obj key-type))
                key-types))))

(defun validate-json (json typespec)
  (let* ((cl-json:*json-array-type* 'vector)
         (cl-json:*boolean-handler* #'read-bool)
         (obj (cl-json:decode-json-from-string json)))
    (check-typespec obj typespec)))

;;; "check-json" goes here. Hacks and glory await!
