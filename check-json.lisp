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
    ((equal str "null") nil)
    (t (error "Unknown boolean token ~S" str))))

(defun check-typespec (obj spec path)
  (check-type spec (or keyword list))
  (check-type path list)
  (if (keywordp spec)
      (obj-type-errors obj spec '() path)
      (obj-type-errors obj (first spec) (rest spec) path)))

(defun dict (&rest entries)
  (let* ((i (length entries))
         (m (make-hash-table :size i :test 'equal)))
    (loop for e on entries by #'cddr
       do (setf (gethash (first e) m)
                (second e)))
    m))

(defgeneric obj-type-errors (json-obj type type-args path)
  (:documentation "Return a list of errors when validating JSON-OBJ against TYPE."))

(defmethod obj-type-errors :around (json-obj type type-args path)
  (check-type path list)
  (call-next-method json-obj type type-args path))

;; Wildcard, always true
(defmethod obj-type-errors (json-obj (type (eql :*)) type-args path)
  (check-type type-args null)
  '())

(defmethod obj-type-errors (json-obj (type (eql :number)) type-args path)
  (check-type type-args null)
  (if (not (numberp json-obj))
      (list (dict :path path
                  :error-type :wrong-type
                  :expected :number
                  :got json-obj))
      '()))

(defmethod obj-type-errors (json-obj (type (eql :string)) type-args path)
  (check-type type-args null)
  (if (not (stringp json-obj))
      (list (dict :path path
                  :error-type :wrong-type
                  :expected :string
                  :got json-obj))
      '()))

(defmethod obj-type-errors (json-obj (type (eql :bool)) type-args path)
  (check-type type-args null)
  (if (or (eq json-obj 'json-bool:true)
          (eq json-obj 'json-bool:false))
      '()
      (list (dict :path path
                  :error-type :wrong-type
                  :expected :bool
                  :got json-obj))))

(defmethod obj-type-errors (json-obj (type (eql :null)) type-args path)
  (check-type type-args null)
  (if (not (null json-obj))
      (list (dict :path path
                  :error-type :wrong-type
                  :expected :null
                  :got json-obj))))

(defmethod obj-type-errors (json-obj (type (eql :array)) type-args path)
  (check-type type-args list)
  (destructuring-bind ((&key exact) &rest item-types) type-args
    (cond
      ;; Need to separate vectors and strings here, despite the subtype
      ((or (not (typep json-obj 'vector))
           (typep json-obj 'string))
       (list (dict :path path
                   :error-type :wrong-type

                   :expected :array
                   :got json-obj)))
      ((> (length item-types) (length json-obj))
       (list (dict :path path
                   :error-type :wrong-length
                   :expected (length item-types)
                   :got (length json-obj))))
      (t (let ((item-errors (reduce #'append
                                    (map 'list
                                         #'check-typespec
                                         json-obj
                                         item-types
                                         (loop for i from 0 to (length json-obj)
                                            collect (cons i path))))))
           (if (and exact (not (= (length json-obj) (length item-types))))
               (cons (dict :path path
                           :error-type :trailing-elements)
                     item-errors)
               item-errors))))))

(defun key-spec-key (key-spec)
  (typecase key-spec
    (cons (car key-spec))
    (otherwise key-spec)))

;; Note: path is the path to the hash, not the key
(defun key-spec-errors (obj key-spec path)
  (flet ((find-key (key obj)
           (find-if (lambda (cell)
                      (equal (car cell) key))
                    obj)))
    (let* ((key (key-spec-key key-spec))
           (cell (find-key key obj)))
      (cond
        ((null cell)
         (list (dict :path path
                     :error-type :missing-key
                     :key key)))
        ((consp key-spec)
         (check-typespec (cdr cell) (cdr key-spec) (cons key path)))
        (t t)))))

(defmethod obj-type-errors (json-obj (type (eql :hash)) type-args path)
  (check-type type-args list)
  (destructuring-bind ((&key exact) &rest key-types) type-args
    (cond
      ((or (not (typep json-obj 'list))
           (notevery #'consp json-obj))
       (list (dict :path path
                   :error-type :wrong-type
                   :expected :hash
                   :got json-obj)))
      (t (let ((item-errors (reduce #'append
                                    (map 'list
                                         (lambda (key-type)
                                           (key-spec-errors json-obj key-type path))
                                         key-types))))
           (if exact
               (let ((obj-keys (remove-duplicates json-obj
                                                  :test #'equal
                                                  :key #'car))
                     (type-keys (remove-duplicates key-types
                                                   :test #'equal
                                                   :key #'key-spec-key)))
                 (if (= (length obj-keys) (length type-keys))
                     item-errors
                     (cons (dict :path path
                                 :error-type :extra-keys
                                 :expected (mapcar #'key-spec-key type-keys)
                                 :got (mapcar #'car obj-keys))
                           item-errors)))
               item-errors))))))


(defun validate-json (json typespec)
  (let* ((cl-json:*json-array-type* 'vector)
         (cl-json:*boolean-handler* #'read-bool)
         (obj (cl-json:decode-json-from-string json))
         (errors (check-typespec obj typespec '())))
    (if (endp errors)
        t
        (values nil (nreverse errors)))))

;;; "check-json" goes here. Hacks and glory await!
