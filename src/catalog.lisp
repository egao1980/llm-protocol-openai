(in-package #:llm-protocol-openai)

(defparameter +default-local-model-flags+
  '(("nemotron-3-nano-4b" :min-completion-tokens 128)
    ("zai-org/glm-4.6v-flash" :min-completion-tokens 128
     :reasoning-as-content t))
  "Built-in openai-compat flags for local LM Studio / llama.cpp ids.
   Not llm-model-info slots — those stay in llm-protocol.")

(defun copy-default-model-catalog ()
  (copy-tree +default-local-model-flags+))

(defun %catalog-str (x)
  (cond
    ((or (null x) (eq x :null)) "")
    ((stringp x) x)
    ((symbolp x) (string-downcase (symbol-name x)))
    (t (princ-to-string x))))

(defun %normalize-model-id (model)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (%catalog-str model)))

(defun %ids-match-p (catalog-id requested)
  (let ((a (%normalize-model-id catalog-id))
        (b (%normalize-model-id requested)))
    (or (equal a b)
        (equalp a b)
        (let ((slash (position #\/ b :from-end t)))
          (and slash (plusp (length a)) (equalp a (subseq b (1+ slash)))))
        (let ((slash (position #\/ a :from-end t)))
          (and slash (plusp (length b)) (equalp (subseq a (1+ slash)) b))))))

(defun %hash-to-plist (table)
  (let ((out nil))
    (maphash (lambda (k v)
               (setf out (list* (if (keywordp k)
                                    k
                                    (intern (string-upcase (%catalog-str k)) :keyword))
                                v
                                out)))
             table)
    out))

(defun %entry-plist (row)
  "Normalize a catalog row or value to a flag plist."
  (cond
    ((null row) nil)
    ((hash-table-p row) (%hash-to-plist row))
    ((and (consp row) (keywordp (car row))) row)
    ((consp row)
     (let ((rest (cdr row)))
       (cond
         ((hash-table-p rest) (%hash-to-plist rest))
         ((and (consp rest) (keywordp (car rest))) rest)
         ((and (consp rest) (null (cdr rest))
               (or (hash-table-p (car rest))
                   (and (consp (car rest)) (keywordp (caar rest)))))
          (%entry-plist (car rest)))
         (t rest))))
    (t nil)))

(defun lookup-model-flags (catalog model)
  "Plist of per-model flags for MODEL in CATALOG (alist or hash), or NIL."
  (cond
    ((null catalog) nil)
    ((hash-table-p catalog)
     (or (%entry-plist (gethash (%normalize-model-id model) catalog))
         (let ((found nil))
           (maphash (lambda (k v)
                      (when (and (not found) (%ids-match-p k model))
                        (setf found (%entry-plist v))))
                    catalog)
           found)))
    ((consp catalog)
     (%entry-plist (find-if (lambda (e)
                              (and (consp e) (%ids-match-p (car e) model)))
                            catalog)))
    (t nil)))

(defun model-catalog-flag (catalog model flag &optional default)
  (let ((plist (lookup-model-flags catalog model)))
    (if (eq (getf plist flag :%absent) :%absent)
        default
        (getf plist flag))))
