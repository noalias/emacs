;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'cl-generic)

;; Type
(defun cl-struct-type (inst)
  (if (recordp inst)
      (aref inst 0)
    (elt inst 0)))

;; Serde
(defun cl-struct-serde-to-plist (x)
  (let* ((ty (if (recordp x) (aref x 0) (elt x 0)))
         (slots (cl-struct-slot-info ty)))
    (cl-reduce (pcase-lambda (plist `(,slot . ,val))
                 (if (eq slot 'cl-tag-slot)
                     (plist-put plist
                                (intern ":type") ty)
                   (plist-put plist
                              (intern (concat ":" (symbol-name slot)))
                              (or (cl-struct-slot-value ty slot x)
                                  val))))
               slots
               :initial-value '())))

(provide 'cl-struct-tools)
