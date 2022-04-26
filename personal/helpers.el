(defun join-symbols (&rest symbols)
  (make-symbol (apply 's-concat (mapcar 'symbol-name symbols))))


(defmacro redef-internal (func func-to-redef redef)
  "For the duration of FUNC, redefine FUNC-TO-REDEF to REDEF."
  `(defadvice ,func-symbol (around ,(join-symbols 'my- func-symbol) activate)
     (let (orig)
       (fset 'orig (symbol-function (quote ,func-to-redef)))
       (fset (quote ,func-to-redef) ,redef)
       (unwind-protect
           ad-do-it
         (fset (quote ,func-to-redef) (symbol-function 'orig))))))

(defun line-length ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((init (point)))
      (end-of-line)
      (- (point) init))))

(provide 'helpers)
