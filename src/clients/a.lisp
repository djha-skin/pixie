(defclass consumer
  ((coercer
     :documentation
     "Coerce this thing using this function."
     :type 'function
     )
   (children
      :documentation
      "Function that takes a key and returns a consumer."
      :type 'function))
    (defun objectify (table 