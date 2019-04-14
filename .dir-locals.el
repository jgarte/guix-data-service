;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (fill-column . 78)
  (tab-width . 8))
 (scheme-mode
  (indent-tabs-mode)
  (eval put 'with-time-logging 'scheme-indent-function 1)
  (eval put 'make-parameter 'scheme-indent-function 1))
 (texinfo-mode
  (indent-tabs-mode)
  (fill-column . 72)))
