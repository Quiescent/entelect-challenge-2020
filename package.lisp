(defpackage :bot
  (:use :cl :yason :iterate :metabang-bind :anaphora)
  (:export #:main
           #:define-poclo
           #:camel-case
           #:snake-case
           #:screaming-snake-case))
