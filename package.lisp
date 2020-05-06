(defpackage :bot
  (:use :cl :yason :iterate :metabang-bind :anaphora :arrow-macros)
  (:export #:main
           #:define-poclo
           #:camel-case
           #:snake-case
           #:screaming-snake-case))
