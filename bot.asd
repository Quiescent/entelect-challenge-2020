(in-package :cl-user)

(defpackage :bot.sys
  (:use :asdf :cl))

(in-package :bot.sys)

(defsystem :bot
  :name "Quantum"
  :author "Edward Steere"
  :version "0.0.1"
  :maintainer "edward.steere@gmail.com"
  :license "BSD"
  :description "Entellect challenge bot"
  :long-description "Bot for the Entellect Challenge"
  :depends-on (:yason :cl-ppcre :iterate)
  :components ((:file "bot" :depends-on ("state" "package"))
               (:file "state" :depends-on ("parsing" "package"))
               (:file "parsing" :depends-on ("package"))
               (:file "package")))
