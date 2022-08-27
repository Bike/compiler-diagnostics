(asdf:defsystem compiler-diagnostics
  :version "0.0.1"
  :author "Bike <aeshtaer@gmail.com>"
  :license "BSD"
  :description "Portable interface to CL compiler diagnostics."
  :homepage "https://github.com/Bike/compiler-diagnostics"
  :bug-tracker "https://github.com/Bike/compiler-diagnostics/issues"
  :source-control (:git "https://github.com/Bike/compiler-diagnostics.git")
  :depends-on ()
  :components ((:file "packages")
               (:file "diagnostics" :depends-on ("packages"))))
