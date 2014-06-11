(asdf:defsystem ql2catkin
  :name "ql2catkin"
  :author "Daniel Di Marco"
  :version "0.1"
  :maintainer "Daniel Di Marco"
  :licence "BSD"
  :depends-on ()
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "ql2catkin" :depends-on ("package"))))))


