(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (unless (probe-file quicklisp-init)
    (error "Quicklisp is not installed!"))
  (load quicklisp-init))

(ros-load:load-system "ql2catkin" :ql2catkin)
(in-package :ql2cat)

(if (/= (length sb-ext:*posix-argv*) 3)
  (format t "usage:~%ql2catkin <name> <base-path>~%")
  (catkinize-system (nth 1 sb-ext:*posix-argv*)
                    (nth 2 sb-ext:*posix-argv*)))
(sb-ext:quit)
