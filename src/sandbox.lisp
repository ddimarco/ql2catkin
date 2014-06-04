(in-package :ql2cat)

(eval-when (:load-toplevel)
  ;; TODO: download first?
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp is not installed!"))
    (load quicklisp-init)))

(defun directory-p (p)
  (null (pathname-name p)))
(defun recursively-find-files-of-type (dir type)
  (let ((files nil)
        (dir (if (equal (type-of dir) 'pathname)
                 (namestring dir)
                 dir)))
    (dolist (p (directory (make-pathname :directory dir :name :wild :type :wild)))
      (when (string= (pathname-type p) type)
        (push p files))
      (when (directory-p p)
        (setf files (append files (recursively-find-files-of-type p type)))))
    files))

(defun run-process (control-string &rest args)
  (let* ((return-value 0)
         (output-string
          (with-output-to-string (asdf::*verbose-out*)
            (setf return-value
             (apply #'asdf:run-shell-command control-string args)))))
    (values return-value output-string)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun rosify-name (name)
  (let ((name (if (string/= (subseq name 0 3) "cl-")
                  (concatenate 'string "cl_" name)
                  name)))
    (replace-all (replace-all name "-" "_") "." "_")))

(defun find-ros-package (name)
  (handler-case
      (ros-load:ros-package-path name)
    (simple-error () nil)))

(defun find-in-ros-system (asdf-name)
  (or
   (find-ros-package asdf-name)
   (find-ros-package (rosify-name asdf-name))
   (find-ros-package (subseq (rosify-name asdf-name) 3))))

(defun catkinize-system (name base-path)
  "creates a ROS package for the quicklisp system `name'."
  (format t "catkinize ~a into ~a~%" name base-path)
  (let ((ros-pkg (find-in-ros-system name)))
    (when ros-pkg
      (format t "already found ~a in ~a~%" name ros-pkg)
      (return-from catkinize-system
        (car (last (pathname-directory ros-pkg))))))

  (let* ((dist (ql-dist:find-system name))
         (ros-name (rosify-name name))
         (pkg-path (make-pathname :directory `(:absolute ,base-path ,ros-name)))
         (deps (mapcar (lambda (req-sys)
                         (catkinize-system req-sys base-path))
                       (ql-dist:required-systems dist))))
    (assert dist)
    (when (probe-file pkg-path)
      (format t "path to ~a already exists. canceling...~%" pkg-path)
      (return-from catkinize-system ros-name))

    (ensure-directories-exist pkg-path)
    (let* ((release (ql-dist:release dist))
           (archive (ql-dist:ensure-local-archive-file release))
           (cmd (format nil "tar -xf ~a -C ~a" archive pkg-path)))
      (multiple-value-bind (retval output)
          (run-process cmd)
        (format t "~a~%~a~%" retval output))
      (let ((asdf-files (recursively-find-files-of-type pkg-path "asd"))
            (asdf-path (merge-pathnames (make-pathname :directory '(:relative "asdf"))
                                       pkg-path)))
        (ensure-directories-exist asdf-path)
        (dolist (asd-file asdf-files)
          (format t "~a; ~a~%" asdf-path asd-file)
          (run-process "cd ~a; ln -s ~a" asdf-path asd-file)))

      ;; create package.xml and CMakeLists.txt
      (with-open-file (cmake (merge-pathnames (make-pathname :name "CMakeLists" :type "txt")
                                              pkg-path)
                             :direction :output)
        (format cmake "cmake_minimum_required(VERSION 2.8.3)~%project(~a)~%find_package(catkin REQUIRED)~%catkin_package()~%foreach(dir~%src~%tests)~%install(DIRECTORY ${dir}~%DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}~%PATTERN \".svn\" EXCLUDE~%PATTERN \".git\" EXCLUDE)~%endforeach()~%# install (FILES cram-utilities.asd cram-utilities-tests.asd~%#     DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})~%"
                ros-name))
      (with-open-file (pkg (merge-pathnames (make-pathname :name "package" :type "xml")
                                            pkg-path)
                           :direction :output)
        (format pkg "<package>
  <name>~a</name>
  <version>0.0.1</version>
  <description>
package automatically imported from quicklisp.
  </description>

  <author email=\"bla@bla.org\">See sources</author>
  <maintainer email=\"bla@bla.org\">See sources</maintainer>

  <license>BSD</license>
  <url type=\"website\">http://ros.org/</url>
  <buildtool_depend>catkin</buildtool_depend>
  <build_depend>sbcl</build_depend>
  <run_depend>sbcl</run_depend>~%" ros-name)
        ;; deps
        (dolist (d deps)
          (format pkg "<run_depend>~a</run_depend>~%" d))

        (format pkg "</package>~%"))
      ros-name)))
