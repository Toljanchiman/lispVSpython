;https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/binarytrees-sbcl-3.html
(setf sb-ext:*evaluator-mode* :interpret)
;(declaim (sb-ext:muffle-conditions cl:warning))
;(declaim (sb-ext:muffle-conditions cl:style-warning))
(deftype uint () '(unsigned-byte 62))
;(defmacro uint () '(unsigned-byte 62))
(defconstant min-depth 4 "Minimal depth of the binary tree.")
;(DEFVAR min-depth 4 "Minimal depth of the binary tree.")
(defparameter num-workers 4 "Number of concurrent workers.")
;(DEFVAR num-workers 4 "Number of concurrent workers.")

;выполнить код в файле, в cmd
;sbcl --dynamic-space-size 8192 (load "binaryTree.lsp")
;sbcl --noinform --disable-ldb --end-runtime-options --script "binaryTree.lsp"
;Код можно скомпилировать в байт-код и загружать его быстрее '(load "test.fasl")'
;sbcl --noinform --disable-ldb --end-runtime-options --script "lc.lsp"
;(require :sb-concurrency)(load (compile-file "binaryTree.lsp" ))
;sbcl --noinform --disable-ldb --end-runtime-options --load "binaryTree.fasl"

(require :sb-concurrency)

(defun build-tree (depth)
    "Build a binary tree of the specified DEPTH. Leaves are represented by NIL,
branches are represented by a cons cell."
  (declare (ftype (function (uint) list) build-tree)
           (uint depth)
           (optimize (speed 3) (safety 0)))
  (if (zerop depth) (cons nil nil)
      (cons (build-tree (1- depth))
            (build-tree (1- depth)))))

(defun check-node (node)
  (declare (ftype (function (list) uint) check-node)
           (optimize (speed 3) (safety 0)))
  (if (null (car node))
      1
      (the uint (+ 1 (check-node (car node)) (check-node (cdr node))))))

(defun check-trees-of-depth (depth max-depth)
  (declare (uint depth max-depth)
           (optimize (speed 3) (safety 0)))
  (loop with iterations of-type uint = (ash 1 (+ max-depth min-depth (- depth)))
        for i of-type uint from 1 upto iterations
        sum (check-node (build-tree depth))
        into result of-type uint
        finally (return (format nil "~d~c trees of depth ~d~c check: ~d~%"
                                iterations #\Tab depth #\Tab result))))

(defun loop-depths-async (max-depth)
  (declare (fixnum max-depth))
  (let* ((tasks (sb-concurrency:make-queue
                 :initial-contents
                 (loop for depth from min-depth by 2 upto max-depth
                       collect depth)))
         (outputs (sb-concurrency:make-queue))
         (threads
           (loop for i of-type fixnum from 1 to num-workers
                 collect (sb-thread:make-thread
                          #'(lambda ()
                              (loop as task = (sb-concurrency:dequeue tasks)
                                    while task
                                    do (sb-concurrency:enqueue
                                        (cons task
                                              (check-trees-of-depth task max-depth))
                                        outputs)))))))
    (mapc #'sb-thread:join-thread threads)
    (let ((results (sort (sb-concurrency:list-queue-contents outputs)
                         #'< :key #'car)))
      (loop for (k . v) in results
            do (format t "~a" v)))))

(defun binary-trees-upto-size (n)
  (declare (type (integer 0 255) n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths-async n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

;(defun main (&optional (n (parse-integer (or (car (last sb-ext:*posix-argv*))"1"))))
;(binary-trees-upto-size n))
(time (binary-trees-upto-size 21))
;(binary-trees-upto-size 21)
;(main)
(quit)
;(load (compile-file "binaryTree.lsp" ))