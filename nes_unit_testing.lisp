(defpackage :nes-unit-testing
  (:use :cl :cl-6502))
(in-package :nes-unit-testing)

(defun get-number (text)
  "Parse the integer, in either decimal or hexidecimal notation."
  (cond
    ((char= (aref text 0) #\$) (parse-integer (subseq text 1) :radix 16))
    (t (parse-integer text :radix 10))))

(defmacro sethash (hash key val)
  "Set the value in the hash table given the key."
  `(setf (gethash ,key ,hash) ,val))

(defun read-entire-file (filename)
  "Read all of the given filename's contents."
  (with-open-file (fp filename :direction :input)
    (let ((seq (make-string (file-length fp))))
      (read-sequence seq fp)
      seq)))

(defun handle-directives (text env hook-func)
  "Add any assignments to the environment, ignore other directives."
  (let ((stripped (loop for line in (cl-ppcre:split "\\n" text)
                     when (cond
                            ((and hook-func (funcall hook-func line))
                             nil)
                            ((or (cl-ppcre:scan "^\\." line)
                                 (cl-ppcre:scan " \\.byte\\b" line)
                                 (cl-ppcre:scan " \\.word\\b" line)) nil)
                            ((cl-ppcre:scan "^\\w+\\s*=\\s*" line)
                             (destructuring-bind (label value)
                                 (cl-ppcre:split "\\s*=\\s*" line)
                               (setf (gethash label env) (get-number value))
                               nil))
                            (t t))
                     collect line)))
    (format nil "~{~a~^~%~}" stripped)))

(defun compile-assembly-source (test-path env &optional hook-func)
  "Find the source file that corresponds to the test script, and compile it."
  (let* ((name (pathname-name test-path))
         (trim "_test")
         (offset (- (length name) (length trim)))
         (path nil))
    (when (string= (subseq name offset) trim)
      (setf name (subseq name 0 offset)))
    (setf path (make-pathname :directory (pathname-directory test-path)
                              :name name :type "asm"))
    (format nil "~a~%brk~%" (handle-directives (read-entire-file path)
                                               env hook-func))))

(defmacro cpu-flag-symbol (flag)
  `(if (= (6502:status-bit ,flag) 0) #\. #\+))

(defun pretty-print-cpu (cpu)
  (format t (concatenate 'string "[A ~(~2,'0x~)] [X ~(~2,'0x~)] [Y ~(~2,'0x~)] "
                                 "[C ~a N ~a Z ~a V ~a] [T ~a] "
                                 "[PC ~(~4,'0x~)]~%")
          (6502:cpu-ar cpu) (6502:cpu-xr cpu) (6502:cpu-yr cpu)
          (cpu-flag-symbol :carry) (cpu-flag-symbol :negative)
          (cpu-flag-symbol :zero) (cpu-flag-symbol :overflow)
          (6502:cpu-cc cpu)
          (6502:cpu-pc cpu)))

(defun get-brk-address (cpu)
  (let* ((sp (6502:cpu-sp cpu))
         (ret-addr (+ sp 1))
         (low-byte (6502::get-range ret-addr (+ ret-addr 1)))
         (high-byte (6502::get-range (+ ret-addr 1) (+ ret-addr 2))))
    (format t "Stack: ~a - ~a ~a~%" sp low-byte high-byte)

  ;(stack-pop cpu)
  ;(stack-pop-word cpu)
    ))

(define-condition expectation-complete ()
  ()
  (:documentation "Condition when expections have completed being checked."))

(define-condition expectation-error (error)
  ((name :initarg :name :reader :name)
   (actual-value :initarg :actual-value :reader :actual-value)
   (expect-value :initarg :expect-value :reader :expect-value))
  (:documentation "Error when an expectation fails."))

(defmethod print-object ((object expectation-error) stream)
  (with-slots (actual-value expect-value name) object
    (format stream "~a - Actual ~s, Expect ~s~%"
            name actual-value expect-value)))

(defmacro expect-equal (actual expect &optional name)
  "Verify the actual value matches the expected value."
  `(restart-case (unless (equalp ,actual ,expect)
                   (error 'expectation-error :actual-value ,actual
                          :expect-value ,expect :name (or ,name ',expect)))
     (check-more-expectations () nil)))

(defvar *ppu-updates* nil)
(defvar *ppu-latch* 0)
(defvar *ppu-pointer* nil)

(handler-bind ((style-warning #'muffle-warning))
  "Redefine memory access, mapping addresses to the ppu."
  (defun 6502:get-byte (address)
    (cond
      ((< address #x2000) (aref 6502::*ram* address))
      ((= address #x2002) (progn (setf *ppu-latch* 0) 0))
      (t (aref 6502::*ram* address))))
  (defun (setf 6502:get-byte) (new-val address)
    (cond
      ((< address #x2000) (setf (aref 6502::*ram* address) new-val))
      ((= address #x2006) (progn
                            (if (= *ppu-latch* 0)
                                (setf *ppu-pointer* new-val)
                                (setf *ppu-pointer* (+ (* *ppu-pointer* 256)
                                                       new-val)))
                            (setf *ppu-latch* (- 1 *ppu-latch*))))
      ((= address #x2007) (progn
                            (vector-push-extend (list *ppu-pointer* new-val)
                                                *ppu-updates*)
                            (incf *ppu-pointer*)))
      (t (setf (aref 6502::*ram* address) new-val)))))

(defvar *current-filename* nil)

(defvar *cpu* nil)
(defvar *cpu-bytes* nil)
(defvar *cpu-org-start* nil)

(defvar *env-addrs* nil)
(defvar *env-vars* nil)

(defvar *test-inputs* nil)
(defvar *test-min-time* nil)
(defvar *test-max-time* nil)
(defvar *test-total-time* nil)
(defvar *test-num-runs* nil)

(defun write-memory-value (addr value)
  (setf (cl-6502:get-range addr) (make-array (length value)
                                             :initial-contents value)))

(defun assign-variable-value (name value)
  (let* ((size (gethash name *env-vars*))
         (label (string-downcase (substitute #\_ #\- (string name))))
         (addr (gethash label *env-addrs*)))
    (case size
      (:byte (setf (cl-6502:get-range addr) (vector value)))
      (:word (setf (cl-6502:get-range addr) (vector (mod value 256)
                                                    (floor (/ value 256)))))
      (otherwise (error (format nil "Unknown size ~s" size))))))

(defun expect-memory-value (addr value)
  (expect-equal (cl-6502::get-range addr (+ addr (length value)))
                (make-array (length value) :initial-contents value)
                (format nil "$~4,'0x" addr)))

(defun expect-variable-value (name value)
  (let* ((size (gethash name *env-vars*))
         (label (string-downcase (substitute #\_ #\- (string name))))
         (addr (gethash label *env-addrs*)))
    (case size
      (:byte (expect-equal (cl-6502:get-range addr 1) (vector value) label))
      (:word (expect-equal (cl-6502:get-range addr 2)
                           (vector (mod value 256) (floor (/ value 256)))
                           label))
      (otherwise (error (format nil "Unknown size ~s" size))))))

(defun get-cpu-state (kind)
  "Get a register value or the cycle count."
  (case kind
    (:a (6502:cpu-ar *cpu*))
    (:x (6502:cpu-xr *cpu*))
    (:y (6502:cpu-yr *cpu*))
    (:cc (6502:cpu-cc *cpu*))))

(defun initialize-test-case (&key env filter-source)
  "Setup ram according to the environment, and parse the source file."
  (let ((addr 0) file-content)
    (setf *env-addrs* (make-hash-table :test 'equal))
    (setf *env-vars* (make-hash-table))
    ; Process environment.
    (loop for (name value) in env
         do (let ((label (substitute #\_ #\- (string name))))
              (when (symbolp name)
                (setf label (string-downcase label)))
              (cond
                ((eq value :byte) (progn (sethash *env-addrs* label addr)
                                         (sethash *env-vars* name value)
                                         (incf addr 1)))
                ((eq value :word) (progn (sethash *env-addrs* label addr)
                                         (sethash *env-vars* name value)
                                         (incf addr 2)))
                ((numberp value) (sethash *env-addrs* label value)))))
    ; Reset ram.
    (setf 6502::*ram* (6502::bytevector #x10000))
    ; Parse source file and setup cpu.
    (setf file-content (compile-assembly-source *current-filename* *env-addrs*
                                                filter-source))
    (setf *cpu-org-start* #x8000)
    (setf *cpu-bytes* (cl-6502:asm file-content *env-addrs* *cpu-org-start*))
    (setf *cpu* cl-6502:*cpu*)
    (setf *test-min-time* nil *test-max-time* 0 *test-total-time* 0
          *test-num-runs* 0)))

(defun initialize-fake-ppu ()
  "Clear any leftover ppu updates."
  (setf *ppu-updates* (make-array 0 :adjustable t :fill-pointer 0)))

(defun run-test-case (&key a x y memory)
  "Set the state of registers and ram, then run the compiled code."
  ; Reset.
  (6502:reset *cpu*)
  (setf *test-inputs* nil)
  ; Input.
  (when a (setf (6502:cpu-ar *cpu*) a) (push `(a ,a) *test-inputs*))
  (when x (setf (6502:cpu-xr *cpu*) x) (push `(x ,x) *test-inputs*))
  (when y (setf (6502:cpu-yr *cpu*) y) (push `(y ,y) *test-inputs*))
  (when memory
    (loop for (addr value) in memory
         do (cond
              ((numberp addr) (write-memory-value addr value))
              ((symbolp addr) (assign-variable-value addr value))
              (t (error "Unknown memory assignment ~s <- ~s" addr value))))
    (push `(:memory ,memory) *test-inputs*))
  ; Load code.
  (setf (cl-6502:get-range *cpu-org-start*) *cpu-bytes*)
  (setf (6502:cpu-pc *cpu*) *cpu-org-start*)
  ; Setup stack.
  (6502::stack-push-word #xfffd *cpu*)
  ; Execute.
  (cl-6502:execute *cpu*)
  ; Remove time to brk.
  (setf (6502:cpu-cc *cpu*) (- (6502:cpu-cc *cpu*) 7))
  ; Accumulate test time.
  (let ((time (get-cpu-state :cc)))
    (when (or (not *test-min-time*) (< time *test-min-time*))
      (setf *test-min-time* time))
    (when (> time *test-max-time*)
      (setf *test-max-time* time))
    (incf *test-total-time* time)
    (incf *test-num-runs*))
  nil)

(defun expect-result (&key a x y memory ppu)
  "Verify the state of the regsters, ram, and ppu."
  (when a (expect-equal (6502:cpu-ar *cpu*) a))
  (when x (expect-equal (6502:cpu-xr *cpu*) x))
  (when y (expect-equal (6502:cpu-yr *cpu*) y))
  (when memory
    (loop for (addr value) in memory
       do (cond
            ((numberp addr) (expect-memory-value addr value))
            ((symbolp addr) (expect-variable-value addr value))
            (t (error "Unknown memory assignment ~s <- ~s" addr value)))))
  (when ppu
    (expect-equal *ppu-updates* (make-array (length ppu)
                                            :initial-contents ppu)))
  (signal 'expectation-complete))

(defvar *all-nes-tests* (make-hash-table))

(defmacro deftest (name &body body)
  "Begin a test case."
  (sethash *all-nes-tests* name t)
  (let ((this-filename nil)
        (test-success-sym (gensym))
        (test-expectation-failures-sym (gensym)))
    (eval-when (:compile-toplevel)
      (setf this-filename *compile-file-pathname*))
    (eval-when (:load-toplevel :execute)
      (unless this-filename
        (setf this-filename *load-truename*)))
    `(defun ,name ()
       (let ((*current-filename* ,this-filename)
             (,test-success-sym t)
             (,test-expectation-failures-sym nil))
         (handler-bind
             ((expectation-error
               #'(lambda (c)
                   (push c ,test-expectation-failures-sym)
                   (invoke-restart 'check-more-expectations)))
              (expectation-complete
               #'(lambda (c)
                   (declare (ignore c))
                   (when ,test-expectation-failures-sym
                     (format t "Failed on test: ~a~%~{~a~}~%" *test-inputs*
                             ,test-expectation-failures-sym)
                     (setf ,test-expectation-failures-sym nil)
                     (setf ,test-success-sym nil)))))
           ,@body
           (when ,test-success-sym
             (format t "Success!~%")
             nil))))))

(defun all-tests-in-current-directory ()
  "Loads all lisp files in the current directory named *_test.lisp. Run tests."
  (let ((entities (directory "*.lisp")))
    (loop for entity in entities
       do (let* ((name (pathname-name entity))
                 (suffix "_test")
                 (offset (max (- (length name) (length suffix)) 0)))
            (when (string= suffix (subseq name offset))
              (load entity)))))
  (loop for k being the hash-keys of *all-nes-tests*
     do (funcall k)))

(defun display-test-timing ()
  "Display to standard output the timing of the test runs."
  (format t "Number of test runs: ~s~%" *test-num-runs*)
  (format t "Minimum time: ~s~%" *test-min-time*)
  (format t "Maximum time: ~s~%" *test-max-time*)
  (format t "Average time: ~s~%" (float (/ *test-total-time* *test-num-runs*))))
