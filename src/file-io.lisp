(in-package :parsimony)

;; Simple structure to represent a location in a file
(defstruct (file-location (:conc-name :fl-))
  (path (error "must provide path") :type (or string pathname))
  (row 1 :type fixnum)
  (col 1 :type fixnum))

(defun pretty-print-file-loc (loc)
  (declare (type file-location loc))
  (format nil "~a:~d:~d" (fl-path loc) (fl-row loc) (fl-col loc)))

(defmethod print-object ((obj file-location) stream)
  (format stream "#<PRS:FILE-LOCATION ~a>"
          (pretty-print-file-loc obj)))

(defun copy-file-loc (loc)
  (declare (type file-location loc))
  (make-file-location :path (fl-path loc)
                      :row (fl-row loc)
                      :col (fl-col loc)))

(defun inc-file-row (loc)
  (declare (type file-location loc))
  (incf (fl-row loc)))

(defun inc-file-col (loc)
  (declare (type file-location loc))
  (incf (fl-col loc)))

(defun dec-file-row (loc)
  (declare (type file-location loc))
  (decf (fl-row loc)))

(defun dec-file-col (loc)
  (declare (type file-location loc))
  (decf (fl-col loc)))

(defclass prs-file-stream ()
  ((stream :initform (error "needs a stream")
           :initarg :stream)
   (loc :type file-location :initform (error "needs initial location")
        :initarg :loc)
   (line-lengths :type list :initform nil
                 :reader prs-fs-ll))
  (:documentation "A parsimony stream of an input file. Expects to completely own the input, i.e, the file-stream object should be the only way the stream is read."))


(defun new-file-stream (path)
  (declare (type (or pathname string) path))
  (let* ((stream (open path :direction :input))
         (loc (make-file-location :path path)))
    (make-instance 'prs-file-stream :stream stream :loc loc)))

(defmethod get-stream ((s prs-file-stream) ctxt)
  (declare (ignore ctxt))
  (with-slots (stream loc) s
    (let ((c (read-char stream nil :eof))
          (start-loc (copy-file-loc loc)))
      (case c
        ;; If EOF, no need to change loc
        (:eof (values :eof start-loc))

        ;; If newline, update loc accordingly
        (#\newline
         (inc-file-row loc)
         (push (fl-col loc) (slot-value s 'line-lengths))
         (setf (fl-col loc) 1)
         (values #\newline start-loc))

        ;; If ordinary char, just advance col no
        (otherwise
         (inc-file-col loc)
         (values c start-loc))))))

(defmethod put-stream ((obj character) (s prs-file-stream))
  (with-slots (loc) s
    (if (eq obj #\newline)
        ;; If a newline, restore to previous line's column no
        (progn
          (dec-file-row loc)
          (setf (fl-col loc)
                (pop (slot-value s 'line-lengths))))
        (dec-file-col loc))
    (assert (not (or (<= (fl-col loc) 0)
                     (<= (fl-row loc) 0)))))
  (unread-char obj s))

(defmethod peek-stream ((s prs-file-stream) ctxt)
  (values (peek-char nil s nil :eof)
          (copy-file-loc (slot-value s 'loc))))