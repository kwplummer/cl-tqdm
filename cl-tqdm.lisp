(defpackage :cl-tqdm
  (:use :cl)
  (:export
   #:tqdm
   #:config
   #:with-config
   #:with-tqdm
   #:update))

(in-package :cl-tqdm)

(declaim (inline time-since))
(defun time-since (now then)
  (/ (- now then) internal-time-units-per-second))

(defstruct (EMA)
  "EMA (Exponential Moving Average) Structure."
  (alpha 0.1 :type single-float)
  (value 1.0 :type single-float))
(defun add-ema (ema value)
  (declare (type EMA ema))
  (setf (ema-value ema)
        (+ (* (ema-alpha ema) value)
           (* (- 1.0 (ema-alpha ema)) (ema-value ema)))))
(defun get-ema (ema)
  (declare (type EMA ema))
  (ema-value ema))

(defstruct (TqdmBar
            (:conc-name tqdm-)
            (:print-function
             (lambda (tqdm stream depth)
               (declare (ignore depth))
               (update tqdm :incf 0 :stream stream)
               nil))
            (:constructor
                tqdm (total-count
                      &optional
                        (description "")
                      &aux (creation-time (get-universal-time)))))
  "Tqdm Structure that contains informations.

APIs read this structure and update, rendering progress-bar in terminals.

Example:

```lisp
(print (tqdm :FirstBar 100))
```"
  (identifier :no-name :type symbol)
  (total-count 0 :type fixnum)
  (ema (make-ema :alpha 0.1))
  (count-idx 0 :type fixnum)
  (creation-time 0 :type (integer 0 4611686018427387903))
  (last-update-time 0 :type (integer 0 4611686018427387903))
  (description "" :type string))

(defstruct (TqdmConfig
            (:conc-name config-)
            (:print-function
             (lambda (config stream depth)
               (declare (ignore depth))
               (format stream "TqdmConfig{~% :space-string ~a~% :bar-string ~a :indent ~a~%}"
                       (config-space-string config)
                       (config-bar-string config)
                       (config-indent config))))
            (:constructor
                config (&key
                          (space-string " ")
                          (bar-string "█")
                          (indent 0))))
  (space-string " " :type string)
  (bar-string "█" :type string)
  (indent 0 :type fixnum))

(defparameter *tqdm-config* (config :space-string " "))

(defparameter *in-update-method* nil)

(defmacro with-tqdm (out
                     total-size
                     description
                     &body
                       body)
  "Example:
(with-tqdm x :ProgressBar1 100 \"\"
  (update x))"
  `(let ((,out (tqdm ,total-size ,description)))
     (fresh-line)
     ,@body))

(defmacro with-config (config &body body)
  `(let ((*tqdm-config* ,config))
     ,@body))

(defun update (tqdm &key (incf 1) (description "") total-count (stream t))
  (declare ;(optimize (speed 3))
   (type fixnum incf)
   (type tqdmbar tqdm))
  (if total-count (setf (tqdm-total-count tqdm) total-count))
  (let ((now (get-internal-real-time))
        (*in-update-method* t))
    (incf (tqdm-count-idx tqdm) incf)
    (setf (tqdm-description tqdm) description)
    (add-ema (tqdm-ema tqdm) (/ (time-since now (tqdm-last-update-time tqdm)) incf))
    (setf (tqdm-last-update-time tqdm) now)
    (render-progress-bar stream tqdm))
  incf)

(defun progress-percent (status)
  (fround (* 100 (/ (tqdm-count-idx status) (tqdm-total-count status)))))

(declaim (ftype (function (tqdmbar) string) render))
(defun render (status)
  "Rendering given status (which is the structure of tqdmbar), render returns the output string."
  (declare (optimize (speed 3))
           (type tqdmbar status))
  (with-output-to-string (bar)
    (let ((spl (- (config-indent *tqdm-config*) (length (tqdm-description status)) -1)))
      (write-string (tqdm-description status) bar)
      (dotimes (_ spl) (write-string " " bar))
      (unless (equal (tqdm-description status) "")
        (write-string ":" bar)
        (write-string " " bar)))
    (let* ((n (the fixnum (round (the single-float (progress-percent status)))))
           (r (round (if (>= (/ n 10) 10.0) 10 (/ n 10)))))
      (if (< n 100)
          (write-string " " bar))
      (write-string (write-to-string n) bar)
      (write-string "% |" bar)
      (dotimes (_ r) (write-string (config-bar-string *tqdm-config*) bar))
      (dotimes (_ (- 10 r)) (write-string (config-space-string *tqdm-config*) bar)))
    (write-string "| " bar)
    (write-string (write-to-string (tqdm-count-idx status)) bar)
    (write-string "/" bar)
    (write-string (write-to-string (tqdm-total-count status)) bar)
    (write-string " [" bar)
    (let* ((now-time (get-universal-time))
           (dif (- now-time (tqdm-creation-time status))))
      (write-string (write-to-string (coerce dif 'single-float)) bar)
      (write-string "s<" bar))
    (let* ((average-sec (get-ema (tqdm-ema status)))
           (total (tqdm-total-count status))
           (remaining (- total (tqdm-count-idx status))))
      (write-string (write-to-string (* remaining average-sec)) bar)
      (write-string "s, " bar)
      (write-string (write-to-string average-sec) bar)
      (write-string "s/it]" bar))))

(defun backward-lines ()
  (write-char #\Return)
  (write-char #\Rubout))

(defun render-progress-bar (stream tqdm)
  (declare (optimize (speed 3))
           (type TqdmBar tqdm))
  (fresh-line)
  (format stream (render tqdm))
  nil)
