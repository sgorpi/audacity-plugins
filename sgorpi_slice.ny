;nyquist plug-in
;version 4
;type analyze
;name "Beat Slice"
;author "Sgorpi"
;copyright "GNU General Public License v3 or later"
;release "0.1"
;info "Slice audio according to beats"


; Set the output format (example: 53.3 dB)
;;(setq *float-format* "%#4.3f");
(setq *float-format* "%1.5f") ; 5 decimal places
; Block size in smp

$control BPM "Beats per minute" float "BPM" 120.0 70.0 250.0
$control KEEP "Beats to keep" string "1=keep, -=with previous, 0=ignore" "0000 001-"
$control FILEPATH (_ "Save slices to...") file (_ "Select a file") "*default*/slice_.wav" (
						   ((_ "WAV file") (wav WAV))
						   ((_ "FLAC file") (flac FLAC))
						   ((_ "AIFF file") (aiff AIFF))
                           ((_ "All files") (""))) "save,overwrite"
$control ACTION "Action" choice "Label,Label and write to file,Write to file" 1
;; see 'equalabel.ny' and 'sample-data-export.ny'


;; Global constants
(setf SECONDS_PER_BEAT (/ 60 BPM))
(setf SAMPLES_PER_BEAT (* SECONDS_PER_BEAT *sound-srate*))
(setf ZEROPAD 4)


(defun get-file-extension (fname)
;; Return file extension or empty string
  (let ((n (1- (length fname)))
        (ext ""))
    (do ((i n (1- i)))
        ((= i 0) ext)
      (when (char= (char fname i) #\.)
        (setf ext (subseq fname (1+ i)))
        (return ext)))))

(defun get-file-name (fname)
;; Return filename without extension
  (let ((n (1- (length fname))) (name ""))
    (do ((i 0 (1+ i)))
        ((= i n) name)
      (when (char= (char fname i) #\.)
        (setf name (subseq fname 0 i))
        (return name))
	)
  )
)

(defun string-to-action-list (s)
;; Returns a list of actions based on input string
  (if (> (length s) 0)
    (append (cond
      ((equal (subseq s 0 1) "-") '(2))
      ((equal (subseq s 0 1) "1") '(1))
      ((equal (subseq s 0 1) "0") '(0))
      (t '()) ;; skip all other characters
    ) (string-to-action-list (subseq s 1)))
  )
)

(defun action-time-list-to-action (l) (first (first l)))
(defun action-time-list-to-seconds (l) (second (first l)))

(defun action-list-to-action-time-list (action-list)
;; Returns a list of tuples (action, numseconds)
  (let ((n (length action-list))
        (tuple (list (first action-list) SECONDS_PER_BEAT)) )
    (cond
      ((> n 1)
        (let ((outl (list tuple))
              (prevl (action-list-to-action-time-list (rest action-list)))
             )
          ;; if the previous action was to 'keep with previous'', or current and previous actions are 'ignore'
          ;; then merge two beats in one action, else keep separate
          (if (or (and (= (action-time-list-to-action outl) 0) (= (action-time-list-to-action prevl) 0))
                  (= (action-time-list-to-action prevl) 2))
            (append (list (list (first action-list) (+ SECONDS_PER_BEAT (action-time-list-to-seconds prevl)))) (rest prevl))
            (append outl prevl)
          )
        )
      )
      ((= n 1)
        (list tuple))
      (t nil)
    )
  )
)

(defun get-current-action (i action-time-list)
  (nth (rem i (length action-time-list)) action-time-list)
)

(defun make-padded-label (num)
;; zero-pad a number
  (let* ((num-text (format nil "~a" num))
        (non-zero-digits (length num-text)))
    (dotimes (i (max 0 (- ZEROPAD non-zero-digits)))
      (setf num-text (format nil "~a~a" "0" num-text)))
    num-text
  )
)
(defun make-labels (action-time-list &aux labels current-time cnt)
;; create labels based on the action-time-list. The time is relative to the start of the selection
  (setf current-time 0)
  (setf cnt 0)
  (do ( (i 0 (1+ i)) 
      )
      ((or (> current-time (/ len *sound-srate*)) (>= i 9999))) ;; stay within selection and zero-pad digits
    (let ((current-action (first (get-current-action i action-time-list)))
          (current-duration (second (get-current-action i action-time-list)))
          (start-time current-time)
         )
      (setf current-time (+ current-time current-duration))
      (if (= current-action 1)
        (push (list start-time current-time (make-padded-label (length labels))) labels))
    )
  )
  labels
)

(defun get-snd-samples (sig numsmp)
;; get an array of numsmp samples from sound 'sig'
  (let ((samples (make-array numsmp)))
    (dotimes (i numsmp)
      (let ((smp (snd-fetch sig)))
        (setf (aref samples i) (if smp smp 0.0)))
    )
    (snd-from-array 0 *sound-srate* samples)
  )
)

(defun store-audio (action-time-list audio file-prefix file-ext &aux current-time cnt)
;; store all sections of audio marked to keep in action-time-list to files with file-prefix and file-extension
  (setf current-time 0)
  (setf cnt 0)
  (do ( (i 0 (1+ i)) 
      )
      ((or (> current-time (/ len *sound-srate*)) (>= i 9999))) ;; stay within selection and zero-pad digits
    (let* ((current-action (first (get-current-action i action-time-list)))
          (current-duration (second (get-current-action i action-time-list)))
          (sample-start (* current-time *sound-srate*))
          (sample-length (* current-duration *sound-srate*))
          (start-time current-time)
         )
      (setf current-time (+ current-time current-duration))
      ;; always fetch audio
      (let ((audio-sample (multichan-expand #'get-snd-samples audio (truncate sample-length))))
        (if (= current-action 1)
          (let ((filename (format nil "~a~a.~a" file-prefix (make-padded-label cnt) file-ext)))
            (setf cnt (1+ cnt))
            ;; todo: save in same format as source material
            (s-save audio-sample NY:ALL filename :format snd-head-wave :mode snd-mode-float :bits 32)
          )
        )
      )
    )
  )
)

(let* ((action-time-list (action-list-to-action-time-list (string-to-action-list KEEP)))
      (file-prefix (get-file-name FILEPATH))
      (file-ext (get-file-extension FILEPATH))
      (labels (make-labels action-time-list))
     )
  (case ACTION
    (1 (store-audio action-time-list *track* file-prefix file-ext) labels)
    (2 (store-audio action-time-list *track* file-prefix file-ext) "")
    (t labels)
  )
)
