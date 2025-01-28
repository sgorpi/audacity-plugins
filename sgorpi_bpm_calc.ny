;nyquist plug-in
;version 4
;type analyze
;name "BPM Calculator"
;author "Sgorpi"
;copyright "GNU General Public License v3 or later"
;release "0.1"
;info "Calculate the BPM based on the selected audio length"

$control BPM "Beats per minute" float "BPM" 120.0 70.0 250.0

(setf SECONDS_PER_BEAT (/ 60 BPM))

(defun guess-num-beats ()
    (setf nsec (/ len *sound-srate*))
    (setf nbeats (round (/ nsec SECONDS_PER_BEAT)))
)

(defun derive-bpm ()
    (setf nsec (/ (/ len *sound-srate*) (guess-num-beats)))
    (setf bpm (/ 60 nsec))
)

;; output
(format nil "Selection: \t~a samples~%Selection: \t~a seconds~%Selection: \t~~~a beats?~%~%Guessed: \t~a BPM~%Derived: \t~a BPM~%"
len
(/ len *sound-srate*)
(guess-num-beats)
BPM
(derive-bpm))
