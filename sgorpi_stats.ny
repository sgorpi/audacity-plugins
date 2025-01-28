;nyquist plug-in
;version 4
;type analyze
;name "Volume Stats"
;author "Sgorpi"
;copyright "GNU General Public License v3 or later"
;release "0.1"
;info "Modified from Steve Daulton. http://audacity.easyspacepro.com Released under GPL v2."


(setf largenumber 100000000) ;; Largest number of samples that can be imported (from clipfix)
(setf blocksize 0.05) ;; in seconds; audition std = 50 ms
(setf blockOverlap 0.0) ;; in percent
;;(setf sqCorr (sqrt 2.0)) ;; correction to square wave..
(setf sqCorr 1.0) ;; correction to square wave..

; Set the output format (example: 53.3 dB)
(setq *float-format* "%#3.2f");
; Block size in smp
(setf blocksizeSmp (round (* blocksize *sound-srate*)))
(setf blockOverlapReal (/ 1.0 (- 1.0 blockOverlap)))

(defun numSamplesToSeconds (smp)
	(setf sec (round (/ smp *sound-srate*)))

	(setf out (if (> sec 3600)(format NIL "~ah" (/ sec 3600))(format NIL "")) )
	(setf sec (if (> sec 3600)(rem sec 3600)(+ sec 0)) )

	(setf out (if (> sec 60)(format NIL "~a ~am" out (/ sec 60))(format NIL "~a" out)) )
	(setf sec (if (> sec 60)(rem sec 60)(+ sec 0)) )
	(setf out (format NIL "~a ~as" out sec))
	out
	)


(defun array-to-list (arr alen)
	(setf outl '())
	(dotimes (i alen)
		(setf outl (append outl (list (aref arr i))))
	)
	outl
)
;; adapted from nyquist-src/lib/statistics.lsp
(defun get-median (data)
	(setf count (snd-length data largenumber))
	(setf data (snd-samples data largenumber))
	(setf data (array-to-list data count))
	(setf data (sort data '<))
	(cond
		((oddp count)
			(nth (/ count 2) data))
		(t
			(setf i (/ count 2))
			(* 0.5 (+ (nth i data) (nth (1- i) data)))
		)
	)
)

(defun process-mono (input) ; mono track
	(setf rmsBlock_dB ())
	(setf rmsBlock_dBa ())
	(setf input_peak (peak input NY:ALL))

	;; Generate a 'sound' with averages
	;;(setf block_average (snd-avg input blocksizeSmp blocksizeSmp OP-AVERAGE))
	(setf block_average (rms input blockOverlapReal blocksizeSmp))
	(setf block_average_length (snd-length block_average largenumber))
	;; total rms
	(setf input_rms_total (snd-fetch (snd-avg block_average block_average_length block_average_length OP-PEAK) ))
	;; average rms
	(setf input_rms_average (snd-fetch (snd-avg block_average block_average_length block_average_length OP-AVERAGE) ))
	;; median rms
	(setf input_rms_median (get-median block_average))
	

	(format NIL "Sample Peak: \t~a dB~%RMS peak: \t~a dB~%RMS median: \t~a dB~%RMS mean: \t~a dB" 
		(linear-to-db input_peak) 
		(linear-to-db (* input_rms_total sqCorr))
		(linear-to-db (* input_rms_median))
		(linear-to-db (* input_rms_average sqCorr))
	) ;; ~% is new line, ~a is ascii
)

(defun analyze-mono (input)
	(format NIL "Mono track ~a~%~%Mono channel:~%~a~%"
		(numSamplesToSeconds len)
		(process-mono input)
		)
	)
(defun analyze-stereo (input) ; for stereo tracks
	(setf left (aref input 0))
	(setf right (aref input 1))
	(format NIL "Stereo track ~a~%~%Left channel:~%~a~%~%Right channel:~%~a~%~%Mid:~%~a~%~%Side:~%~a~%"
		(numSamplesToSeconds len)
		(process-mono left)
		(process-mono right)
		(process-mono (mult (snd-add left right) 0.5)) 				;; L+R aka mid
		(process-mono (mult (snd-add left (mult right -1.0)) 0.5))	;; L-R aka side
		)
	)

(if (arrayp *track*)(analyze-stereo *track*)(analyze-mono *track*))

