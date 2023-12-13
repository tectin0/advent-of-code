;; some code to test the general idea

(defvar *seeds* nil)
(setq *seeds* (list (cons 79 93) (cons 55 68)))

(format t "~&~a~%" *seeds*)

(defvar *soil* nil)

(defvar *source_range* nil)
(setq *source_range* (list (cons 98 100) (cons 50 98)))

(defvar *destination_range* nil)
(setq *destination_range* (list (cons 50 52) (cons 52 100)))

(defvar *source_length* (length *source_range*))

(format t "source length: ~a~%" *source_length*)

(defvar idx 0)

(defvar *is_loop_break* 0)

;; loop over indices of source
(loop for source_index from 0 to (- *source_length* 1) do
    (setq idx 0)
    (setq *is_loop_break* 0)
    (loop while (= *is_loop_break* 0) do
        (when (nth idx *seeds*)
            (let* (
                (seed (nth idx *seeds*))

                (seed_start (car seed))
                (seed_end (cdr seed))
                ;; get current source_range
                (source_range_start (car (nth source_index *source_range*)))
                (source_range_end (cdr (nth source_index *source_range*)))
                ;; get current destination_range
                (destination_range_start (car (nth source_index *destination_range*)))
                (destination_range_end (cdr (nth source_index *destination_range*)))

                (partial_source_start (max seed_start source_range_start))
                (partial_source_end (min seed_end source_range_end))

                (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                )

                (format t "idx: ~a~%" idx)
                (format t "source_idx: ~a~%" source_index)
                (format t "~&~a~%" seed)

                (format t "~&~a~%" source_range_start)
                (format t "~&~a~%" source_range_end)

                (when (< partial_source_start partial_source_end)
                    (format t "seed_start: ~a~%" seed_start)
                    (format t "seed_end: ~a~%" seed_end)

                    (format t "source_range_start: ~a~%" source_range_start)
                    (format t "source_range_end: ~a~%" source_range_end)

                    (format t "destination_range_start: ~a~%" destination_range_start)
                    (format t "destination_range_end: ~a~%" destination_range_end)

                    (format t "partial_source_start: ~a~%" partial_source_start)
                    (format t "partial_source_end: ~a~%" partial_source_end)

                    (format t "partial_destination_start: ~a~%" partial_destination_start)
                    (format t "partial_destination_end: ~a~%" partial_destination_end)

                    (setq *soil* (append *soil* (list (cons partial_destination_start partial_destination_end))))
                    
                    ;; set current seed to nil
                    (setf (nth idx *seeds*) nil)
                    (setq *seeds* (append *seeds* (list (cons seed_start partial_source_start))))
                    (setq *seeds* (append *seeds* (list (cons partial_source_end seed_end))))
                )

            )
        )

        (setq idx (+ idx 1))
        (format t "length of seeds: ~a~%" (length *seeds*))
        (when (= idx (length *seeds*))
            (setq *is_loop_break* 1)
        )
    )
)

(loop for seed in *seeds* do
    ;; when entry exists and both values are not the same
    (when (and seed (not (= (car seed) (cdr seed))))
        (setq *soil* (append *soil* (list seed)))
    )
)


(format t "~&~a~%" *seeds*)
(format t "~&~a~%" *soil*)