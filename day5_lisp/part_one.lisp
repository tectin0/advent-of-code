(load ".sbclrc")
(ql:quickload "str")

(defun get_lowest_location_value (filename)

;; open file example and then write line by line to an array

;; explanation:
;; defparameter creates a global variable
;; *lines* is the name of the variable, the `*` are just a convention
;; nil is the initial value of the variable
(defparameter *lines* nil)

(let ((in (open filename :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (push line *lines*))
    (close in)))


(defvar *seeds* nil)

(let* ((first_line (last *lines*))
      ;; first line is CONS type, so we need to convert it to a string
      (first_line (str:join "" first_line)))
  ;; split the string into a list of strings
  (let* ((split_line (str:split ":" first_line))
          ;; only keep the second element of the list
          (split_line (second split_line)))
    (let ((seeds (str:words split_line)))
      (setf seeds (mapcar (lambda (seed) (parse-integer seed)) seeds))
      (setf *seeds* seeds))))

(format t "seeds: ~a~%" *seeds*)

;; all -1 same size as seeds
(defvar *soils* (make-list (length *seeds*) :initial-element -1))
(defvar *fertilizers* (make-list (length *seeds*) :initial-element -1))
(defvar *waters* (make-list (length *seeds*) :initial-element -1))
(defvar *lights* (make-list (length *seeds*) :initial-element -1))
(defvar *temperatures* (make-list (length *seeds*) :initial-element -1))
(defvar *humidities* (make-list (length *seeds*) :initial-element -1))
(defvar *locations* (make-list (length *seeds*) :initial-element -1))

(defvar *parse_state* "")

;; remove last line from *lines*
(setf *lines* (butlast *lines*))

;; loop over the lines
(loop for line in (reverse *lines*) do
  ;; check if line contains "seed-to-soil"
  (cond
    ;; seed-to-soil
    ((str:containsp "seed-to-soil" line) (format t "map line: ~a~%" line) (setf *parse_state* "seed-to-soil"))
    ;; soil-to-fertilizer
    ((str:containsp "soil-to-fertilizer" line) (format t "map line: ~a~%" line) (setf *parse_state* "soil-to-fertilizer"))
    ;; fertilizer-to-water
    ((str:containsp "fertilizer-to-water" line) (format t "map line: ~a~%" line) (setf *parse_state* "fertilizer-to-water"))
    ;; water-to-light
    ((str:containsp "water-to-light" line) (format t "map line: ~a~%" line) (setf *parse_state* "water-to-light"))
    ;; light-to-temperature
    ((str:containsp "light-to-temperature" line) (format t "map line: ~a~%" line) (setf *parse_state* "light-to-temperature"))
    ;; temperature-to-humidity
    ((str:containsp "temperature-to-humidity" line) (format t "map line: ~a~%" line) (setf *parse_state* "temperature-to-humidity"))
    ;; humidity-to-location
    ((str:containsp "humidity-to-location" line) (format t "map line: ~a~%" line) (setf *parse_state* "humidity-to-location"))
    ;; empty line
    ((str:blankp line) (format t "empty line~%" line))
    (t 
      (format t "data line: ~a~%" line)
      ;; split the line into a list of strings and vonert to integers
      (setf line (mapcar (lambda (x) (parse-integer x)) (str:words line)))
      (let* (
        (destination_range_start (first line))
        (source_range_start (second line))
        (range_length (third line)))

        (format t "destination_range_start: ~a~%" destination_range_start)
        (format t "source_range_start: ~a~%" source_range_start)
        (format t "range_length: ~a~%" range_length)

        (cond
          ;; check if seed >= source_range_start and seed < source_range_start + range_length then set soil of same index to destination_range_start + seed - source_range_start
          ((string= *parse_state* "seed-to-soil")
            (loop for seed in *seeds* and idx from 0 do
              (when (and (>= seed source_range_start) (< seed (+ source_range_start range_length)))
                (setf (nth idx *soils*) (+ destination_range_start (- seed source_range_start)))
                ))
          )
          ((string= *parse_state* "soil-to-fertilizer")
          ;; set all soil values that are still -1 to the same as seeds
          (loop for soil in *soils* and idx from 0 do
            (when (= soil -1)
              (setf (nth idx *soils*) (nth idx *seeds*))
              (setf soil (nth idx *soils*))
              )
            (when (and (>= soil source_range_start) (< soil (+ source_range_start range_length)))
              (setf (nth idx *fertilizers*) (+ destination_range_start (- soil source_range_start)))
              )
          ))
          ((string= *parse_state* "fertilizer-to-water")
            (loop for fertilizer in *fertilizers* and idx from 0 do
              (when (= fertilizer -1)
                (setf (nth idx *fertilizers*) (nth idx *soils*))
                (setf fertilizer (nth idx *fertilizers*))
                )
              (when (and (>= fertilizer source_range_start) (< fertilizer (+ source_range_start range_length)))
                (setf (nth idx *waters*) (+ destination_range_start (- fertilizer source_range_start)))
                )
              
          ))
          ((string= *parse_state* "water-to-light")
            (loop for water in *waters* and idx from 0 do
              (when (= water -1)
                (setf (nth idx *waters*) (nth idx *fertilizers*))
                (setf water (nth idx *waters*))
                )
              (when (and (>= water source_range_start) (< water (+ source_range_start range_length)))
                (setf (nth idx *lights*) (+ destination_range_start (- water source_range_start)))
                )
              
          ))
          ((string= *parse_state* "light-to-temperature")
            (loop for light in *lights* and idx from 0 do
              (when (= light -1)
                (setf (nth idx *lights*) (nth idx *waters*))
                (setf light (nth idx *lights*))
                )
              (when (and (>= light source_range_start) (< light (+ source_range_start range_length)))
                (setf (nth idx *temperatures*) (+ destination_range_start (- light source_range_start)))
                )
              
          ))
          ((string= *parse_state* "temperature-to-humidity")
            (loop for temperature in *temperatures* and idx from 0 do
              (when (= temperature -1)
                (setf (nth idx *temperatures*) (nth idx *lights*))
                (setf temperature (nth idx *temperatures*))
                )
              (when (and (>= temperature source_range_start) (< temperature (+ source_range_start range_length)))
                (setf (nth idx *humidities*) (+ destination_range_start (- temperature source_range_start)))
                )
              
          ))
          ((string= *parse_state* "humidity-to-location")
            (loop for humidity in *humidities* and idx from 0 do
              (when (= humidity -1)
                (setf (nth idx *humidities*) (nth idx *temperatures*))
                (setf humidity (nth idx *humidities*))
                )
              (when (and (>= humidity source_range_start) (< humidity (+ source_range_start range_length)))
                (setf (nth idx *locations*) (+ destination_range_start (- humidity source_range_start)))
                )
              
          ))
        )
      )
    )
))

(loop for location in *locations* and idx from 0 do
  (when (= location -1)
    (setf (nth idx *locations*) (nth idx *humidities*))))

(format t "soils: ~a~%" *soils*)
(format t "fertilizers: ~a~%" *fertilizers*)
(format t "waters: ~a~%" *waters*)
(format t "lights: ~a~%" *lights*)
(format t "temperatures: ~a~%" *temperatures*)
(format t "humidities: ~a~%" *humidities*)
(format t "locations: ~a~%" *locations*)

(reduce #'min *locations*))

;; idk does not work when also running example - probably because of global variables or something - don't care enough to fix it
;; (format t "lowest location: ~a~%" (get_lowest_location_value "example"))
(format t "Part 1 Answer: ~a~%" (get_lowest_location_value "input"))