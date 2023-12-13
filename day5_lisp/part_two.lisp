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

(defvar *seed_range_buffer* nil)

(let* ((first_line (last *lines*))
      ;; first line is CONS type, so we need to convert it to a string
      (first_line (str:join "" first_line)))
  ;; split the string into a list of strings
  (let* ((split_line (str:split ":" first_line))
          ;; only keep the second element of the list
          (split_line (second split_line)))
    (let ((seeds (str:words split_line)))
      (setf seeds (mapcar (lambda (seed) (parse-integer seed)) seeds))
      (loop for seed in seeds and idx from 0 do
        ;; when seed is even or zero then add to *seed_range_buffer* when seed is odd then add to *seed_range_buffer* and add the range to *seeds*
        (if (or (zerop (mod idx 2)) (= idx 0))
          (setq *seed_range_buffer* (append *seed_range_buffer* (list seed)))
            (progn
              (setq *seed_range_buffer* (append *seed_range_buffer* (list seed)))
              (setf *seeds* (append *seeds* (list (cons (first *seed_range_buffer*) (second *seed_range_buffer*)))))
              (setf *seed_range_buffer* nil)
            )
        )
      )
    )
  )
)

(loop for seed in *seeds* and idx from 0 do
  ;; convert (seed_start seed_length) to (seed_start seed_end)
  (setf (nth idx *seeds*) (cons (car seed) (+ (car seed) (cdr seed))))
)

(format t "seeds: ~a~%" *seeds*)

(defvar *soils* nil)
(defvar *fertilizers* nil)
(defvar *waters* nil)
(defvar *lights* nil)
(defvar *temperatures* nil)
(defvar *humidities* nil)
(defvar *locations* nil)

(defvar *parse_state* "")

;; remove last line from *lines*
(setf *lines* (butlast *lines*))

(defvar idx 0)
(defvar *is_loop_break* 0)

;; loop over the lines
(loop for line in (reverse *lines*) do
  ;; check if line contains "seed-to-soil"
  (cond
    ;; seed-to-soil
    (
      (str:containsp "seed-to-soil" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "seed-to-soil")
    )
    ;; soil-to-fertilizer
    (
      (str:containsp "soil-to-fertilizer" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "soil-to-fertilizer")
      (loop for seed in *seeds* do
          ;; when entry exists and both values are not the same
          (when (and seed (not (= (car seed) (cdr seed))))
              (setq *soils* (append *soils* (list seed)))
          )
      )
    )
    ;; fertilizer-to-water
    (
      (str:containsp "fertilizer-to-water" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "fertilizer-to-water")
      (loop for soil in *soils* do
          ;; when entry exists and both values are not the same
          (when (and soil (not (= (car soil) (cdr soil))))
              (setq *fertilizers* (append *fertilizers* (list soil)))
          )
      )
    )
    ;; water-to-light
    (
      (str:containsp "water-to-light" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "water-to-light")
      (loop for fertilizer in *fertilizers* do
          ;; when entry exists and both values are not the same
          (when (and fertilizer (not (= (car fertilizer) (cdr fertilizer))))
              (setq *waters* (append *waters* (list fertilizer)))
          )
      )
    )
    ;; light-to-temperature
    (
      (str:containsp "light-to-temperature" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "light-to-temperature")
      (loop for water in *waters* do
          ;; when entry exists and both values are not the same
          (when (and water (not (= (car water) (cdr water))))
              (setq *lights* (append *lights* (list water)))
          )
      )
    )
    ;; temperature-to-humidity
    (
      (str:containsp "temperature-to-humidity" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "temperature-to-humidity")
      (loop for light in *lights* do
          ;; when entry exists and both values are not the same
          (when (and light (not (= (car light) (cdr light))))
              (setq *temperatures* (append *temperatures* (list light)))
          )
      )
    )
    ;; humidity-to-location
    (
      (str:containsp "humidity-to-location" line)
      ;; (format t "map line: ~a~%" line)
      (setf *parse_state* "humidity-to-location")
      (loop for temperature in *temperatures* do
          ;; when entry exists and both values are not the same
          (when (and temperature (not (= (car temperature) (cdr temperature))))
              (setq *humidities* (append *humidities* (list temperature)))
          )
      )
    )
    ;; empty line
    (
      (str:blankp line)
      ;; (format t "empty line~%" line)
    )
    (t 
      ;; (format t "data line: ~a~%" line)
      ;; split the line into a list of strings and vonert to integers
      (setf line (mapcar (lambda (x) (parse-integer x)) (str:words line)))
      (let* (
        (destination_range_start (first line))
        (source_range_start (second line))
        (range_length (third line))
        (destination_range_end (+ destination_range_start range_length))
        (source_range_end (+ source_range_start range_length)))
        (setq idx 0)
        (setq *is_loop_break* 0)
        (cond
          ;; check if seed >= source_range_start and seed < source_range_start + range_length then set soil of same index to destination_range_start + seed - source_range_start
          ((string= *parse_state* "seed-to-soil")
              (loop while (= *is_loop_break* 0) do
                (when (nth idx *seeds*)
                  (let* (
                      (seed (nth idx *seeds*))
                      (seed_start (car seed))
                      (seed_end (cdr seed))
                      (partial_source_start (max seed_start source_range_start))
                      (partial_source_end (min seed_end source_range_end))

                      (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                      (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                    )
                    (when (< partial_source_start partial_source_end)
                      (setq *soils* (append *soils* (list (cons partial_destination_start partial_destination_end))))
                        
                      ;; set current seed to nil
                      (setf (nth idx *seeds*) nil)

                      ;; when start smaller end and not already in list then add to list
                      (when (and (< seed_start partial_source_start) (not (member (cons seed_start partial_source_start) *seeds*)))
                        (setq *seeds* (append *seeds* (list (cons seed_start partial_source_start))))
                      )

                      (when (and (< partial_source_end seed_end) (not (member (cons partial_source_end seed_end) *seeds*)))
                        (setq *seeds* (append *seeds* (list (cons partial_source_end seed_end))))
                      )
                    )

                  )
                )

                (setq idx (+ idx 1))
                (when (= idx (length *seeds*))
                    (setq *is_loop_break* 1)
                )
            ))
          ((string= *parse_state* "soil-to-fertilizer")
            (format t "soils: ~a~%" *soils*)
            ;; soilds to fertilizers
            (loop while (= *is_loop_break* 0) do
              (when (nth idx *soils*)
                (let* (
                    (soil (nth idx *soils*))
                    (soil_start (car soil))
                    (soil_end (cdr soil))
                    (partial_source_start (max soil_start source_range_start))
                    (partial_source_end (min soil_end source_range_end))

                    (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                    (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                  )
                  (when (< partial_source_start partial_source_end)
                    (setq *fertilizers* (append *fertilizers* (list (cons partial_destination_start partial_destination_end))))
                      
                    ;; set current soil to nil
                    (setf (nth idx *soils*) nil)
                    
                    ;; when start smaller end and not already in list then add to list
                    (when (and (< soil_start partial_source_start) (not (member (cons soil_start partial_source_start) *soils*)))
                      (setq *soils* (append *soils* (list (cons soil_start partial_source_start))))
                    )

                    (when (and (< partial_source_end soil_end) (not (member (cons partial_source_end soil_end) *soils*)))
                      (setq *soils* (append *soils* (list (cons partial_source_end soil_end))))
                    )

                  )

                )
              )

              (setq idx (+ idx 1))
              (when (= idx (length *soils*))
                  (setq *is_loop_break* 1)
              )
            ))
          ((string= *parse_state* "fertilizer-to-water")
            (format t "fertilizers: ~a~%" *fertilizers*)
            ;; fertilizers to waters
            (loop while (= *is_loop_break* 0) do
              (when (nth idx *fertilizers*)
                (let* (
                    (fertilizer (nth idx *fertilizers*))
                    (fertilizer_start (car fertilizer))
                    (fertilizer_end (cdr fertilizer))
                    (partial_source_start (max fertilizer_start source_range_start))
                    (partial_source_end (min fertilizer_end source_range_end))

                    (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                    (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                  )
                  (when (< partial_source_start partial_source_end)
                    (setq *waters* (append *waters* (list (cons partial_destination_start partial_destination_end))))
                      
                    ;; set current fertilizer to nil
                    (setf (nth idx *fertilizers*) nil)

                    ;; when start smaller end and not already in list then add to list
                    (when (and (< fertilizer_start partial_source_start) (not (member (cons fertilizer_start partial_source_start) *fertilizers*)))
                      (setq *fertilizers* (append *fertilizers* (list (cons fertilizer_start partial_source_start))))
                    )

                    (when (and (< partial_source_end fertilizer_end) (not (member (cons partial_source_end fertilizer_end) *fertilizers*)))
                      (setq *fertilizers* (append *fertilizers* (list (cons partial_source_end fertilizer_end))))
                    )
                  )
                )
              )
              (setq idx (+ idx 1))
              (when (= idx (length *fertilizers*))
                  (setq *is_loop_break* 1)
              )
            ))
          ((string= *parse_state* "water-to-light")
            (format t "waters: ~a~%" *waters*)
            ;; waters to lights
            (loop while (= *is_loop_break* 0) do
              (when (nth idx *waters*)
                (let* (
                    (water (nth idx *waters*))
                    (water_start (car water))
                    (water_end (cdr water))
                    (partial_source_start (max water_start source_range_start))
                    (partial_source_end (min water_end source_range_end))

                    (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                    (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                  )
                  (when (< partial_source_start partial_source_end)
                    (setq *lights* (append *lights* (list (cons partial_destination_start partial_destination_end))))
                      
                    ;; set current water to nil
                    (setf (nth idx *waters*) nil)
                    
                    ;; when start smaller end and not already in list then add to list
                    (when (and (< water_start partial_source_start) (not (member (cons water_start partial_source_start) *waters*)))
                      (setq *waters* (append *waters* (list (cons water_start partial_source_start))))
                    )

                    (when (and (< partial_source_end water_end) (not (member (cons partial_source_end water_end) *waters*)))
                      (setq *waters* (append *waters* (list (cons partial_source_end water_end))))
                    )
                  )
                )
              )
              (setq idx (+ idx 1))
              (when (= idx (length *waters*))
                  (setq *is_loop_break* 1)
              )
            ))
          ((string= *parse_state* "light-to-temperature")
            (format t "lights: ~a~%" *lights*)
            ;; lights to temperatures
            (loop while (= *is_loop_break* 0) do
              (when (nth idx *lights*)
                (let* (
                    (light (nth idx *lights*))
                    (light_start (car light))
                    (light_end (cdr light))
                    (partial_source_start (max light_start source_range_start))
                    (partial_source_end (min light_end source_range_end))

                    (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                    (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                  )
                  (when (< partial_source_start partial_source_end)
                    (setq *temperatures* (append *temperatures* (list (cons partial_destination_start partial_destination_end))))
                      
                    ;; set current light to nil
                    (setf (nth idx *lights*) nil)
                    
                    ;; when start smaller end and not already in list then add to list
                    (when (and (< light_start partial_source_start) (not (member (cons light_start partial_source_start) *lights*)))
                      (setq *lights* (append *lights* (list (cons light_start partial_source_start))))
                    )

                    (when (and (< partial_source_end light_end) (not (member (cons partial_source_end light_end) *lights*)))
                      (setq *lights* (append *lights* (list (cons partial_source_end light_end))))
                    )
                  )
                )
              )
              (setq idx (+ idx 1))
              (when (= idx (length *lights*))
                  (setq *is_loop_break* 1)
              )
            ))
          ((string= *parse_state* "temperature-to-humidity")
            (format t "temperatures: ~a~%" *temperatures*)
            ;; temperatures to humidities
            (loop while (= *is_loop_break* 0) do
              (when (nth idx *temperatures*)
                (let* (
                    (temperature (nth idx *temperatures*))
                    (temperature_start (car temperature))
                    (temperature_end (cdr temperature))
                    (partial_source_start (max temperature_start source_range_start))
                    (partial_source_end (min temperature_end source_range_end))

                    (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                    (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                  )
                  (when (< partial_source_start partial_source_end)
                    (setq *humidities* (append *humidities* (list (cons partial_destination_start partial_destination_end))))
                      
                    ;; set current temperature to nil
                    (setf (nth idx *temperatures*) nil)

                    ;; when start smaller end and not already in list then add to list
                    (when (and (< temperature_start partial_source_start) (not (member (cons temperature_start partial_source_start) *temperatures*)))
                      (setq *temperatures* (append *temperatures* (list (cons temperature_start partial_source_start))))
                    )

                    (when (and (< partial_source_end temperature_end) (not (member (cons partial_source_end temperature_end) *temperatures*)))
                      (setq *temperatures* (append *temperatures* (list (cons partial_source_end temperature_end))))
                    )
                    
                  )
                )
              )
              (setq idx (+ idx 1))
              (when (= idx (length *temperatures*))
                  (setq *is_loop_break* 1)
              )
            ))
          ((string= *parse_state* "humidity-to-location")
            (format t "humidities: ~a~%" *humidities*)
            ;; humidities to locations
            (loop while (= *is_loop_break* 0) do
              (when (nth idx *humidities*)
                (let* (
                    (humidity (nth idx *humidities*))
                    (humidity_start (car humidity))
                    (humidity_end (cdr humidity))
                    (partial_source_start (max humidity_start source_range_start))
                    (partial_source_end (min humidity_end source_range_end))

                    (partial_destination_start (+ destination_range_start (- partial_source_start source_range_start)))
                    (partial_destination_end (+ destination_range_end (- partial_source_end source_range_end)))
                  )
                  (when (< partial_source_start partial_source_end)
                    (setq *locations* (append *locations* (list (cons partial_destination_start partial_destination_end))))
                      
                    ;; set current humidity to nil
                    (setf (nth idx *humidities*) nil)

                    ;; when start smaller end and not already in list then add to list
                    (when (and (< humidity_start partial_source_start) (not (member (cons humidity_start partial_source_start) *humidities*)))
                      (setq *humidities* (append *humidities* (list (cons humidity_start partial_source_start))))
                    )
                    
                    (when (and (< partial_source_end humidity_end) (not (member (cons partial_source_end humidity_end) *humidities*)))
                      (setq *humidities* (append *humidities* (list (cons partial_source_end humidity_end))))
                    )
                  )
                )
              )
              (setq idx (+ idx 1))
              (when (= idx (length *humidities*))
                  (setq *is_loop_break* 1)
              )
            ))
        )
      )
    )
))

(loop for humidity in *humidities* do
    ;; when entry exists and both values are not the same
    (when (and humidity (not (= (car humidity) (cdr humidity))))
        (setq *locations* (append *locations* (list humidity)))
    )
)

(format t "locations: ~a~%" *locations*)

;; find lowest location value
;; assume that the start index is lower than the end index
(defvar *lowest_location_value* -1)
(loop for location in *locations* do
  (let* (
      (location_start (car location))
    )
    ;; when -1 then set to location_start
    (when (= *lowest_location_value* -1)
      (setq *lowest_location_value* location_start)
    )
    ;; when location_start < *lowest_location_value* then set to location_start
    (when (< location_start *lowest_location_value*)
      (setq *lowest_location_value* location_start)
    )
  )
)

return *lowest_location_value*
)

;; idk does not work when also running example - probably because of global variables or something - don't care enough to fix it
;; (format t "lowest location: ~a~%" (get_lowest_location_value "example"))
(format t "Part 1 Answer: ~a~%" (get_lowest_location_value "input"))