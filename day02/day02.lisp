(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :str)

(defparameter *test-input* "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defun parse-input (lines)
  (loop for line in (str:lines lines)
        collect (ppcre:register-groups-bind (game-id rest) ("Game (\\d+): (.*)" line)
                  (let ((game-id (parse-integer game-id))
                        (sets (loop for set in (str:split "; " rest)
                                    collect (let ((counts (str:split ", " set)))
                                              (mapcar (lambda (count)
                                                        (let ((pair (str:split #\space count)))
                                                          (list (alexandria:make-keyword (string-upcase (second pair)))
                                                                (parse-integer (first pair)))))
                                                      counts)))))
                    (list :game-id game-id
                          :sets sets)))))

(defun number-of-color (color set)
  (or (second (assoc color set)) 0))

(defun game-impossible-p (game)
  (let ((sets (getf game :sets)))
    (some (lambda (set)
            (or (> (number-of-color :red set) 12)
                (> (number-of-color :green set) 13)
                (> (number-of-color :blue set) 14)))
          sets)))

(defun fewest-cubes-for-game (game)
  (let ((sets (getf game :sets)))
    (loop for set in sets
          maximize (number-of-color :red set) into red
          maximize (number-of-color :green set) into green
          maximize (number-of-color :blue set) into blue
          finally (print (list red green blue)))))

(defparameter *games* (parse-input (alexandria:read-file-into-string "input.txt")))

;; part 1
(loop for game in *games*
      unless (game-impossible-p game)
        sum (getf game :game-id) into id-sum
      finally (print id-sum))

;; part 2
(loop for game in *games*
      for sets = (getf game :sets)
      sum (loop for set in sets
                maximize (number-of-color :red set) into red
                maximize (number-of-color :green set) into green
                maximize (number-of-color :blue set) into blue
                finally (return (* red green blue)))
        into total-sum
      finally (print total-sum))
