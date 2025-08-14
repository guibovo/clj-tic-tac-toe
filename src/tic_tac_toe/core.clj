(ns tic-tac-toe.core
  (:gen-class))

(defn new-board
  "Creates a new tic-tac-toe board"
  []
  (vec (repeat 3 (vec (repeat 3 nil)))))

(def board (atom (new-board)))
(def players [:O :X])
(def current-player (atom (get players 0)))

(defn valid-move?
  [board line column]
  (and (<= line 2) (>= line 0) (<= column 2) (>= column 0) (nil? (get-in board [line column]))))

(defn is-valid-input? 
  [input]
  (boolean (re-matches (re-pattern "[0-2]") input)))

(defn read-value
  [player selector]
  (println "Player" player "choose the desired" selector "(0-2):")
  (let [input (read-line)]
    (println)
    (if (is-valid-input? input)
      (read-string input)
      (do
        (println "Invalid value, please try again.")
        (read-value player selector)))))

(defn make-move
  "Makes a move in the game setting the player symbol to the desired position if possible"
  [board player]
  (let [line (read-value player "line")
        column (read-value player "column")]
    (if (valid-move? board line column)
      (assoc-in board [line column] player)
      (do
        (println "Invalid position, please try again.")
        (make-move board player)))))

(defn board-full?
  [board]
  (not (some nil? board)))

(defn validate-values
  [values]
  (if (not (some #(= nil %) values))
   (if (apply = values)
    (get values 0))))

(defn validate-lines
  [board]
  (for [line board]
     (validate-values line)))

(defn validate-columns
  [board]
  (let [columns-values 
        (map-indexed
          (fn [line-idx _]
            (map-indexed
              (fn [_ column-values]
                (get column-values line-idx)))
            board)
          board)]
    (for [column columns-values]
      (validate-values (vec column)))))

(defn validate-diagonals
  [board]
  (let [board-size (count board)
        first-diagonal
          (vec
             (for [y (range) :while (< y board-size)]
               (get-in board [y y])))
          second-diagonal
        (vec
          (for [column (range) :while (< column board-size)]
            (let [line (- board-size (+ column 1))]
              (get-in board [line column]))))]
    (for [diagonal [first-diagonal second-diagonal]]
      (validate-values diagonal))))

(defn get-winner
  [results]
  (get
    (vec
      (filter #(not (nil? %)) results))
    0))

(defn winner?
  [board]
  (let [lines-results (validate-lines board)
        columns-results (validate-columns board)
        diagonals-results (validate-diagonals board)]
    (get-winner (flatten [columns-results lines-results diagonals-results]))))

(defn print-line
  [line]
  (println (get line 0) "|" (get line 1) "|" (get line 2)))

(defn print-board
  [board]
  (let [line_one (get board 0)
        line_two (get board 1)
        line_three (get board 2)]
    (print-line line_one)
    (println "--------------")
    (print-line line_two)
    (println "--------------")
    (print-line line_three)))

(defn reset-terminal-text
  []
  (print (str (char 27) "[2J"))  
  (print (str (char 27) "[;H")))

(defn -main
  "Starts tic-tac-toe"
  []
  (loop [board (new-board) player :O]
    (reset-terminal-text)
    (print-board board)
    (if-let [w (winner? board)]
      (println "Winner is" w)
      (let [next-board (make-move board player)]
        (recur next-board (if (= player :O) :X :O))))))

