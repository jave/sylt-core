(require '[babashka.process :refer [shell process exec]])

(def lines (with-open [rdr (clojure.java.io/reader "/home/joakim/roles/music/overtone-sylt/in-the-engine-room.txt")]
                  (doall (line-seq rdr))) )

;(println lines)

(map (fn [x] (shell  "python" "bark_speak.py" "--use-smaller-models" "--text_prompt"  x "--history_prompt" "en_speaker_9")) lines)
