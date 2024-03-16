#!/usr/bin/env bb

(require '[babashka.process :refer [shell process exec]])
(require '[taoensso.timbre :as timbre])
(require '[babashka.cli :as cli])
(require '[babashka.fs :as fs])

;;python -m bark --text "Hello, my name is Suno." --output_filename "Downloads/example6.wav" --history_prompt "v2/it_speaker_4"


;;(doall (map (fn [x] (shell   "python" "-m" "bark" "--text"  x "--history_prompt" "v2/en_speaker_9" "--output_filename"   )) lines))

;;(require '[clojure.string :as str])
(defn format-string-as-filename [counter text]
  (str (format "%02d" counter) "_"
       (-> text
           (str/replace #" " "_") ; replace spaces with underscores
           (str/replace #"[^A-Za-z0-9_]" "")))) ; remove special characters

(defn read-text [textfile]
  (with-open [rdr (clojure.java.io/reader textfile)]
    (doall (line-seq rdr))))

(defn bark [line voice path counter]
  (let [cmd (str "python -m  bark --text \""  line "\" --history_prompt " voice " --output_filename " (str path "/" (format-string-as-filename counter line) ".wav"))]
    (timbre/info cmd)
    (if (not (= "" line))
      (shell {:env {"VIRTUAL_ENV" "/mnt/big/roles/ai/mybark/bark20240221/venv"
                    "PATH" "/mnt/big/roles/ai/mybark/bark20240221/venv/bin" }} cmd  ))
    )
  )

(defn process-text [textfile voice path]
  (let [lines  (read-text textfile)]
       
       (loop [lines lines counter 1]
         (when-not (empty? lines) 
           (bark (first lines) voice path counter)
           (recur (rest lines) (inc counter)))
         
         )))

(defn process-sentences [filename voice path]
  (let [sentences (str/split (slurp filename) #"\.|\?|\:|\n\n")]
    (doseq [[index sentence] (map-indexed vector (filter #(not (= "" %)) sentences))]
      (let [stripped-sentence (-> sentence
                                  (str/replace  #"\n" " ")
                                  (str/replace  #"\*" "")
                                  (str/trim))]
        (timbre/info (format "%02d" index) stripped-sentence)
        (bark stripped-sentence voice path index)
        )
      )
    )

  )

(defn -main [args]
  (timbre/info "starting")
  (fs/create-dirs (:path args))
  (cond 
    (:lines args ) (process-text (:textfile args) (:voice args) (:path args))
    (:sentences args)  (process-sentences (:textfile args) (:voice args) (:path args))
    )
  (timbre/info "stoping")
  )

(-main (cli/parse-opts *command-line-args* {:require [:textfile :voice :path]}))

;;https://suno-ai.notion.site/8b8e8749ed514b0cbf3f699013548683?v=bc67cff786b04b50b3ceb756fd05f68c
;;bb bark_driver.bb --textfile test.txt --voice "v2/en_speaker_9" --path out
