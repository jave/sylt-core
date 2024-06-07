#!/usr/bin/env bb

(require '[babashka.process :refer [shell process exec]])
(require '[taoensso.timbre :as timbre])
(require '[babashka.cli :as cli])
(require '[babashka.fs :as fs])

(require  '[clojure.edn :as edn] )
(require '[clojure.java.io :as io])

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

(def dryrun (atom  false))
(defn bark [line voice path counter]
  (let [outfile (str path "/" (format-string-as-filename counter line) ".wav")
        cmd (str "python -m  bark --text \""  line "\" --history_prompt " voice " --output_filename " outfile)]
    (timbre/info cmd)
    (if (not (or (fs/exists? outfile) @dryrun (= "" line)))
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

(defn read-edn-file [file]
  (-> file
      slurp
      edn/read-string))

;;TODO this should take a default struct as arg, then process-edn can interate a list of defaults in the edn instead
(defn process-edn-impl [ednfile default]
  (let [edn-content (read-edn-file ednfile)
        text (:text edn-content)
        ;;default (:default edn-content)
        ]

    (if (:path default)
      (fs/create-dirs (:path default) ))
    
    (doseq [x text] (bark (str (:prefix default)(:text x)(:postfix default))
                          (get x :voice (:voice default) )
                          (:path default) (:index x)))))

(defn process-edn [m]
  (doseq [default (:default (read-edn-file  (get-in m [:opts :file])))]
    (process-edn-impl
     (get-in m [:opts :file])
     ;;(get-in m [:opts :path])
     default
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

(defn help []
  (println 
   "--lines: input file is line oriented, each line is a bark run
--sentences: input file is sentence oriented, a sentence ends with a period or question mark
--edn: input file is edn
--voice: bark voice
--textfile: input file
--path: where to write samples

reasons not to call bark:
--dryrun: dont really call bark
empty lines
existing files

"))

(defn process-edn-dbg [m]
  (println m)
  (println "process-edn")
  (println (get-in m [:opts :file]))
  (println (get-in m [:opts :voice]))
  (println (get-in m [:opts :path]))  
  )
;; attempt to use dispatch
(def table
  [{:cmds ["text"]   :fn process-text :args->opts [:file :voice :path]
    :spec {:file {:require true}
           :voice {:require true}
           :path {:require true}}
    }
   {:cmds ["sentences"] :fn process-sentences :args->opts [:file :voice :path]
    :spec {:file {:require true}
           :voice {:require true}
           :path {:require true}}
    }
   {:cmds ["edn"] :fn process-edn
    :args->opts [:file ]
    :spec {:file {:require true}
           }
    }
   ;;help is last
   {:cmds ["help"]         :fn help}])


(defn -main []
  
  (timbre/info "starting")
  (if (:dryrun (cli/parse-opts *command-line-args*))
    (reset! dryrun true)
    (if (:path (cli/parse-opts *command-line-args*))
      (fs/create-dirs (:path (cli/parse-opts *command-line-args*)))))
  (cli/dispatch table *command-line-args* )
  (timbre/info "stoping")
  )

(-main)

