(ns insane-noises.loseq
  (:require [overtone.api])
  (:use
   [overtone.live] ;; for the internal sc server
   ;;   [overtone.core]
   )
  )


;; * dnb and amen beats
;; here i want some simple drum machine code to play with.

;; you can find lots of drum patterns on wikipedia, and you can convert them rather easily to lisp constructs.

;; then i combined with my other instruments to to form ... something.

;; reverse a beat: (map #(list (first %) (reverse (second %)) ) amen-beat)
;; #+BEGIN_SRC clojure

;;the dummy beat thing is a workaround, because i really wanted some kind of forward declaration
(def dummy-beat)
(def *beat (ref dummy-beat))
(def *beat-count (ref 0))
(defonce *metro (ref (metronome 240)))
(def *il (ref 0));;default interleave

(defn set-metro [& {:keys [bpm il]
                         :or {bpm 160
                              il 0}}]
  (dosync (ref-set *il il))

  (@*metro :bpm (* (+ 1 il) bpm))
  )

(defn set-beat 
  ([name beat] (intern 'insane-noises.core name beat) (set-beat beat))
  ([beat] (dosync (ref-set *beat beat))))



(def *drums (ref {}));;(ref my-drums))
(defn set-drums [drums]
  (dosync (ref-set *drums drums)))


                                        ;re-def works, but not ref-set?
;;(sync (ref-set *beat amen-beat))
;;(def *beat (ref amen-beat))
;; #+BEGIN_SRC clojure

(defn reverse-beat [pattern]
  "reverse PATTERN"
  (map #(list (first %) (reverse (second %)) ) pattern))

(defn drum [voice pattern]
  (dosync (alter *beat conj [voice pattern])))

(defn get-drum-fn [voice]
  "get the drum function for VOICE, from the current drum kit in *drums.
voice is either a symbol or a map. if symbol, just look up the symbol in the drumkit.
if map, use the :voice key to lookup the drum
"
       (if (map? voice)
           (get @*drums (:voice voice)) 
           (get @*drums voice))
  )

(comment
  (apply (get-drum-fn :B )['x])
  (apply(get-drum-fn {:voice :B})['x])
  (get-drum-il :B )
  (get-drum-il {:voice :B :il 1})
    (get-drum-il {:voice :B})
  )

(defn get-drum-il [voice]
  "like get-drum-fn, but get the voice interleave instead. 
"
       (if (map? voice)
         (or  (:il voice) @*il)
           @*il)
  )

(defn clear-drums []
  "clear out the drum kit"
  (dosync (ref-set *beat [])))

(defn il [num seq]
    "interleave sequences with nops. for tempo experiments."
  (mapcat (fn [x] (concat (list x) (repeat num '-) )) seq))

(defn drum-fn [beat beat-count]
  "take a vertical slice out of BEAT, at BEAT-COUNT, and play this slice
allows for voice patterns of different length
undefined voices are dropped"
  (doseq [[voice pattern] beat] ;; map over each voice/pattern pair
    (let* [pattern-il (get-drum-il voice)
           pattern (il pattern-il pattern) ;; interleave
           index (mod beat-count (count pattern) ) ;; *beat-count is global counter, make index modulo pattern length
           drum (get-drum-fn voice)] ;;figure out the drum function for the voice
      (if (and drum (not (= '- (nth pattern index)))) ;;play the drum if there a) is a drum and b) the pattern contains something that isnt "-"
        (do
          (try
            (let [nthpattern (nth pattern  index)]
              (if (sequential? nthpattern);; the drum arg can be a list or a single thing
                (apply drum nthpattern)
                (apply drum [nthpattern]))
              )
            ;; since its not really possible atm to guarantee that the stuff in the sequence is compatible with the drum function,
            ;; errors are simply catched and ignored. otherwise the entire sequence stops which i dont want
            (catch Exception e (println "uncompatible drum fn"))
            ) 
          
          )
        )
      )))

(defn drum-fn-globalbeat []
  (drum-fn  @*beat @*beat-count)
  
  )

(defn play-metro2 [m beat-num]
   "start playing drums, using m as metro"
   ;;play drums using a metronome strategy, which has advantages over the periodic strategy
   ;; 1st reschedule next call to the sequencer
  (apply-at (m (+ 1 beat-num))
            play-metro2
            m
            (+ 1 beat-num)
            [])
  ;; 2nd schedule the drum voice
  (at (m beat-num)(drum-fn-globalbeat))
   ;;3d step global counters
  (dosync (ref-set *beat-count (inc @*beat-count) ))
  )
;;(defonce metro  (metronome 240))
;;(stop)
;;(play-metro-old metro (metro))
;;(play-metro-old @*metro (@*metro))

(defn play-metro []
  (play-metro2 @*metro (@*metro)))

;;(play-metro)
;;    (drum-set-metro :bpm 250)
;;(stop)

(defn beat-max-len [beat]
  "the bars can be different lengths in a beat, so figure out the longest one"
  ;;TODO oh great. this doesnt account for the interleaves.
  ;;  (reduce max (map #(count %) (map (fn [key] (get beat key)) (keys beat))))
  (reduce max  (map (fn [key] (* (inc(get-drum-il key))(count(get beat key)))) (keys beat)))

  )
;;(get-drum-il {:voice :B :il 2}) 
;;(beat-max-len '{:B [1 2] :C [1 2 3]})


(defn play-once2 [m beat-num beat count-cur count-end]
  "start playing drums, using m as metro. only play them once, unlike the sister function."
  ;;count-cur is normally 0 at the outset, and count-end the count of the longest beat bar
  ;;initially i tried using "loop/recur", and "at" instead of temporal recursion, but that didnt work for some reason
  ;;which i find odd.
  (if (> count-end count-cur) ;;limit the number of calls to the sequence length
    (do
      (println count-cur count-end)
      (apply-at (m (inc beat-num))
                play-once2
                m
                (inc beat-num)
                beat
                (inc count-cur)
                count-end
                [])
      ;; 2nd schedule the drum voice
      (at (m beat-num)(drum-fn beat count-cur))

      )
    )
  

  )



(defn play-once [beat]
  (play-once2 @*metro (@*metro) beat 0 (beat-max-len beat))

  )

(declare mk-arpeggio-pattern)
;; (beat-max-len {:A '[a b] :B '[c d e] :C '[d]})
;; (beat-max-len (mk-arpeggio-pattern :BL  '(1 - 2 2 2 1 3) (chord-degree :iii :d4 :ionian) 2 ))

(defn play-chord [a-chord voice]
  (doseq [note a-chord] (voice note)))

;; now arpeggios, whatever they are
;;this is an arpeggiator i think

(defn mk-arpeggio-pattern [voice seq notes il]
  ;;convert a list of notes(a chord, likely) and a seq of numbers, to a beat pattern
  ;; ( a b c) (1 - 2 3 2 1) :B -> {:B [a - b c b a]}
  { {:voice voice :il il}
   (into [] (map #(if (= '- %) '- (nth notes %)) seq  ))
   }
  )
;;(mk-arpeggio-pattern :B  '(1  2 2 2 1) '( a b c) 3)
;;(mk-arpeggio-pattern :BL  '(1  2 2 2 1 3) (chord-degree :iii :d4 :ionian)  3)

;; dashes should also be allowed, doesnt work atm
;; (mk-arpeggio-pattern :BL  '(1 - 2 2 2 1 3) (chord-degree :iii :d4 :ionian) 2 )

;; (play-once (mk-arpeggio-pattern :F  '(1  3 2 1 3) (chord-degree :iii :d4 :ionian) 0  ))

;; TODO the interleave feature is cool, but unpredictable. i guess the base interleave should always be the same
;; otherwise the behaviour of pattern changes when the interleave changes
;; also play-once doesnt seem to play all notes, except if il is 0

(defn play-arpeggio [voice seq notes il]
  ;;this is much like the other drum seq routines i guess
  (play-once (mk-arpeggio-pattern voice  seq notes il ))

  )
;;(play-arpeggio :BL  '(0 1 2 3 2 1 ) (chord-degree :i :d4 :major) 1 )
;;(play-arpeggio :BL  '(0 1 2 3 ) (chord-degree :i :d4 :minor) 1 )
;;(play-arpeggio :BL  '(0 1 2 3 ) (chord-degree :i :d4 :diminished) 1 )
