(ns sylt.loseq
  (:require [overtone.api])
  (:use
   [overtone.live] ;; for the internal sc server
   )
  )

;; * LoSeq
;; A simple sequencer for Overtone
;; LoSeq is a low level sequencer.
;; Think of it as amusing quirky low-level hardware sequencer rather than a high level notation editor

;; drum  - while it can be a drum, its actually a function, also called voice
;; drums - a "drumkit" of functions
;; beat  - a pattern on which the drumkit is applied
;; interleave - add noops between events


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
  ([name beat] (intern 'sylt.core name beat) (set-beat beat))
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


(defn drum-identity-transform [beat]
  ;; just return what we got
  beat
  )

(defn drum-mute-transform [beat]
  ;; just return nothing
  )

(defn get-drum-transform [voice]
  (or (:transform voice) 'drum-identity-transform)
  )

(defn apply-drum-transform [transform beat]
  ;;(println beat)
  beat;TODO imlement finding out maybe a transform for this beat
  )


(defn clear-drums []
  "clear out the drum kit"
  (dosync (ref-set *beat [])))

(defn il [num seq]
    "interleave sequences with nops. for tempo experiments."
  (mapcat (fn [x] (concat (list x) (repeat num '-) )) seq))

(def *muted-voices (atom {}))

(defn set-mute-voice-n [n val]
  (reset! *muted-voices
          (assoc @*muted-voices
                 (nth (keys @*beat) (dec n))
                 val)))

(defn get-mute-voice-n [n]
  (get @*muted-voices
       (nth (keys @*beat) (dec n))
         ))

  (defn get-mute-voice [voice]
    (get @*muted-voices
         voice
         ))
  
  (defn toggle-mute-voice-n [n]
    (set-mute-voice-n n (not(get-mute-voice-n n)))
    )

  (defn clear-mute []
    (reset! *muted-voices {}))

(comment

  (set-mute-voice-n 5 true)
  (get-mute-voice-n 5 )
  
  
  (toggle-mute-voice-n 0)
  (toggle-mute-voice-n 1)  
  (clear-mute)
  )


(defn beat-eval [strct]
  "go through BEAT, if it contains lambdas, eval them"
  (let [eval-value (fn [v]
                     (if (fn? v)
                       (v)
                       v))]
    (reduce-kv (fn [m k v]
                 (assoc m k (eval-value v)))
               {}
               strct)))
;; Example Usage:
;; (def example-structure
;; {:voice :I,
;; :il 0,
;; :P '[1.0 1.1 1.2 1.0 1.1 1.12 0.9 1.12 1.3 0.1],
;; :PV '[:a2 :b2 :c3 :d3],
;; :computed (fn [] [:a2 :b2 :c3 :d3])})
;; (beat-eval example-structure)



(defn drum-fn [beat beat-count]
  "take a vertical slice out of BEAT, at BEAT-COUNT, and play this slice
allows for voice patterns of different length
undefined voices are dropped"
  (let [beat-evaled (beat-eval beat)]
    (doseq [[voice pattern] beat-evaled ;; 
            ] ;; map over each voice/pattern pair
      (let* [pattern (apply-drum-transform (get-drum-transform voice) pattern) ;;TODO
             
             pattern-il (get-drum-il voice)
             pattern (il pattern-il pattern) ;; interleave
             index (mod beat-count (count pattern) ) ;; *beat-count is global counter, make index modulo pattern length
             drum (get-drum-fn voice)] ;;figure out the drum function for the voice
        (if (and drum (not (get-mute-voice voice)) (not (= '- (nth pattern index)))) ;;play the drum if there a) is a drum and b) the pattern contains something that isnt "-"
          (do
            (try
              (let [nthpattern (nth pattern  index)]
                (if (sequential? nthpattern);; the drum arg can be a list or a single thing
                  (apply drum nthpattern)
                  (apply drum [nthpattern]))
                )
              ;; since its not really possible atm to guarantee that the stuff in the sequence is compatible with the drum function,
              ;; errors are simply catched and ignored. otherwise the entire sequence stops which i dont want
              (catch Exception e (println "uncompatible drum fn" e))
              ) 
            
            )
          )
        ))))

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

;; * ramp
;; ramp from start to stop, in time-ms
;; ramp from start to stop, inc every 100ms
(defn ctl-ramp-2 [start stop inc ramp-fn]
  (let [cur (+ start inc)]
    (if (< start stop)
      (do
        ;;(println cur)
        (ramp-fn cur)
        ;;apply our ramp-fn, every 100 ms, until the ramp is done
        (apply-at (+ 100 (now)) ctl-ramp-2 [cur stop inc ramp-fn]  ))
      ;; else we are done and do naught
      ;;(println "done")
      )))

(defn ctl-ramp [start stop time-ms ramp-fn]
  (ctl-ramp-2 start stop
              (/ (- stop start) (/ time-ms 100.0))
              ramp-fn))


;; just test the ramp
;; (ctl-ramp 0 100 10000 #(println %))

