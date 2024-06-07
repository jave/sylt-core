;; * namespace declaration
;; boot the server
;; do this 1st manually, wait, and the cider-eval-buffer
;;(require '[overtone.live])
;; sleep so the server boots, before loading it with stuff
;;(require '[overtone.inst synth piano drum sampled-piano])
;;(demo (sampled-piano))
;;(Thread/sleep 10000)
;;now continue 
(ns sylt.core
  (:require [overtone.api]
            [overtone.live]
            [sylt.loseq :as seq]
            [overtone.inst.sampled-piano :refer :all]
            [overtone.inst.piano :refer :all])
  (:use
   [overtone.live] ;; overtone boots an external scsynth
   [overtone.inst synth piano drum sampled-piano]
   [overtone.examples.instruments space monotron]
   [sylt.inst]
   )
  )

;; just to see everything works and we get sound, rig connections with qjackctl
(demo (sin-osc))


;; * Song: Lexical Libation: In the Engine Room
;; ** preamble

(def curoffset (atom 0))

(defn mkoffset [loffset]
  (reset! curoffset (+ loffset @curoffset)))
;; load machine room babbling samples
(def babble-samples (load-samples  "/home/joakim/roles/music/overtone-sylt/sylt2023/bark_samples/*wav"))
(def truck-sample (load-samples "530052__legnalegna55__starting-truck-engine.wav"))
(definst truck [rate 0.5](play-buf :rate rate  :num-channels 1 :bufnum  (nth truck-sample 0) :loop false))
(defn ind2-state [state]
  (case state
    0 (do (clear-fx industrial2) (inst-volume! industrial2 1))
    1 (do  (def indch (inst-fx! industrial2 fx-chorus))   (inst-volume! industrial2 1.8))
    2 (do (inst-fx! industrial2 fx-reverb)(inst-volume! industrial2 0.8))
    3 (do (inst-fx! industrial2 fx-distortion-tubescreamer)(inst-volume! industrial2 3))
    ))
(defn auto-ind2 [state]
  (let [state (if (= state 1 ) 0 (inc state))]
    (println state)
    (ind2-state state)
    (apply-at (+ 8000 (now)) auto-ind2 [state])))

;; ** play the song

(def song-events {"init"
                  ;; start the industrial drone slow   ;; base bpm is 250, but since its an engine, i want to change bpm
                  (fn[](do (def d (industrial2 :bpm 0))
                          (truck)
                          (inst-volume! truck 3)
                          (seq/ctl-ramp 0 250 16000  #(ctl d :bpm %))
                          ;; start automatic mods of industrial
                          (auto-ind2 0) ))
                  "1" (fn[] (do
                             (reset! curoffset 0)
                             (def sw-babble (ab-player-sweep :dur 80 :bufnum (nth babble-samples 0)))
                             (inst-volume! ab-player-sweep 10)))
                  "2" (fn []
                        (inst-fx! ab-player-sweep fx-reverb)
                        )
                  "3" (fn[] (inst-fx! ab-player-sweep fx-echo) )
                  "4" (fn[] (clear-fx ab-player-sweep) )
                  "5" (fn[]
                        ;; rewind sample a bit
                        (ctl sw-babble :offset  (mkoffset -10000))  )})

(comment

  ;; TODO :dur should be set automatically somehow, now i have to figure it out by hand;;
  ;; investigate (:duration buf)   (:duration babble-samples)  (according to mail)


;;(ab-player-sweep :dur 1) i dont know but i had a orchestra hit type sample it sounded interesting when played really slow, at pace 80, at pace 1 you hear the original sound
(ctl sw-babble :offset  (mkoffset -10000))

(ctl sw-babble :trig 0)
(ctl sw-babble :trig 1)
;; pause sample
(ctl sw-babble :run 0)
;; restart sample
(ctl sw-babble :run 1)



  ;; variatons
  ;;reset drone engine to start values
  (ctl d :bpm 250 :wobble 1 :note 32 :snare-vol 1 :kick-vol 1 :v 1 :wobble-vol 1 :bass-vol 1)

  ;; control stuff inside the drone engine
  (ctl d :wobble 0.1)
  (ctl d :wobble-vol 0.05)
  (ctl d :kick-vol 0.05)
  (ctl d :bass-vol 0.02)
  (ctl d :snare-vol 0.05)
  (ctl d :note 40)
  (ctl d :note 20)

  (inst-volume! industrial2 1)
  ;; ramp
  (seq/ctl-ramp 0 1 16000  #(inst-volume! industrial2 %))

  ;; inst-fx! returns a node, that then can be ctl:d
  (ind2-state 0)



;;rate 0.002 depth 0.01
(ctl indch :depth 0.1)
(ctl indch :rate  0.002)


  (ctl ab :offset (mkoffset -10000))
  (ctl ab :offset 0)


  (definst babble-player []
    (ab-player))


  ;;weird way to rewind a sample, 1) set reverse playback, 2) wait using at, then play sample forward
  (do   (ctl b :rate -20)
        (at (+ (now) 100) (ctl b :rate 0.5))
          )


  (inst-fx! babble fx-reverb)
  (inst-fx! babble fx-echo)
  (inst-volume! babble 10)
  (clear-fx babble)
  (keys babble);;(:name :params :args :sdef :group :instance-group :fx-group :mixer :bus :fx-chain :volume :pan :n-chans)
  (get 0 (:ugens (:sdef babble)))
  (get-in babble [:sdef :ugens ] )
  (ctl babble :vol 1)
  )


;; * some basic tutorial code


;; sampled kick drum
;; from http://www.freesound.org/people/opm/sounds/2086/
;; the overtone freesound API allows you to download freesounds samples
;; by id (2086 in this case)

;; this conflicted, and then it wasnt cached


(def kick2086 (sample (frp 2086)))

(comment
  (kick)
  )


;; we can schedule beats for the future with the at macro:


(comment
  (at (+ 1000 (now)) (kick))
  )


;; ...and chain multiple beats together with a do form:

(comment
  (let
      [time (now)]
    (at (+    0 time) (kick) )
    (at (+  400 time) (hat)  )
    (at (+  800 time) (kick) )
    (at (+ 1200 time) (hat)  ))
  )

;; to repeat, we use the apply-at macro to schedule a recursive call
;; for the future

(defn loop-beats [time]
  (at (+    0 time) (kick) )
  (at (+  400 time) (hat)  )
  (at (+  800 time) (kick) )
  (at (+ 1200 time) (hat)  )
  (apply-at (+ 1600 time) loop-beats (+ 1600 time) []))

(comment
  (loop-beats (now))

  )

;; rather than thinking in terms of milliseconds, it's useful to think
;; in terms of beats. We can create a metronome to help with this. A
;; metronome counts beats over time. Here's a metronome at 180 beats
;; per minute (bpm):

(defonce metro (metronome 240))

;; we use it as follows:

(metro) ; current beat number
(metro 3) ; timestamp of beat number 3

;; if we rewrite loop-beats using a metronome, it would look like
;; this:

(defn metro-beats [m beat-num]
  (at (m (+ 0 beat-num)) (kick))
  (at (m (+ 1 beat-num)) (hat))
  (at (m (+ 2 beat-num)) (kick))
  (at (m (+ 3 beat-num)) (hat))
  (apply-at (m (+ 4 beat-num)) metro-beats m (+ 4 beat-num) [])
  )

(comment
  (metro-beats metro (metro))

  )

;; because we're using a metronome, we can change the speed:

(comment
  (metro :bpm 180) ;slower
  (metro :bpm 300) ;faster
  )

;; a more complex rhythm

(defn weak-hat []
  (hat 0.3))

;; phat beats
;; i just played around with the tut code a bit more to create a little beat

(defn phat-beats [m beat-num]
  (at (m (+ 0 beat-num)) (kick) (weak-hat))
  (at (m (+ 1 beat-num)) (kick))
  (at (m (+ 2 beat-num)) (kick)       (hat))
  (at (m (+ 3 beat-num)) (kick) (weak-hat))
  (at (m (+ 4 beat-num)) (kick) (weak-hat))
  (at (m (+ 4.5 beat-num)) (kick))
  (at (m (+ 5 beat-num)) (kick))
  (at (m (+ 6 beat-num)) (kick) (hat) )
  (at (m (+ 7 beat-num)) (kick)       (weak-hat) )
  (apply-at (m (+ 8 beat-num)) phat-beats m (+ 8 beat-num) [])
  )

(comment
  (phat-beats metro (metro))


  )

(defn mytb303_2 []
  ;;this is dubious now
  ;;  (tb303 50 :wave 3 :amp 10  :cutoff 18)
  )

(defn mykick []
  (dance-kick 40)
  )
(defn mysnare []
  (snare 100)
  )

(defn myh1 []
  (closed-hat :low 1000 :hi 2000)
  )

(defn myh2 []
  (closed-hat :low 1000 :hi 2000)
  )

;(definst myblip []  (g-verb(blip 100 200)))




(defn myseq [note]
  ;;2 overpad for xtra phat(with echo and chorus)
  (overpad  :note (- note 10) :release 0.4 :dur 1)
  (overpad  :note (- note 10) :release 0.4 :dur 1)
  ;;bliptrack sometimes
  ;;(overpad  :note (+ note 15) :release 0.05 :dur 0.5)
  ;high blip seldom
  ;;(overpad  :note (+ note 32) :release 0.1 :dur 0.5)
  ;(simple-flute )
   ;; (g-verb (blip (mouse-y 24
   ;;         48) (mouse-x 1 100)) 200 8)
  ;;(myblip :note  (- note 00) :release 0.15)
  )


;; ** sam aaron examples from #emacs
;; some snippets which sam aaron on #emacs shared.

(comment
  (demo 60 (g-verb (blip (mouse-y 24
                                  48) (mouse-x 1 100)) 200 8))

  (demo 60 (g-verb (sum (map #(blip (* (midicps (duty:kr % 0 (dseq
                                                              [24 27 31 36 41] INF))) %2) (mul-add:kr (lf-noise1:kr 1/2) 3 4)) [1
                                                                                                                                1/2 1/4] [1 4 8])) 200 8))
  )

;; ** simple beats
;; continues on plhat beats

(defn simple-beats [m beat-num]
  (at (m (+ 0 beat-num))   (mytb303_2) (myh1) (mykick) (myseq 50)
      ;;(dream-inc)
      )
  (at (m (+ 0.5 beat-num)) (myh2) (myseq 55))
  (at (m (+ 1 beat-num))   (myh1) (mysnare)(myseq 50))
  (at (m (+ 1.5 beat-num)) (myh2) (myseq 55))
  (at (m (+ 2 beat-num))   (myh1) (mykick)(myseq 55)(noise-snare))
  (at (m (+ 2.5 beat-num)) (myh2) (myseq 55))
  (at (m (+ 3 beat-num))   (myh1) (mysnare)(myseq 50))
  (at (m (+ 3.5 beat-num)) (myh1) (myseq 55))
  (at (m (+ 4 beat-num))   (myh1) (mykick)(myseq 55))
  (at (m (+ 4.5 beat-num)) (myh2) (myseq 55))
  (at (m (+ 5 beat-num))   (myh1) (mysnare)(myseq 50))
  (at (m (+ 5.5 beat-num)) (myh2) (myseq 55))
  (at (m (+ 6 beat-num))   (myh1) (mykick)(myseq 55)(noise-snare))
  (at (m (+ 6.5 beat-num)) (myh1) (myseq 55) )
  (at (m (+ 7 beat-num))   (myh2) (mysnare)(myseq 5))
  (at (m (+ 7.5 beat-num)) (myh1) (myseq 55)(noise-snare))

  (apply-at (m (+ 8 beat-num)) simple-beats m (+ 8 beat-num) [])
  )





;; * Song: Lexical Libation: In the clone vats
(defn psykick []
  (kick4 40 :decay 2 )
  (kick 50 :decay 2 )
  (dance-kick 40 :decay 0.25 )
  )

(defn psysnare []
  (noise-snare :decay 0.7 )
  )

(defn psyh1 []
  (closed-hat 1 :low 1000 :hi 1500)
  )

(defn rand-int-range [a b]
  (+ a (rand-int (inc (- b a)))))

;; psy beats
;; here i tried to get a psytrance feeling, but it wound up as something else. still trancey though.
(defn psy-beats [m beat-num]
  ;(psybass m beat-num)
  (at (m (+ 0 beat-num))  ( psyh1) (psykick) (psybass 40 :numharm (rand-int-range 10 190)  )
      (psybass2 40 :numharm (rand-int-range 10 19)  )
      )
  (at (m (+ 1 beat-num))  ( psyh1) (myseq 40)(psybass 50 :numharm (rand-int-range 10 190)  )
            (psybass2 40 :numharm (rand-int-range 10 19)  ))
  (at (m (+ 2 beat-num))  ( psyh1) (myseq 40)(psybass 60 :numharm (rand-int-range 10 190) )
            (psybass2 40 :numharm (rand-int-range 10 19)  ))

  (at (m (+ 3 beat-num))  ( psyh1) (psykick) (psysnare) (psybass 40 :numharm (rand-int-range 10 190)  )
            (psybass2 40 :numharm (rand-int-range 10 19)  ))
  (at (m (+ 4 beat-num))  ( psyh1) (myseq 40)(psybass 50 :numharm (rand-int-range 10 190) )
            (psybass2 40 :numharm (rand-int-range 10 19)  ))
  (at (m (+ 5 beat-num))  ( psyh1) (myseq 40)(psybass 60 :numharm (rand-int-range 10 190))
            (psybass2 40 :numharm (rand-int-range 10 19)  ))

  (apply-at (m (+ 6 beat-num)) psy-beats m (+ 6 beat-num) [])
  )

(def hyperrat-sample (load-sample "/home/joakim/roles/Creative/music/hyperrat2/output.wav"))


(def song-events {"init"
                  (fn[]
                    (metro :bpm 480)
                    (psy-beats metro (metro)))
                  "1" (fn[]
                        (def hyperrat-inst (ab-player-sweep :bufnum hyperrat-sample :dur 180))
                        (inst-volume! ab-player-sweep 32)
                        )
                  "2" (fn []
                        (inst-fx! ab-player-sweep fx-echo))
                  "3" (fn[]  (clear-fx ab-player-sweep))
                  "4" (fn []   (apply (choose [(fn [] (inst-fx! psybass fx-echo))
                                              (fn [] (inst-fx! psybass fx-chorus))
                                              (fn [] (inst-fx! psybass fx-reverb))
                                              (fn []
                                                (clear-fx psybass)
                                                )
                                              ])
                                     nil))
                  })


(comment

;;  (psybass metro (metro))

  (inst-fx! overpad fx-echo)
  (inst-fx! overpad fx-chorus )
  (clear-fx overpad)

  (inst-fx! closed-hat fx-echo)
  (inst-fx! closed-hat fx-chorus)
  (clear-fx closed-hat)


  (inst-fx! psybass2 fx-distortion-tubescreamer)
  (clear-fx psybass2)
  (do
    (inst-fx! psybass fx-echo)
    (inst-fx! psybass fx-chorus)
    (inst-fx! psybass fx-reverb))
  (clear-fx psybass)



)


;; * Song: Lexical Libation: Fast and Slow
;; technically a forrest dream variaton
(comment
  ;; [2024-02-03 Sat]
  ;;experimenterar med extremt långsam beat
  ;; på nått sätt råkade technobabble komma in uppitchat vid (dream-inc), med effekter var det kul
  ;; oklart om det går att reproducera
  ;; I will christen this funny little interlude in my space opera:
  ;; spaceships meeting at relative velocities


  (metro :bpm 24)
  (simple-beats metro (metro))

  ;; man kan prova lite olika rates, snabb eller långsam
  (definst slowbabble [](play-buf :rate 0.125  :num-channels 1 :bufnum  (nth  babble-samples 4)))
  (definst fastbabble [](play-buf :rate 2.0  :num-channels 1 :bufnum  (nth  babble-samples 4)))
  (inst-volume! slowbabble 10)
  (slowbabble)
  (babble)
  (fastbabble)
  (inst-fx! slowbabble fx-chorus )
  (inst-fx! slowbabble fx-reverb)

  (inst-fx! fastbabble fx-chorus )
  (simple-beats metro (metro))
  (inst-fx! dance-kick fx-chorus )
  (inst-fx! dance-kick fx-echo)
  (clear-fx dance-kick)

  (inst-fx! overpad fx-echo)
  (inst-fx! overpad fx-chorus )
  (inst-fx! overpad fx-distortion)
  (inst-fx! overpad fx-chorus )
  (inst-fx! overpad fx-reverb)
  (clear-fx overpad)
  )

;; * Song: Forest Dream

;; first i played with an old track called am i alive. this variation didnt turn out as much yet.

;; then i wrote an entirely new song around this beat that turned out rather good, called forest dream!


(def dreamidx (agent 0))



(defn dream-inc [] (send dreamidx inc )(dream @dreamidx))

(comment
  ;; forest dream original
  (metro :bpm 240) ;;? which bpm
  (simple-beats metro (metro))
  ;(inst-fx! overpad fx-compressor)
  ;(inst-fx! overpad fx-sustainer)
  ;(inst-fx! overpad fx-freeverb)
  (def s1 (load-sample   "/home/joakim/roles/am_i_alive_all/am_i_alive/01.wav"))
  (def s2 (load-sample   "/home/joakim/roles/jave/music/am_i_alive_all/am_i_alive/02.wav"))

  ;;(map (fn [x] (load-sample (format   "/home/joakim/roles/jave/music/am_i_alive_all/am_i_alive/%s.wav" x))) '["01" "02"])
  (def dream-samples (for [i (range 1 40)] (load-sample (format   "/home/joakim/roles/jave/music/am_i_alive_all/am_i_alive/%02d.wav" i))))
  ;;(def dream-samples (load-samples    "/home/joakim/roles/jave/music/am_i_alive_all/am_i_alive/*.wav"))

  ;either
  (inst-fx! overpad fx-echo)
  (inst-fx! overpad fx-chorus )
  ;or
  (inst-fx! overpad fx-distortion)
  (inst-fx! overpad fx-chorus )
  (inst-fx! overpad fx-reverb)
  (clear-fx overpad)

  (inst-fx! dream fx-echo)
  (inst-fx! dream fx-chorus)
  (inst-fx! dream fx-reverb)
  (inst-fx! dream fx-compressor)
  ;;(dream (nth dream-samples 3))
  ;;reset, but it doesnt seem right
  (def dreamidx (agent 0))
  (dream-inc)
  (clear-fx dream)

  (choir2s)
  (inst-fx! choir2s fx-feedback-distortion)
  (inst-fx! choir2s fx-chorus )

  (clear-fx choir2s)

  ;;(choir)
  (choir4s)
  (inst-fx! choir4s fx-feedback-distortion)
  ;;you can add a bunch of chorus in serial for a really dreamy effect, but then you need to increase the sample volume
  ;;6 times chorus, 64 times vol, seems okay
  (inst-fx! choir4s fx-chorus )
  (inst-fx! choir4s fx-echo)
  (inst-fx! choir4s fx-reverb)
  (inst-fx! choir4s g-verb)
  (clear-fx choir4s)



  )
;; #+END_SRC

;; define a vector of frequencies from a tune
;; later, we use (cycle notes) to repeat the tune indefinitely

(def notes (vec (map (comp midi->hz note) [:g1 :g2 :d2 :f2 :c2 :c3 :bb1 :bb2
                                           :a1 :a2 :e2 :g2 :d2 :d3 :c2 :c3])))

;; bass is a function which will play the first note in a sequence,
;; then schedule itself to play the rest of the notes on the next beat

(defn mybass [m num notes]
  (at (m num)
      (overpad :note (first notes)))
  (apply-at (m (inc num)) mybass m (inc num) (next notes) []))

;; wobble changes the wobble factor randomly every 4th beat

(defn wobble [m num]
  (at (m num)
      (ctl dubstep :wobble-freq
           (choose [4 6 8 16])))
  (apply-at (m (+ 4 num)) wobble m (+ 4 num) [])
  )

;; put it all together

(comment
  (do
    (metro :bpm 180)
    (dubstep) ;; start the synth, so that bass and wobble can change it
    (mybass metro (metro) (cycle notes))
    (wobble metro (metro))
    )
  )




;; * beats
(def amen-beat
  {
   :C '[   - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - -  - - - - - - - - - - X - - - - -  ]
   :R '[   x - x - x - x - x - x - x - x -  x - x - x - x - x - x - x - x -  x - x - x - x - x - X - x - x -  x - x - x - x - x - - - x - x -  ]
   :S '[   - - - - o - - o - o - - o - - o  - - - - o - - o - o - - o - - o  - - - - o - - o - o - - - - o -  - o - - o - - o - o - - - - o -  ]
   :B '[   o - o - - - - - - - o o - - - -  o - o - - - - - - - o o - - - -  o - o - - - - - - - o - - - - -  - - o o - - - - - - o - - - - -  ]
   }
  )

(def amen2-beat
  {
   :C '[   - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - -  - - - - - - - - - - X - - - - -  ]
   :R '[   x - x - x - x - x - x - x - x -  x - x - x - x - x - x - x - x -  x - x - x - x - x - X - x - x -  x - x - x - x - x - - - x - x -  ]
   :S '[   - - - - o - - o - o - - o - - o  - - - - o - - o - o - - o - - o  - - - - o - - o - o - - - - o -  - o - - o - - o - o - - - - o -  ]
   :B '[   o - o - - - - - - - o o - - - -  o - o - - - - - - - o o - - - -  o - o - - - - - - - o - - - - -  - - o o - - - - - - o - - - - -  ]
   }
  )

(def dnb-beat
  {

   ;;http://www.newgrounds.com/bbs/topic/662530
   :B '[ 0 - - - - - - - - - 0 - - - - - ]
   :S '[ - - - - 0 - - - - - - - 0 - - - ]
   :R '[ 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - ]
   }
  )

(def silent-beat
  {

   ;;http://www.newgrounds.com/bbs/topic/662530
   :B '[ - - - - - - - - - - - - - - - - ]
   :S '[ - - - - - - - - - - - - - - - - ]
   :R '[ 0 - - - 0 - - - 0 - - - 0 - - - ]
   }
  )

(do
  (def psy-beat
    {
     :B2 '[ 0 - - - 0 - - - 0 - - - 0 - - - ]
     :S  '[ - - - - 0 - - - - - - - 0 - - - ]
     :H  '[ c c o - c c o - c c o - c c o - ]
     ;;  :R2 '[ - :c4  :c4 - :c4 :c4 :c4 - :c4 :c4 :c4 - :c4 :c4 :c4 ]
     ;;  :R2 '[ - :c2  :c2 :a2 - :c2 :c2 :a2 - :c2 :c2 :a2 - :c2 :c2 :b2 ]
     :R2 '[ - :c2  :c2 :c2 - :c2 :c2 :c2 - :c2 :c2 :c2 - :c2 :c2 :c2 ]
     :R  '[ - - - - 0 - - - - - - - 0 - - - ]
     }
    )
  (seq/set-beat psy-beat)
  )

(def my-drums
  {:C (fn [x] (hat-demo))
   :R (fn [x] (psybass2 60 :numharm (rand-int-range 10 200))
        (closed-hat) (haziti-clap)
        )

   :R2 (fn [x]
         (psybass3 (note x) )
         (psybass3 (note x) :numharm (rand-int-range 40 600))
         (psybass3 (note x) :numharm (rand-int-range 40 600))
         )
   :S (fn [x] (noise-snare) (noise-snare) (noise-snare :decay 0.4)
                                        ;(dub-kick)
        )
   :B2 (fn [x]
         (dub-kick)
         )
   :B (fn [x] (kick)(tom)(quick-kick))
   :H (fn [x] (if (= 'c x)(closed-hat) (open-hat)))
   }
  )


(comment
  (seq/set-metro :bpm 400)
  (seq/set-drums my-drums)
  (seq/play-metro)
  (seq/set-beat amen-beat)
  (seq/set-beat dnb-beat)
  (seq/set-beat (seq/reverse-beat amen-beat))
  (seq/set-beat dnb-beat)

  ;; psy-beat out of place really
  (seq/set-beat psy-beat)
  (seq/set-beat (seq/reverse-beat psy-beat))

  (inst-fx! psybass2 fx-chorus)
  (inst-fx! psybass2 fx-reverb)
  (inst-fx! psybass2 fx-echo)
  (clear-fx psybass2)

  (inst-fx! psybass3 fx-chorus)
  (inst-fx! psybass3 fx-echo)
  (inst-fx! psybass3 fx-feedback-distortion)
  (inst-fx! psybass3 fx-distortion)
  (inst-fx! psybass3 fx-compressor)

  (clear-fx psybass3)

  (inst-fx!   noise-snare fx-chorus)
  (inst-fx! noise-snare fx-echo)
  (inst-fx!   noise-snare fx-reverb)
  (clear-fx noise-snare)

  (inst-fx!   closed-hat fx-chorus)
  (inst-fx!   open-hat fx-chorus)
  (inst-fx! closed-hat fx-echo)
  (inst-fx!   closed-hat fx-reverb)
  (clear-fx closed-hat)
  (clear-fx open-hat)

  (inst-fx! tom fx-reverb)
  (clear-fx tom )


  (inst-fx! hat-demo fx-echo)

  (dubstep 25)
  (kill dubstep)
  (ctl dubstep :wobble-freqm
       (choose [4 6 8 16]))

  )

(comment
    (seq/set-metro :bpm 250)
    (seq/play-once {
                      ;;:V2 '[ 220 440 880]
                      :B '[x x x x x x x x]
                    :H '[ r]
                    } )
)


;; * Song: Revenue Inspector
;; a totaly different kind of song!
;; it sounds a little bit like a train
;; start the monotrons, occasionally start the theremin, end with some rythms(somewhere else in the code)



(defn song-init-revenueinspector []
  ;;TODO yeah you shuoldnt use def here, just messing around...
  ;; actually i want to store insts in some structure, and also create a couple of them using a loseq sequence,
  ;; so theres some space between them
  (def N1 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0 1))
  (def N2 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0 0))
  (ctl N1 :cutoff 600.0)
  (ctl N2 :cutoff 200.0)
  )


(def song-events {"init" song-init-revenueinspector
                  "1" (fn[])
                  "2" (fn []
                        (def st1 (space-theremin :out-bus 10 :amp 0.8 :cutoff 1000))
                        (space-reverb [:after st1] :in-bus 10))
                  "3" (fn[](kill space-reverb))})


;; this is the adapter that connects with sylt.el
(defn song-event [event]
  (apply (get song-events event ) ()))



(comment
  ;; do a couple of these with different pan
  (def N1 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0 1))
  (def N2 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0 0))
  (ctl N1 :cutoff 600.0)
  (ctl N2 :cutoff 200.0)
  ;; then occasionally these, which you then kill
  (def st1 (space-theremin :out-bus 10 :amp 0.8 :cutoff 1000))
  (space-reverb [:after st1] :in-bus 10)
  (kill space-reverb)

;;then stop

)


;; * Song: Glassheads
;; previously called "escape from synthmen"





;;20240205, smurf.wav is gone? dunno where ~/samples is now
;;(def organ-sample (load-sample "~/samples/organ.wav"))
;;(def organ-sample (load-sample "/home/joakim/smurf.wav"))
;;(def orgloop (sample "/home/joakim/smurf.wav"))
                                        ;(orgloop)
                                        ;
(def dr1 (load-sample   "/home/joakim/roles/Creative/music/am_i_alive_all/am_i_alive/01.wav"))
;(def glasscrash (sample (frp 221528)))
                                        ;(play-buf 1 organ-sample)


;(glasscrashinst)
(comment
  ;;reset
  (do
    (stop)
    (kill grainy2)  (clear-fx grainy2)
    (kill grainy3) (clear-fx grainy3)
    (kill grainy4) (clear-fx grainy4)
    (clear-fx glasscrash)
    )
  ;;setup
  (do
    (glasscrash)
    (seq/set-beat silent-beat)
    ;;(play-drums 100 16)
    (seq/set-metro :bpm 500)
    (seq/set-drums my-drums)
    (seq/play-metro)

    )

  ;;start/reset with mattias grain opnly
  (do
    (inst-fx! glasscrash fx-echo)
    (glasscrash)
    (kill grainy4) (clear-fx grainy4)
    (grainy4 dr1)
    (inst-fx! grainy4 fx-echo)
    (kill grainy2)  (clear-fx grainy2)
    (kill grainy3) (clear-fx grainy3)

    )

  ;; now for some beat
  (do
    (inst-fx! glasscrash fx-echo)
    (glasscrash)
    (seq/set-beat dnb-beat)
    )

  ;; organy grains
  (do
    (inst-fx! glasscrash fx-echo)
                                        ;           (inst-fx!   glasscrash fx-reverb)
    (glasscrash)
                                        ;    (grainy2 organ-sample)
                                        ;(inst-fx!   grainy2 fx-chorus)
                                        ;(inst-fx!   grainy2 fx-reverb)
    (kill grainy4) (clear-fx grainy4)
    (grainy3 organ-sample)
    (ctl grainy3  :vol 1)
    (inst-fx!   grainy3 fx-chorus)
    (inst-fx!   grainy3 fx-reverb)
    )

  (do
    (ctl grainy3  :vol 0.25)
    (grainy4 dr1)
    (seq/set-beat amen-beat)
    )


  (seq/set-beat silent-beat)

  (do
    (kill grainy4)
    (seq/set-beat silent-beat)
    (inst-fx! glasscrash fx-echo)
    (inst-fx!   glasscrash fx-reverb)
    (glasscrash)
    )




  (inst-fx! grainy3 fx-feedback-distortion)
  (clear-fx grainy3)
  (kill grainy2)
  (inst-fx! grainy2 fx-echo)
  (inst-fx! grainy2 fx-feedback-distortion)
  (inst-fx! grainy2 fx-distortion)

  (glasscrash)

  (clear-fx grainy2)
  )

;; * Song: Industrial Wastelands
;;; now "industrial wastelands"

(def wasteland-beat
  {

   ;;http://www.newgrounds.com/bbs/topic/662530
   :B  '[ 0 - - - 0 - - - 0 - - - 0 - - -  0 - - - 0 - - - 0 - - - 0 - - - ]
   :Q  '[ 0 - - - - - - - - - - - 0 - - -  - - - - - - - - - - - - - - - - ]
   :S  '[ - - - - 0 - - - - - - - 0 - - -  - - - - 0 - - - - - - - 0 - - - ]
   :R  '[ 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 -  0 - 0 - 0 - 0 - 0 - 0 - 0 0 0 - ]
   :H  '[ 0 c c - 0 c c - 0 - c c 0 - c -  0 - c c 0 - c - 0 - c - c 0 0 - ]
   :R2 '[ :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2  ]
   :R3 '[ :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :c2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2 :e2  ]

   }
  )

(def hiss2 (sample (frp   66248  )))

(def hiss3 (sample (frp 119741)))
(def door (sample (frp 66817)))
(def steel (sample (frp 172421)))
(def chain (sample (frp 167915)))

(def wasteland-drums
  {:C (fn [x] (hat-demo))
   :R (fn [x]
        ;(psybass2 60 :numharm (rand-int-range 10 200))
        (closed-hat)
        ;(haziti-clap)
        )

   :R2 (fn [x]
;          (jvmonotron (+ 12(note x)) 4 0.001 1 1 0.2 2.5 350.0  (midi->hz (+ 0(note x))) 3.0 0)
         (psybass3 :note (note x) :amp 4 :numharm 10)
;       (psybass3 :note (+ 12(note x)) :amp 8 :numharm 10)
                                        ;   (psybass3 :note (+ 24(note x)) :amp 8 :numharm 10)
        (psybass3 :note (+ 19(note x)) :amp 2 :numharm 10)
     (psybass3 :note (+ 36(note x)) :amp 2 :numharm 10)
  ;   (psybass3 :note (+ 48(note x)) :amp 8 :numharm 10)
         ;; (psybass3 (note x) )         (psybass3 (note x) )
         ;; (psybass3 (note x) )
         ;(psybass3 (note x) )         (psybass3 (note x) )         (psybass3 (note x) )
         ;(psybass3 (note x) :numharm (rand-int-range 40 600))
                                        ;(psybass3 (note x) :numharm (rand-int-range 40 600))
     )
      :R3 (fn [x]

            ;;(jvmonotron (+ 0 (note x)) 4 0.001 1 1 0.2 2.5 350.0  (midi->hz (+ 0(note x))) 3.0 0)
            (if (> 20 (rand-int-range 0 100))(do    (kill simple-flute)
                                                    ;(simple-flute :freq (midi->hz(+ 48 (note x))))
                                                    )
                )
;            (simple-flute :freq (midicps (+ 0 (note x))))
            (simple-flute :freq (midi->hz(+ 36 (note x))))
        )

   :S (fn [x] (noise-snare) (noise-snare) (noise-snare :decay 0.4)
        ;(dub-kick)
        )
   :B2 (fn [x]
        (dub-kick)
        )
   :Q (fn [x]   (apply (choose [ (fn [] (steel))(fn [] (chain)) (fn [] (hiss3)) ]) []))
   :B (fn [x] (kick)(tom)(quick-kick))
   :H (fn [x] (if (= 'c x)(closed-hat) (open-hat)))
   }
  )




(defn song-init-wasteland []
  (seq/set-beat wasteland-beat)
  (seq/set-drums wasteland-drums)
  ;;120 is nice, and 200 is also nice
  (seq/set-metro :bpm 300)
  ;; this doesnt do play-metro, do it separately.
  )

(comment
  (song-init-wasteland)
  (def syntheticmachine(sample (frp 249879))) ;synthetic machine
  ;;(syntheticmachine)
  ;; (def tst (sample (frp 257021))) ;mp3 cant be handled

  ;; for the transformers
  ;; - if the voice is a bare symbol, like :B, then convert to a map 1st
  ;; - in the voice map, overwrite the :transformer setting
  ;;  (dosync (ref-set seq/*beat  ))
  ;;  (assoc {:B []} 1 1)

  (hiss2)
  (steel)

  (door)
  (hiss3)
  (chain)
  (steel)


  (inst-fx! psybass3 fx-reverb)
  (inst-fx! psybass3 fx-echo)
  (inst-fx! psybass3 fx-chorus)
  (clear-fx psybass3 )
  (closed-hat)
  (inst-fx! closed-hat fx-chorus)
  (clear-fx closed-hat )

  (tb303  :note 36 :wave 0 :decay 2 :amp 2 :cutoff 100 :r 0.01)
  (overpad :note 72)
  (ks1 :note 12)
  (whoahaha)
  (bubbles)
  (kill bubbles)

  (simple-flute )
  (note :c2)
  (kill simple-flute)
  (inst-fx! simple-flute fx-echo)
  (inst-fx! simple-flute fx-chorus)


  (jvmonotron (note :e4) 2 0.001 1 1 0.0 2.5 350.0 400.0 3.0 0)
  (inst-fx! jvmonotron fx-reverb)
  (inst-fx! jvmonotron fx-echo)
  (inst-fx! jvmonotron fx-chorus)
  (clear-fx jvmonotron )


  (kill jvmonotron)
  (clear-fx psybass3)
  )



;; * Lockstep
;; a dnb song with a simple melody.



(def lockstep-drums ;lockstep-drums
  {:C (fn [x] (hat-demo))
   :R (fn [x]
        ;(psybass2 60 :numharm (rand-int-range 10 200))
        (closed-hat)
        ;(haziti-clap)
        )

   :R2 (fn [x]
      (psybass3 :note (note x) :amp 20 :numharm 10)
         (tb303  :note (note x) :r 0.5 :wave 1 :release 0.1)
         (tb303  :note (+ 12 (note x)) :r 0.9 :wave 0 :release 0.1)
;        (overpad :note (+ 12 (note x)))
;        (overpad :note (+ 12 (note :c2)))
;         (psybass3)
         )
     :R4 (fn [x]
         (tb303  :amp 2 :note (note x) :r 0.5 :wave 1 :release 0.1)
;         (tb303  :note (+ 12 (note x)) :r 0.9 :wave 0 :release 0.1)
        ;(overpad :note (+ 12 (note x)))
;        (overpad :note (+ 12 (note :c2)))
;         (psybass3)
     )

      :R3 (fn [x]

                                        ;(jvmonotron (+ 0 (note x)) 4 0.001 1 1 0.2 2.5 350.0  (midi->hz (+ 0(note x))) 3.0 0)
            (if (> 20 (rand-int-range 0 100))(do    (kill simple-flute)
                                                    ;(simple-flute :freq (midi->hz(+ 48 (note x))))
                                                    )
                )
;            (simple-flute :freq (midicps (+ 0 (note x))))
            (simple-flute :freq (midi->hz(+ 36 (note x))))
        )

   :S (fn [x] (noise-snare) (noise-snare) (noise-snare :freq 1600 :decay 0.8)
        ;(dub-kick)
        )
   :B2 (fn [x]
        (dub-kick)
        )
   :Q (fn [x]   (apply (choose [ (fn [] (steel))(fn [] (chain)) (fn [] (hiss3)) ]) []))
   :B (fn [x] (kick)(tom)(quick-kick))
   :H (fn [x] (if (= 'c x)(closed-hat) (open-hat)))
   }
  )

(seq/set-beat 'lockstep-beat
  {
   :B  '[ 0 - - - - - - - - - 0 - - - - - ]
   :S  '[ - - - - 0 - - - - - - - 0 - - - ]
   :H  '[ c c c 0 c c c c c c c 0 c 0 c c ]
   {:voice :R22 :seq 2}  '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
   :R2 '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
;      :R3 '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
   }
  )


;; c d# c d# c c c c f d#




(comment
  (seq/set-beat dnb-beat)
  (seq/set-beat wasteland-beat)
  ;;  (seq/play 200 16)
  (seq/set-metro :bpm 200)
  (seq/play-metro)

  (seq/set-beat lockstep-beat)
  (seq/set-drums lockstep-drums)

  (clear-fx noise-snare)
  (kill simple-flute)


  ;;another qy to play with the beat
  (seq/set-beat
   {
    :B  '[ 0 - - - - - - - - - 0 - - - - - ]
    :S  '[ - - - - 0 - - - - - - - 0 - - - ]
    :H  '[ c c c 0 c c c c c c c 0 c 0 c c ]
;;    {:voice :R2 :seq 2 }  '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
    :R2 '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
;;    :R3 '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
    }
   )

  (seq/set-drums my-drums)
  (seq/set-drums lockstep-drums)
  (seq/set-drums wasteland-drums)

  )



(seq/set-beat(def test-beat
  {
   :B  '[ 0 - - - - - - - - - 0 - - - - - ]
   :S  '[ - - - - 0 - - - - - - - 0 - - - ]
   :H  '[ c 0 c c]
   {:H :2}  '[ c 0 c c]
   :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
   {:voice :R2} '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
   }
  ))





;;drum system tests
(comment
  ;;all beats here
  (seq/set-beat silent-beat)
  (seq/set-beat psy-beat)
  (seq/set-beat dnb-beat)
  (seq/set-beat amen-beat)
  (seq/set-beat wasteland-beat)
  (seq/set-beat lockstep-beat)
  (seq/set-beat test-beat)

  ;; you dont need a symbol of course
  (seq/set-beat  {
                   :B  '[ 0 - - - - - - - - - 0 - - - - - ]
                   {:voice :H :seq 2}  '[ 0 0 0 - - - - - - - 0 0 0 - - - ]
                   :S  '[ - - - 0 - - - - - - - 0 - - - ]
                   :H  '[ c 0 c ]
                   :Hsilent  '[ c 0 c  ]
                   :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
                   {:voice :R2 :seq 2}  '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]
                   }
                  )


  (seq/set-beat  [
                   [:B  '[ 0 - - - - - - - - - 0 - - - - - ]]
                   [:B  '[ - 0 - -  - - - - - - - 0 - - - -  ]]
                   [:S  '[ - - - 0 - - - - - - - 0 - - - ]]
                   [:R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]]
                   [{:voice :R2 }  '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]]
                   ]
                  )


  (seq/set-beat  [
                   [:B  '[ 0 - - - - - - - - - 0 - - - - - ]]
                   [:B  '[ - 0 - - - - - - - - - 0 - - - -  ]]
                   [:S  '[ - - - 0 - - - - - - - 0 - - - ]]
                   [:R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]]
                   [{:voice :R2 :seq 3}  '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]]
                   ]
                  )


  ;; the drums
  (seq/set-drums my-drums)
  (seq/set-drums lockstep-drums)
  (seq/set-drums wasteland-drums)



  (seq/set-beat  {
                                     :C  '[ 0 - - - - - - - - - 0 - - - - - ]
                 ;;                    :C  '[ 0  ]
                                        ;                {:voice :H :seq 2}  '[ 0 0 0 - - - - - - - 0 0 0 - - - ]
                                        ;               :S  '[ - - - 0 - - - - - - - 0 - - - ]
                                        ;              :H  '[ c 0 c ]
                   :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
                  { :voice :R2 :seq 2} '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]}

                 )


  (seq/set-drums
   {:C (fn [x] (hat-demo))
    :R2 (fn [x]
          (tb303  :note (+ 12 (note x)) :r 0.9 :wave 0 :release 0.1)
          )

    })
  (inst-fx!  tb303 fx-echo)

  ;;flawed?
  (seq/set-beat  {
                   :H_ '[ c  c c O ]
                   :B  '[ 0 - - - ]
                   :R2 '[ :e4 - - - :c4  - - - :d4 :e4 - - :d4 :c4 - -  ]
                   :R4 '[ :e6  :c6 :d6 :e6  :d6 :c6    ]
                   }
                  )

  (seq/set-drums lockstep-drums)
  (inst-fx!  psybass3 fx-chorus)
  (inst-fx!  psybass3 fx-freeverb)
  (inst-fx!  psybass3 fx-echo)

  (clear-fx psybass3 )

  ;; use metronome strategy
  (seq/set-metro :bpm 250)
  (seq/set-metro :bpm 300)
  (seq/set-metro :bpm 400)
  (seq/set-metro :bpm 4000)
  ;;what does even the bpm mean?
  (seq/play-metro)

;; * Song: Geometric Metaphores
  ;;with cs80 lead thing maybe with a text from geometry lost

  (pan2 [(cs80lead :freq 120 :fatt 0.9)
         (cs80lead :freq 100 :fatt 0.9)]
        [-1 1]
        )

  (pan2 :in (cs80lead :freq 120 :fatt 0.9) :pos -1)
  (def cs801 (cs80lead :freq 120 :fatt 0.9))
  (:synth keys cs801)
  (keys (:synth cs801))(:synth :id :target :position :args :sdef :status :loaded?)
  (pan2 cs801 1)
  (inst-pan! cs801 0)
  (inst-fx!  cs80lead fx-g-verb)
  (inst-fx!  cs80lead fx-chorus)
  (clear-fx cs80lead)



(ctl cs80lead :freq 220 )
(ctl cs80lead :freq [220 110])
  (ctl cs80lead :freq 110)
  (ctl cs80lead :gate 0)

  (ctl cs80lead :fatt 0.1)
  (kill cs80lead)
)

(def note-offset (atom 0))

  (defn song-init-metaphors []
    (seq/set-metro :bpm 250)

    (seq/set-beat  {
                    :C  '[ 0 - - - - - - - - - 0 - - - - - ]
                    {:voice :H :seq 2}  '[ 0 0 0 - - - - - - - 0 0 0 - - - ]
                    :S  '[ - - - 0 - - - - - - - 0 - - - ]
                    :H  '[ c 0 c ]
                    :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 x :c2 x :c2 x ]
                    :B  '[ 0 - - - ]
                    ;;:Q '[ 0 - - - - - - - -]

                    }
                   )
    (seq/set-drums
     {:R2 (fn [x]
            (if (= 'x x)
              (ctl cs80lead :gate 0)
              (do
                (ctl cs80lead :gate 1)
                (ctl cs80lead :freq         (midi->hz (+ @note-offset 0 (note  x)))))))
      :B (fn [x](dub-kick) (kick    :amp-decay 2))
      :B2 (fn [x]  (tb303 :note (note x)) )
      :B3 (fn [x]  (grunge-bass :note (note x)) )
      :Q (fn [x]   (apply (choose [ (fn [] (steel))(fn [] (chain)) (fn [] (hiss3)) ]) []))
      :H (fn [x] (cond
                  (= 'c x)(closed-hat)
                  (= 'r ) (apply (choose [ (fn [] (closed-hat))(fn [] (open-hat)) (fn [] (hat-demo)) ]) [])
                  :else (open-hat)))
      :H2 {:alias :H}
      ;;    :V (fn [x] (grainy5 dr1 (rand-int-range 1 10)  (rand))) ;;some bug here
      :S (fn [x] (snare :freq 440 :decay 20 ))
      :KS (fn [x] (ks1   :note (note x) :decay 0.1 :coef 0.1))
      :C (fn [x] (hat-demo))
      }

     )
    )
  ;;(song-init-metaphors)

(def sw-run-toggle-atom (atom 0))
(defn sw-run-toggle []
  (swap! sw-run-toggle-atom #(if (= 0 %) 1 0))
  )
;;(sw-run-toggle)

;; how its supposed to work:
;; numpad
;; . starts playing
;; 0 initializes
;; 1 starts the lead synth, you can start several, there should be some chorus on them
;; 2 starts the voice sample
;; 3 pauses voice, it echoes out cleverly
;; 7 lead octave down
;; 8 lead octave up
;; 9 lead octave 0





(def song-events {"init" song-init-metaphors
                  "1" (fn[]
                        ;; for more phat, start several cs80lead, with slight delay between
                        (cs80lead :freq 120 :fatt 0.9)

                        )
                  "2" (fn []
                        (reset! curoffset 0)
                        (def metaphor-sample (load-samples  "/home/joakim/roles/Creative/music/overtone-sylt/minisylt2024/geometafors9_2/out.wav"))
                        (def sw-metaphor (ab-player-sweep :dur 400 :bufnum (nth metaphor-sample 0)))
                        (clear-fx  ab-player-sweep)
                        (def myecho (inst-fx!  ab-player-sweep fx-echo2))
                        (ctl myecho :echo-level 0)
                        )
                  "3" (fn[]
                        (ctl sw-metaphor :run (sw-run-toggle))
                        (ctl myecho :echo-level (if (= 0 @sw-run-toggle-atom) 1.0 0.0))

                        )
                  "7" (fn[] (swap! note-offset #(- % 12 )))
                  "8" (fn[](swap! note-offset #(+ 12 %)))
                  "9" (fn[](reset! note-offset 0))
                  })



(comment
  (inst-volume! myecho 0)
  (def myecho (inst-fx!  ab-player-sweep fx-echo2))
  (ctl myecho :echo-level 1)

  (inst? sw-metaphor)

  (clear-fx  ab-player-sweep)


  (ctl sw-metaphor :run 0)
  (ctl a :run 1)
  (ctl a :t-trig 0)
  (ctl a :t-trig 1)


  (inst-volume! cs80lead 1)
  (inst-volume! ab-player-sweep 3)

  (inst-fx!  cs80lead fx-chorus)
  (clear-fx  cs80lead )

  (inst-fx!  cs80lead fx-chorus)
  (inst-fx!  cs80lead fx-freeverb)
  (inst-fx!  cs80lead fx-echo)
  (inst-fx!  tb303 fx-echo)
  (inst-fx!  grainy5 fx-echo)
  (inst-fx!  tb303 fx-freeverb)
  (clear-fx  tb303 )
  (clear-fx cs80lead )

  (inst-fx! grainy5 fx-echo)
  (inst-fx! grainy5 fx-echo)
  (clear-fx grainy5 )

  (grainy5 dr1 4 0.6)
  (kill grainy5)
  )
;; * other stuff follows...
  (seq/set-beat  {
                   :H  '[ c r - -   c - c c   - c - -  c - - -]
                                 :H2  '[ r r r - - - - - - - r r r - -  ]
                   ;;            :H2  '[ r  ]
                   ;;               :S  '[ - - - 0 - - - - - - - 0 - - - ]
                   ;;              :H  '[ c 0 c ]
                   :R2 '[ :g#2  :g#2 :g#2 :a#2 :a#2 :g#2 x]
                   :B  '[ 0 - - - ]
                   :B2  '[ :c2 - - - :e2 - - -]
                   ;;              :Q '[ 0 - - - - - - - -]
                   ;;:V '[ 0 - - -]
                   ;;              :V '[ 0 ]
                   :S '[0 - - - - - - -]

                   }
                  )

  (seq/set-beat  {
                                        ;:H  (il 0 '[c])
                   :KS (seq/il 1 (shift (into [] (map #(note %) '(:c3 :e3 :g3  :c3))) '[ 0 1 2 3  ] -12 ))
                                        ;                 :KS (il 0 '[:c3 :e3 :g3  :c3])
                   :B  (seq/il 1 '[O - -  -] )
                   :S  (seq/il 1 '[0 - - - - - - -])
                   :B2 (seq/il 1 '[- :c2 :c2 :c2])
                   :B3 (seq/il 1 '[- :c3 :c3 :c3 ])
                   }
                  )

(do
  (clear-fx grunge-bass)
  (clear-fx ks1 )
  (clear-fx closed-hat )
  )

  ;; experiment
  (metro :bpm 800)


;; * Song: City of lost geometry
  ;;this or the next one could be stockholm geometry lost
  ;; KS with a lot of reverb and echo is needed

;; this kit is mostly copy paste but i want some special things, and not be confused with overload


  (def city-samples-ps (load-samples  "/home/joakim/roles/Creative/music/overtone-sylt/minisylt2024/city_9/*ps.wav"))
  (def city-samples-ps-long (load-samples  "/home/joakim/roles/Creative/music/overtone-sylt/minisylt2024/city_9/*ps_long.wav"))


  (definst city-ps-long []
    (play-buf :rate 0.9  :num-channels 1 :bufnum (nth city-samples-ps-long 0)))
  (inst-fx! city-ps-long fx-echo)
  (city-ps-long)

  (definst city-speech [bufnum 0]
    (play-buf :rate 0.5  :num-channels 1 :bufnum bufnum))


  (defn city-speech-n [n]
    (city-speech (nth city-samples-ps n)))
  (city-speech-n (rand-int-range 0 6))

  (defn toggle-vol [myinstr]
    (inst-volume! myinstr (if (= 1 (deref (:volume myinstr))) 0 1 ))
    )

  (def song-events
    {"init"



                    (fn []

                    ;; start out at 0, the toggle them to bring them in
                    (inst-volume! ks1 0)
                    (inst-volume! dub-kick 0)
                    (inst-volume! grunge-bass 0)
                    (inst-volume! tb303 0)
                    (inst-volume! city-speech 0)



                      (seq/set-drums
                       {
                        :B (fn [x](dub-kick) (kick    :amp-decay 2))
                        :B2 (fn [x]  (tb303 :note (note x)) )
                        :B3 (fn [x]  (grunge-bass :note (note x)) )
                        :Q (fn [x]     (city-speech-n (rand-int-range 0 6))
                             (inst-pan! city-speech (* 2 (- (rand) 0.5)))
                             )
                        :S (fn [x] (snare :freq 440 :decay 20 ))
                        :KS (fn [x] (ks1   :note (note x) :decay 0.1 :coef 0.1))
                        }

                       )
                      ;; i think i settled on this one now
                      (seq/set-beat  {
                                      ;;:H '[c x c c]
                                        ;               :H '[c ]
                                      :B '[0 - - -]
                                      :S '[0 - - - - - - -]
                                      :B2 '[- :c2 :c2 :c2]
                                      :B3 '[- :c3 :c3 :c3 ]
                                      ;;:V '[- :c3 :c3 :c3 ]

                                        ;              :V '[ - 0 0 0 ]
                                      :KS (seq/il 1 (shift (into [] (map #(note %) '(:c3 :e3 :g3  :c3))) '[ 0 1 2 3  ] -12 ))
                                      :Q  (seq/il 1 '[O - -  - -] )
                                      }
                                     )

                      )
                  "1" (fn[]
                          (toggle-vol ks1)
                        )
                    "2" (fn []
                          (toggle-vol dub-kick )
                        )
                    "3" (fn[]
                          (toggle-vol grunge-bass )
                        )
                    "4" (fn[]
                          (toggle-vol tb303)
                          )
                    "5" (fn[]
                          (toggle-vol city-speech)
                          )

                  "6" (fn[]   (city-ps-long))
                  "9" (fn[])
                  })


  ;; adding the fx chains in a sweep doesnt seem reliable, but it seems to work if i do it step by step
  (clear-fx grunge-bass)
  (clear-fx ks1 )
  (clear-fx dub-kick )
  (clear-fx tb303 )
  (clear-fx city-speech )
  (clear-fx snare )

  (inst-fx! grunge-bass fx-g-verb)
  (inst-fx! ks1 fx-echo)
  (inst-fx! ks1 fx-echo)
  (inst-fx! ks1 fx-freeverb)
  (inst-fx! ks1 fx-freeverb)
  (inst-fx! ks1 fx-freeverb)

  (inst-fx! dub-kick fx-echo)

  (inst-fx! grunge-bass fx-chorus)
  (inst-fx! ks1 fx-echo)
  (inst-fx! ks1 fx-chorus)
  (inst-fx! closed-hat fx-chorus)
  (inst-fx! ks1 fx-g-verb)
  (inst-fx! ks1 fx-freeverb)
  (inst-fx! snare fx-echo)
  (inst-fx! dub-kick fx-echo)
  (inst-fx! grunge-bass fx-freeverb)


(inst-fx! grunge-bass fx-chorus)
(inst-fx! grunge-bass fx-echo)
(inst-fx! ks1 fx-echo)

(inst-fx! closed-hat fx-chorus)
(inst-fx! ks1 fx-freeverb)
(inst-fx! ks1 fx-chorus)
(inst-fx! ks1 fx-g-verb)
(inst-fx! grunge-bass fx-freeverb)
(clear-fx grunge-bass)
(daf-bass :freq (note 36))
(kill daf-bass)

  (kill simple-flute)

  ;;some more features id like:
  ;; mute a voice
  ;; somehow allow more patterns for a voice(perhaps with syms like :B:2
  (overtone.helpers.string/split-on-char (name :b:2) ":")


;; * other stuff

(seq/set-drums {
                 :H (fn [x] (if (= 'c x)(electro-hat) (open-hat)))
                 :B (fn [x] (electro-kick))
                :C (fn [x] (electro-clap))
                :V (fn [x] (vocali2 x 70 0.5))
                 })

(seq/set-beat 'vocali-beat
 {
  :H '[c c c -  c - c -  c - c -  c - c ]
  :B '[c - - -  - - - -  c - - -  - - - -]
  :C '[- - - -  - - - -  - - - -  c - - -]
  :V '[:a :i :e - :E - :OE - :y -  :u - :o - :a -]
  })

(seq/set-beat 'vocali-beat
 {
  :H '[c c c -  c - c -  c - c -  c - c -]
  :B '[c - - c  - - c -  c - - c  - c - -]
  :C '[- - c -  - - - c  - - - -  c - - -]
  :V '[:a :i :e - :E - :OE - :y -  :u - :o - :a -]
  })


(comment

  (seq/set-metro :bpm 600)

  (inst-fx! vocali fx-chorus)
  (inst-fx! electro-hat fx-chorus)
  (inst-fx! vocali fx-echo)

  )

;;debug leaky inst
;; hat fail after a while, kick as well, clap as well!
;; tstbass fails, does anything not fail=
;; (defn loop-beats [time]
;;   (at (+    0 time)  (tstbass) )
;;   (apply-at (+ 50 time) loop-beats (+ 50 time) []))

(seq/set-drums {
                 :V (fn [x] (vocali2 x 70 0.5))
                 :B (fn [x] (electro-kick))
                 :H (fn [x] (electro-hat))
                 })
(seq/set-beat  {
                 :V '[:a :i :o :O :E :e :OE :y :u :oe ]
                 :B '[x - ]
                 :H '[x]

                })
(comment
 (seq/play-metro)
 (seq/set-metro :bpm 200)
 (inst-fx! vocali fx-echo )
 (inst-fx! vocali fx-chorus )

;; chorus + echo on the vocali, sounds interesting
 )

(comment
  (loop-beats (now))
  (demo  (formlet (impulse:ar 50, 0.0) (mouse-x:kr 300 3000) 0.01 (mouse-x:kr 0.1 1.0)))

  )




(defn mystr2 [f]
  (mystr f)
  (mystr (* 2 f))
  (mystr (* 4 f) )
  (mystr (* 8 f)))

(defn mystr3 [f]
  (mystr f)
  (mystr (* 2 f))
  (mystr (* 4 f) )
  (mystr (* 8 f)))
(mystr3 50)
(mystr2 50)
(comment
  (inst-fx! mystr fx-chorus)
  (inst-fx! mystr fx-echo)
(mystr 50)

  (mystr2 50)
  (mystr2 150)
  (mystr2 100)
  (inst-volume! mystr 0.1)
  )

;;(demo (+ (* 0.1 (saw:ar 880))(rlpf (pulse:ar 440 (lin-lin (sin-osc:kr 6) -1 1 0.5 0.55  )) 1500 1 )))

(seq/set-drums {
                 :V (fn [x] (tsts (/  x 1)))
                 :B (fn [x] (electro-kick) (dance-kick))
                                        ;:H (fn [x] (electro-hat))

                 :H (fn [x] (cond
                             (= 'c x)(electro-hat)
                             (= 'r x) (apply (choose [ (fn [] (closed-hat))(fn [] (open-hat)) (fn [] (hat-demo)) ]) [])
                             :else (open-hat)))
                 :V2 (fn [x] (bass x))
                 :S (fn [x] (electro-clap))
                 :m (fn [x y] (println x y) )
                 })
(seq/set-beat  {
                 :V '[110 220 440 ]
                 :B '[x - ]
                :H '[c c r]
                :V2 '[110 110 110 55 -  110 55 -]
                 :S '[- - - x]
                 :m '[[a 1] [b 2] [c 3]]
                 })
;;some echo and chorus on tsts and your good!
(comment
  (seq/play-metro)
  (seq/set-metro :bpm 250)
  (inst-fx! tsts fx-echo)
  (inst-fx! tsts fx-chorus)
  (clear-fx tsts)
  (cs80lead :freq 220 :fatt 0.9)
  (ctl cs80lead :freq 220 )
  (ctl cs80lead :freq 440 )
  (ctl cs80lead :freq 110)
    (ctl cs80lead :freq 55)
  (inst-fx! cs80lead fx-chorus)
  (kill cs80lead)
  (seq/play-once {
                    :V2 '[ 220 440 880]
                    :H '[c c r]
                    } )

  )



(seq/set-drums {
                 :B (fn [x] (electro-kick) (dance-kick)
                      )
                 :H (fn [x] (cond
                             (= 'c x)(electro-hat)
                             (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                             (= 'r x) (apply (choose [ (fn [] (closed-hat))(fn [] (open-hat)) (fn [] (hat-demo)) ]) [])
                             :else (open-hat)))
                 :V2 (fn [x] (buzz x :dur 20))
                 :V3 (fn [x] (vocali2 x 53 2 )  (vocali2 x 55 2 ))
                 :S (fn [x] (electro-clap))
                 })
(seq/set-beat  {
                 :B '[x - ]
                 :H '[c o]
;                 :V3 '[:i  :o :a  :e ]
                 :V2 '[40 40]
                 })
(comment
  ;;song title "o batteri" jacob named it!
  (seq/play-metro)
  (seq/set-metro :bpm 250)
  (kill buzz)
  (inst-fx! buzz fx-chorus)
  (inst-fx! vocali fx-echo)
  (clear-fx vocali )

  )

;;here i want to do some drum sequencing using my new feature of sub sequences
;;atm they wind up in unstrument :F
(seq/set-drums {
                   :B (fn [x]
                        (cond
                          (= 'c x)(electro-kick)
                          (= 'o x) (dance-kick)
                          :else (do (electro-kick) (dance-kick)))
                        )

                 ;; :B (fn [x] (electro-kick) (dance-kick)
                 ;;      )
                 :H (fn [x] (cond
                             (= 'c x)(electro-hat)
                             (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                             :else (open-hat)))
                 :BL (fn [x] (tsts (* 1 (midi->hz(note x))) ))
                 :C (fn [x] (electro-clap))
                 :F (fn [x] (seq/play-once (choose [{:H '[c c c o]}
                                                     {:H '[c - c -]}
                                                     {:H '[c c c c]}
                                                     ])))
                 })

(seq/set-beat  {
                 :B '[x - ]
                 :F '[x - - -]
                 ;;:H  '[c o c o]
                 ;:BL '[:c2 :d2 :c2 :e#2]
                 :BL '[:g2 :g2 :d2 :g2   :a2 :g2 :d2 :b2  :a#2 :a#2 :a#2 :a#2  :a#2 :a#2 :a#2  :a#2]
                     ;;bbxb nbxm jjjj jjj.
                 {:voice :B  :id 3} '[o - ]
                 {:voice :B :il 5 :id 0} '[c o ]
                 {:voice :B :il 3 :id 1} '[c o ]

                 })
(comment
  (seq/play-metro)
  (seq/set-metro :bpm 200)

  (inst-fx! tsts fx-distortion2)
  (inst-fx! tsts fx-distortion-tubescreamer)
  (inst-fx! tsts fx-chorus)
  (inst-fx! tsts fx-echo)

  (clear-fx tsts)
)
(comment
;;   now id like to improve the interleave function of the sequencer. the original idea was very simple,
;;   just use the il macro to splice in the desired num nops in the sequence. then you had to multiply the metro by hand to sompensate.

;;   now i want instead:
;;   (seq/set-metro :bpm 200 il 3)
;;   should mean real metro should be (1+3) *200=800
;;   then each seq should have 3 nops intereleaved automatically if nothing else is said.
;;   but a seq can use whatever interleave it wants, if said explicitly.

 (seq/set-metro :bpm 200 :il 3)

  (seq/set-drums {
                   :B (fn [x]
                        (cond
                          (= 'c x)(electro-kick)
                          (= 'o x) (dance-kick)
                          :else (do (electro-kick) (dance-kick)))
                        )
                   :H (fn [x] (cond
                               (= 'c x)(electro-hat)
                               (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                               :else (open-hat)))
                   :C (fn [x] (clap))
                   :E (fn [x]  (cond (= 'e x)  (inst-fx! clap fx-echo)
                                   (= 's x) (clear-fx clap)))
                   })

  ;;turned out nice, even though its a test track
  (seq/set-beat  {
                   :B '[x - ]
                   {:voice :B  :id 3} '[o - ]
                   :H  '[c o ]
;                 {:voice :H :il 0 :id 0}'[c c ]
                   ;;atm you comment out tracks to mute them
                   ;;atm also you need :id 0, so the keys are unique
                   {:voice :B :il 5 :id 0} '[c o ]
                   {:voice :B :il 3 :id 1} '[c o ]
                   :C '[- - - x]
                   {:voice :E :il 15 } '[e - -  - - s  ]
                   }
                  )

  ;;because doseq is used in drum-fn, this wont work. which is sad
    (seq/set-beat  [
                     :B '[x - ]
                   ]
                  )
    (seq/set-metro :bpm 200 :il 3)
  (seq/play-metro)
  (inst-fx! clap fx-echo)
  (clear-fx clap)
  )

;;now i want to play a bit with chords
;; i have no idea what im doing at all

  (definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     ;;(sin-osc freq)
     (sin-osc freq)
     vol))

  (defn saw2 [music-note]
    (saw-wave (midi->hz (note music-note))))

(declare sf) ;; its an instrument im going to define later.
  (seq/set-drums {
                   :B (fn [x]
                        (cond
                          (= 'c x)(electro-kick)
                          (= 'o x) (dance-kick)
                          (= 'd x) (dub-kick)
                          :else (do (electro-kick) (dance-kick)))
                        )
                   :H (fn [x] (cond
                               (= 'c x)(electro-hat)
                               (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                               :else (open-hat)))
                   :C (fn [x] (clap))
                   :E (fn [x] (cond (= 'e x)  (inst-fx! clap fx-echo)
                                               (= 's x) (clear-fx clap)))
                  :CH (fn [a b] (seq/play-chord (chord-degree a b :phrygian) sampled-piano ;; was saw2
                                               ))
                  :piano (fn [x] (sampled-piano x))
                   :BL (fn [x] (tsts (midi->hz(note x))))
                  :R (fn [x]     (risset :freq 100 :amp 0.9) (risset :freq 101 :amp 0.9))
                  :RO (fn [f a]     (risset :freq f :amp a) (risset :freq (+ f 1) :amp a))
                   :F (fn [x]     (ctl sf :freq (midi->hz(note x))))
                  :F2      (fn [x]  (seq/play-once (seq/mk-arpeggio-pattern :F  '(0 2 1) (chord-degree x :c2 :ionian) 0  ))    )
                  :F3      (fn [x y]  (seq/play-once (seq/mk-arpeggio-pattern :piano  (choose-n 2 '(0 2 1 3)) (chord-degree x :c2 :zhi) 0  ))    )
                  ;;:V (fn [x] (pack-speech-n (swap! pack-next #(if (= (dec (count pack-samples)) %) 0 (inc %)))))
                  })

;; start bisect
;(choose-n 3 [1 2 3] )
(seq/play-chord   (chord-degree :iii :d4 :ionian) saw2)
  ;;turned out nice, even though its a test track
  (seq/set-beat  {
                   :B '[x - ]
                   {:voice :B  :id 3} '[o - ]
                   {:voice :H :il 3 }  '[c o ]
;                  {:voice :H :il 0 :id 0}'[c c ]
                   ;;atm you comment out tracks to mute them
                   ;;atm also you need :id 0, so the keys are unique
                   {:voice :B :il 5 :id 0} '[c o ]
                   {:voice :B :il 3 :id 1} '[c o ]
;                   :C '[- - - x]
                  {:voice :E :il 15 } '[e - -  - - s  ]
                  ;; here it would be nice with a feature like  {:voice :CH :args [:phrygian :d4] } '[[:iii] - - - [:ii] - -  [:i] - - - [:iv] - - ]
                  ;; the call args would be expanded to [:phrygian :d4 :iii] for instance (exact order to be determined)
                   :CH '[[:iii :d4] - - - [:ii :d4] - -  [:i :d4] - - - [:iv :d4] - - ]
                   }
                 )

;; missunderstood blues
  (seq/set-beat  {
                  :B '[x  - ]
                  {:voice :B  :id 3} '[o - ]
                  {:voice :H :il 3 }  '[c o ]
                  {:voice :H :il 0 :id 0}'[c c ]
                   ;;atm you comment out tracks to mute them
                   ;;atm also you need :id 0, so the keys are unique
                  {:voice :B :il 5 :id 0} '[c o ]
                  {:voice :B :il 3 :id 1} '[c o ]
                  :C '[- - - x]
                  {:voice :E :il 15 } '[e - -  - - s  ]
                  ;; here it would be nice with a feature like  {:voice :CH :args [:phrygian :d4] } '[[:iii] - - - [:ii] - -  [:i] - - - [:iv] - - ]
                  ;; the call args would be expanded to [:phrygian :d4 :iii] for instance (exact order to be determined)
                  ;;{:voice :CH :il 3} '[[:i :e4] [:i :e4][:i :e4][:i :e4] [:iv :e4][:iv :e4] [:i :e4][:i :e4] [:v :e4]  [:v :e4] [:i :e4] [:i :e4]]
                  {:voice :CH :il 3} '[[:i :e4] - - - [:iv :e4] - - - [:v :e4]  -  [:i :e4] - ]                  
                  {:voice :F3 :il 3} '[[:i :e4] [:i :e4][:i :e4][:i :e4] [:iv :e4][:iv :e4] [:i :e4][:i :e4] [:v :e4]  [:v :e4] [:i :e4] [:i :e4]]
                  }
                 )

;;(seq/play-once (seq/mk-arpeggio-pattern :piano  '(0 2 1) (chord-degree :i :c2 :ionian) 0  ))

(comment

  (seq/set-metro :bpm 200 :il 3)
    (seq/set-metro :bpm 200 )
  (seq/set-metro :bpm 200 :il 30)
  (seq/play-metro)

)


;; *  song "pack" (you will go on a journey)
;; see lyrics
(declare sf) ;; its an instrument im going to define later.
;; this is kinda duplicated from above, but i like the song self contained, i will strip whats not used


;; i want to bind a singleton instr instance to a voice somehow
;; if theres no current inst, make it, otherwise return the previous one
;; this also get rids of the ugly defs i used in the song init code
(def *voice-insts (atom {}))
;; the call gets to be a little convoluted calling convention, probably the inst setup should be separate


;; {:instance inst :init fn}
(defn get-voice-inst [voice]
  (let [voice-instance (:instance (get @*voice-insts voice))
        voice-init (:init (get @*voice-insts voice))]
    (if (node-live? voice-instance)
      voice-instance
      (reset! *voice-insts (assoc-in @*voice-insts [voice :instance] (apply voice-init nil))))))

(defn set-voice-inst [voice voice-init]
  (reset! *voice-insts (assoc-in @*voice-insts [voice :init] voice-init)))


;;(set-voice-inst :F (fn[] (noise-flute :freq (midi->hz(note :c4)))))
;;(get-voice-inst :F )
;;@*voice-insts
;;(def *voice-insts (atom {}))
;;(seq/play-metro)
;;(sylt.loseq/clear-mute)

(comment
  (free-all-loaded-samples)
  ;; if samples change on disk clear out cache before reloading
)
(def pack-samples (load-samples  "/home/joakim/roles/Creative/music/overtone-sylt/minisylt2024/pack_3/*.wav"))

  (definst pack-speech [bufnum 0]
    (play-buf :rate 0.5  :num-channels 1 :bufnum bufnum))


  (defn pack-speech-n [n]
    (pack-speech (nth pack-samples n)))
(count pack-samples)

;;(nth pack-samples 3)
(def pack-next (atom 0))
(pack-speech-n (swap! pack-next #(if (= (dec (count pack-samples)) %) 0 (inc %))))

(def song-events {"init" (fn[]
                           (seq/set-drums {
                                           :B (fn [x]
                                                (cond
                                                  (= 'c x)(electro-kick)
                                                  (= 'o x) (dance-kick)
                                                  (= 'd x) (dub-kick)
                                                  :else (do (electro-kick) (dance-kick)))
                                                )
                                           :H (fn [x] (cond
                                                       (= 'c x)(electro-hat)
                                                       (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                                                       :else (open-hat)))
                                           :C (fn [x] (clap))
                                           :E (fn [x] (cond (= 'e x)  (inst-fx! clap fx-echo)
                                                           (= 's x) (clear-fx clap)))
                                           :CH (fn [a b] (seq/play-chord (chord-degree a b :ionian) saw2))
                                           :BL (fn [x] (tsts (midi->hz(note x))))
                                           :R (fn [x]     (risset :freq 100 :amp 0.9) (risset :freq 101 :amp 0.9))
                                           :RO (fn [f a]     (risset :freq f :amp a) (risset :freq (+ f 1) :amp a))
                                           :RO1 (fn [f]     (risset :freq (midi->hz(note f)) :amp 0.2) (risset :freq (+ (midi->hz(note f)) 1) :amp 0.2))
                                           :F (fn [x]     (ctl (get-voice-inst :F  ) :freq (midi->hz(note x))))
                                           :F2      (fn [x]  (seq/play-once (seq/mk-arpeggio-pattern :F  '(0 2 1) (chord-degree x :c2 :ionian) 0  ))    )
                                           :V (fn [x] (pack-speech-n (swap! pack-next #(if (= (dec (count pack-samples)) %) 0 (inc %)))))
                                           })

                           (seq/set-beat  {
                                           :B '[d - ]  ;; base drum
                                           :V '[x - - - - - - - - - - - - - - -] ;; voice samples
                                           :R  '[x - - - - - - - ] ;; risset
                                           ;;                :C  '[x - - - - - - - ]
                                           ;;here i intended a simulated echo, but it is hard getting the same timing as the fx-echo
                                           ;;fx-echo manages 6 rissets in the same time as 4 bdrums
                                           {:voice :RO :il 5} '[[100 1] [100 0.8]  [100 0.4] [100 0.3] [100 0.2] [100 0.1] - - ]

                                           ;; a pseudo echo attempt risset
                                           {:voice :RO :il 10}  (into (loop [x 7 a 1 r []] (if (<  x 1 ) r (recur (- x 1) (* a 0.8) (conj r [100  a] ))  )  ) [ '-])

                                           ;;{:voice :H :il 1} '[- c c - c c o -]
                                           ;;{:voice :H :il 1} '[ c c o c o c o - c c - - - - - - ]
                                           ;; noise flute
                                           {:voice :F :il 1} '[ :c#4 :c#4  :c4 :c#4 :c4 :c#4 :c4 :c1 :c#4 :c#4 :c1 - - - - - ];;TODO c1 should be a stopnote, x maybe?
                                           ;; high pitched flute
                                           {:voice :RO1 :il 1} '[ :c#8 :c#8  :c8 :c#8 :c8 :c#8 :c8 :c1 :c#8 :c#8 :c1 - - - - - ];;TODO c1 should be a stopnote, x maybe?
                                           ;;{:voice :F :il 0} '[:c2]
                                           ;;:F2 '[:i -  :ii - - :iv ]
                                           }
                                          )

                           (seq/set-metro :bpm 200 )
                           (reset! pack-next 0)
                           ;;(sylt.loseq/clear-mute)
                           )
                  ;;"0"                                              ;; 0 init, should be safe
                  
                  "1"  (fn []   )                                     ;; 1 base drum
                  "2" (fn [] (reset! pack-next -1)  )                 ;; 2 voice
                  "3" (fn[])                                          ;; 3 risset
                  "4" (fn[])                                          ;; 4 risset 
                  "5"  (fn [] )                                       ;; 5 risset
                  "6"  (fn [](kill  (get-voice-inst :F  )) )          ;; 6 noise flute
                  "7"  (fn [] )                                       ;; 7 high flute
                  "8"  (fn [](simple-flute :freq (midi->hz(note :c2)))) ;; 8 bass flute
                  "9" (fn[])
                  })

;;end bisect



(comment
  (inst-fx! pack-speech fx-chorus)
  (def myverb (inst-fx! pack-speech fx-freeverb))
  (inst-volume! pack-speech 1)
  (clear-fx pack-speech)
  
;  (seq/set-metro :bpm 200 :il 6)
    (seq/set-metro :bpm 200 )
 ;   (seq/set-metro :bpm 200 :il 3)
    (inst-fx! open-hat fx-echo)
    (inst-fx! open-hat fx-chorus)
    (inst-fx! electro-hat fx-chorus)
    (risset :freq 100 :amp 0.9)
    (inst-fx! risset fx-echo)
    (inst-fx! risset fx-chorus)
    (clear-fx risset)
    (seq/play-once {{:voice :RO :il 5}  (into (loop [x 6 a 1 r []] (if (<  x 1 ) r (recur (- x 1) (* a 0.6) (conj r [100  a] ))  )  ) [ '-])
                    })
    (seq/play-once (seq/mk-arpeggio-pattern :F  '(1  3 2 1 3) (chord-degree :iii :c#3 :ionian) 0  ))
    (seq/play-once (seq/mk-arpeggio-pattern :F  '(1  3 2 1 3) [:c#4 :c4 :c1 :c#4] 0  ))
    (def sf (noise-flute :freq 100))
    (def sf (simple-flute :freq 100))
    (simple-flute :fr-eq 100)
    (simple-flute :freq (midi->hz(note :c2)))
    (kill sf)
    (kill simple-flute)
    (def sf (noise-flute :freq 20))
    (def sf (noise-flute :freq 1))
    (clear-fx noise-flute)
    (inst-fx! noise-flute fx-chorus)
    (inst-fx! noise-flute fx-echo)
    (inst-volume! noise-flute 1)
    (ctl sf :freq 2000)
    (ctl sf :rq 0.9)
    (ctl sf :rq 0.01)
    (kill sf)
    (kill noise-flute)

    (inst-fx! simple-flute fx-chorus)
    ;;(inst-fx! simple-flute fx-echo)
    (inst-fx! simple-flute fx-distortion-tubescreamer)
    (clear-fx noise-flute)

    (kill simple-flute)

)

;; * end pack

;; glissando http://chatley.com/posts/10-28-2011/overtone/
(comment

  (definst square-wave [freq 440] (square freq))

(def times (take 220 (iterate #(+ 30 %) 0)))

(defn change-pitch [t f inst] (at (+ t (now)) (ctl inst :freq f)))

(defn falling-pitches [start] (take (/ start 2) (iterate dec start)))
(defn rising-pitches [start] (take start (iterate inc start)))

(defn slide [pitches inst] (map (fn [x y] (change-pitch x y inst)) times pitches))

(square-wave)

(slide (falling-pitches 440) square-wave)
(slide (falling-pitches 440) noise-flute)
(slide (rising-pitches 220) noise-flute)
(slide (rising-pitches 220) square-wave)

)

(seq/set-beat  {
                :B  '[d - - ]
;               :BL '[- [:c2 100] [:c2 100] [:c2 200] - [:c2 100] [:c2 100] [:c2 1000] - [:c2 100] [:c2 1000] [:c2 100]]
                { :voice :BL :seq 2} '[- [:c2 100] [:c2 100] [:c2 130] - [:c2 100] [:c2 100] [:c2 120] - [:c2 100] [:c2 110] [:c2 100]]
;               { :voice :BL :seq 3} '[- [:c3 110] [:c3 110] [:c3 140] - [:c3 110] [:c3 110] [:c3 130] - [:c3 110] [:c3 120] [:c3 110]]
                ;;                :BT '[- :c3 - - - - -  ]
;;                :BT '[- - - - - - - :c3 - - :c3 -]
                ;;         { :voice :H :il 1} '[e c c e c e]
                })

(seq/set-drums {
                :B (fn [x]
                     (cond
                       (= 'c x)(electro-kick)
                       (= 'o x) (dance-kick)
                       (= 'd x) (dub-kick)
                       :else (do (electro-kick) (dance-kick)))
                     )
                :H (fn [x] (cond
                            (= 'e x)(electro-hat)
                            (= 'c x)(closed-hat)
                            (= 's x)(snare)
                            (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                            :else (open-hat)))
                :C (fn [x] (clap))
                :BL (fn [x c]  (mytb303 :amp 0.3  :attack 0.05 :release 0.01 :decay 0.1 :sustain 0.8 :cutoff (+ -20 c) :r 0.001 :wave 2 :note (note x)) )
                :BT (fn [x] (tsts :amp 0.1 :freq (midi->hz(note x))))
    :S (fn [x] (snare :amp 0.7 :freq 880 :decay 20 ))
                   })

(seq/set-beat  {
                :B  '[c - - -]
                ;;:S  '[-  c - ] ;; i want to alt between [-  c - ] [c  c - ] [c  c c ]
               :S (flatten (map  #(repeat 3 %)  '([-  c - ] [c  c - ] [c  c c ])))
                :BL '[- [:c2 120] [:c2 120]  [:c2 120] ]
                { :voice :H :il 3} '[c c - c e c c -]
                ;;:BT '[- :c4 :c3 :c3 ]
                })


(def throat (sample (frp 244155)))

(definst throat2s []
  (* 2 (play-buf :num-channels 2 :bufnum throat :rate 2)))

(comment
  ;;maybe some throat singing backdrop?
  ;;https://www.freesound.org/people/Metzik/sounds/244155/
  ;; or this one: http://www.freesound.org/people/djgriffin/sounds/15488/
  ;; lyrics could be "transheimat:the grid"

  ;; for "the grid" also electric noises, and some moog bleeps
  ;; electricity
  ;;https://www.freesound.org/people/Halleck/sounds/19486/
  (seq/set-metro :bpm 300 :il 3)
  (inst-fx! mytb303 fx-chorus)
  (inst-fx! tsts fx-chorus)
  (inst-fx! mytb303 fx-echo)
  (inst-fx! mytb303 fx-distortion-tubescreamer)
  (inst-fx! mytb303 fx-distortion2)
    (inst-fx! mytb303 fx-feedback-distortion)
  (clear-fx mytb303)
  (inst-fx! tsts fx-chorus)
  (inst-fx! tsts fx-echo)
  (clear-fx tsts)

  (throat2s)
  (kill throat2s)
  (inst-fx! throat2s fx-chorus)
  (inst-fx! throat2s fx-reverb)
  (clear-fx throat2s)

  (seq/set-metro :bpm 500 :il 3)
  (inst-fx! closed-hat fx-chorus)
  (inst-fx! electro-hat fx-echo)
  (clear-fx closed-hat)
    (clear-fx electro-hat)
    (seq/play-metro)

)

;; a loop i thought of on the palma flight
(seq/set-beat  {
                ;;:B '[x -]
                :BT '[:c3 :d3 - - :c3 :d3 :c3 :d3
                      ;;the transpose isnt supposed to be 1 octave, more like 2 steps
                      ;; and i cant figure out how to transpose things!
                      ;; this seems to work (into [] (map #(find-note-name (+ 1 (note %))) '[:c3 :c4]))
                      :c2 :d2 - - :c2 :d2 :c2 :d2  ]
                })



(comment
  (seq/set-metro :bpm 200 :il 3)
  (inst-fx! tsts fx-echo)
  (inst-fx! tsts fx-distortion2)
  (inst-fx! tsts fx-chorus)
  (inst-fx! tsts fx-distortion-tubescreamer)
  (clear-fx tsts)
    (seq/play-metro)

)


;; a loop i thought of "dancing panda in the woods"
;; a panda dancing
(seq/set-beat  {
              :B '[- - - - x - - x]
                :F '[:b2 :d3 :c#3 :c#3 :c0 :c#3 :e3 :c0]
               :RO '[- - - - [70 0.9] - - [70 0.9]]
                ;;id like 2 rissets with different effect chains, how?
;                {:voice :RO :seq 2 } '[- - - - [2070 0.9] - - [1070 0.9]]
                ;;               :H '[c c  - ]
                ;;atm i do this with noiseflute, :c1 is because i dont know how to do long notes, so i make a low jump instead...
                ;;imagine the panda dancing now, in an eery moonlit forest
                ;;its not the panda of kids movies, its a scary panda!
                })

(defsynth fx-myamp
  "amp experiment."
  [bus 0 amp 1.5]
  (let [source (in bus)]
    (replace-out bus (* amp source))))

(comment
  (seq/set-metro :bpm 200 :il 3)
  (def sf (noise-flute :freq 20))

  ;;the effects go with the high freq rissets
  (inst-fx! risset fx-echo)
  (inst-fx! risset fx-distortion2)
  (inst-fx! risset fx-chorus)
  (inst-fx! risset fx-distortion-tubescreamer)
  (inst-fx! risset fx-reverb)
  (clear-fx risset)

  (inst-fx! noise-flute fx-distortion-tubescreamer)
  (inst-fx! noise-flute fx-chorus)
  (inst-fx! noise-flute fx-reverb)
  (inst-fx! noise-flute fx-myamp )

  (fx-myamp :bus 0 :amp 2) ;; i want to have the amp arg, but how do i know bus 0 is noise-flute?
  (ctl fx-myamp :amp 2);;dont work

  (clear-fx noise-flute)

    (seq/play-metro)



)

;;test some new insts for a change
(seq/set-drums
 {
  :S1 (fn [x] (asawbass x))
  :S2 (fn [x] (asyncsaw x))
  }
 )

(seq/set-beat
 {
;  :S1 '[220 -  440 - 880 - 1760 -]
  :S2 '[- 60 - - ]
})

;;(demo (mda-piano 440 ))
;;(piano)
(comment
  (seq/set-metro :bpm 2000 :il 3)
  (inst-fx! asyncsaw fx-distortion-tubescreamer )
  (inst-fx! asawbass fx-distortion-tubescreamer )
    (seq/play-metro)



)

;; * Song: upload me to your transhumanist utopia baby
;;gating experiments
(definst sin-gate [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4 gate 1]
    (*
     ;;env    (env-gen (adsr att decay sus rel) gate :action FREE)
              (env-gen (adsr attack sustain release)  gate :action FREE)
     ;;(sin-osc freq)
              (sin-osc freq)
     vol))
;;(sin-gate)
;;(ctl sin-gate :gate 0)

(defn sin-gate-fn [& arg]
  ;;    (println arg)
  ;;(S) - stop the tone
  ;;(:freq 440 :gate 1) - call the inst with these args, gate 0 the prev note
  ;;- (dash) - do nothing, which means the prev note lives on due to gate
  ;; this is kinda dumb, i should read up how u do pattern matching in clojure
      (cond
        (= '(S) arg) (do
                       ;;(println "stop " arg)
                       (ctl sin-gate :gate 0))
        (and (= 1 (count arg)) (keyword? (first arg)))
        (do
          ;;(println "note " (first arg))
          (ctl sin-gate :gate 0)
          (sin-gate :freq (midi->hz (note (first arg)))))
        (sequential? arg) (do
                            ;;(println "seq " arg)
                            (ctl sin-gate :gate 0) (apply sin-gate arg))
        ) )



;;(sin-gate-fn :a6)
;;(sin-gate-fn :freq 220)

(def barkfx-samples (load-samples  "/home/joakim/roles/Creative/music/overtone-sylt/minisylt2024/barkfx/*.wav"))

(definst barkfx [bufnum 0]
    (play-buf :rate 0.5  :num-channels 1 :bufnum bufnum))


(defn barkfx-n [n]
  (barkfx (nth barkfx-samples n)))

(barkfx-n 2)
(def barkfx-next (atom 0))

(defn barkfx-n-next []    (barkfx-n (swap! barkfx-next #(if (= (dec (count barkfx-samples)) %) 0 (inc %)))))

(seq/set-drums
 {
  :G
  sin-gate-fn
  ;;:G sin-gate
  :bfx (fn[x] (barkfx-n x))
  :bfn (fn[x] (barkfx-n-next))
  }
 )
;;(sin-gate (midi->hz (note :a3)))
(demo (sin-osc (midi->hz (note :a3))))
(demo (sin-osc (midi->hz (note :g#3))))
(demo (sin-osc (midi->hz (note :b3))))
(demo (sin-osc (midi->hz (note :a3))))

(seq/set-beat
 {
  ;;:G '[:a3   -  :a4 S [:freq 880] S [:freq 1760] S]
  :G '[:a3   -  :a4 S :a5 S :a6 S
       :g#3   -  :g#4 S :g#5 S :g#6 S
       :b3   -  :b4 S :b5 S :b6 S
       :a3   -  :a4 S :a5 S :a6 S
       ]
  {:voice :bfx :il (* 3 5 4)} '[0  1 0 2 0 3 0 ;; just sounds
                                15 16 ;;deconstruction
                                9 8  6  5 4 3 ;; nust sounds
                                13 ;; upload me
                                ;;7 ;; this is the modem sound which is thematic, but breaks the mood a bit
                                ]
  })
;;(barkfx-n 7) modem
;;(midi->hz (note :a3))

(comment
  (seq/set-metro :bpm 200 :il 3)

  (inst-fx! sin-gate fx-distortion-tubescreamer )
  (inst-fx! sin-gate fx-echo )
  (inst-fx! sin-gate fx-g-verb)
  (inst-fx! sin-gate fx-resonz)
  (inst-fx! sin-gate fx-chorus)
  (clear-fx  sin-gate)
  (inst-volume! sin-gate 0.1)
)

;; * experiments
;; (
;; SynthDef(\lineReset, { |out, start= 0, end= 1, dur= 1, t_trig= 1, run= 1|
;;     var phasor = Sweep.ar(t_trig, run / dur).linlin(0, 1, start, end, \minmax);
;;     phasor.poll;
;;     Out.ar(out, SinOsc.ar(phasor, 0, 0.2));
;; }).add;
;; )
;; a = Synth(\lineReset, [\start, 400, \end, 800, \dur, 2])
;; a.set(\t_trig, 1)
;; a.set(\run, 0)
;; a.set(\run, 1)
;; a.set(\t_trig, 1)
;; a.free
(definst  line-reset [start 0 end 1 dur 1 t-trig 1 run 1]
  (let [phasor (sweep:ar t-trig (/ run dur ))
        linp (lin-lin phasor 0 1 start end)
        out (sin-osc linp 0 0.2)]
    out)
  )


(defsynth fx-pitcher
  [bus 0 pitch 0.1]
  (let [src (in bus)]
    (replace-out bus
                 (pitch-shift:ar src
                                 0.1
                                 pitch;;(mouse-x:kr 0 2)
                                 0
                                 0.004)
                 )))


(comment
  (demo (sin-osc))
  (def a (line-reset 400 800 2))
  (ctl a :run 0)
  (ctl a :run 1)
  (ctl a :t-trig 0)
  (ctl a :t-trig 1)



  ;; same but experiment with a latch
  (definst ab-player2 [bufnum 0 offset 0 latchtrig 0]
    (let [myphasor (+ offset  (phasor:ar :rate (buf-rate-scale bufnum)
                                        :end (buf-frames bufnum)))
          latch (latch:ar myphasor latchtrig)
          latchphasor (+ latch myphasor)
          myout (buf-rd :bufnum bufnum
                   :phase latchphasor)
          ]
          (out [0 1] myout))
    )

  ;;i dont remember how to play samples atm, but this works
  (definst babble [rate 0.5](play-buf :rate rate  :num-channels 1 :bufnum  (nth  babble-samples 1) :loop true))
  (def b (babble))
  (inst-volume! babble 80)
  (inst-fx!  babble fx-echo)
  (inst-fx!  babble fx-reverb)
  (clear-fx babble)
  (kill b)
  (ctl b :rate -20)
  (ctl b :rate 0.45)

  ;; experiment sending signal to overtone, crashes atm
  ;; create new id
 (def uid (trig-id))

 ;; define a synth which uses send-trig
 (defsynth foo [t-id 0 trigger 0] (send-trig:ar (k2a trigger) t-id (out [0 1](sin-osc))))
 ;; register a handler fn
 (on-trigger uid
             (fn [val] (println "trig val:" val))
             ::debug)

 ;; create a new instance of synth foo with trigger id as a param
 (def tt (foo uid))



(ctl tt :trigger 1)



(demo (ticker))
(demo (ping :note 120 :decay 1))
(inst-fx! ping fx-echo)
(inst-fx! ping fx-chorus)
(demo (risset :freq 700))


;;these dont work, i dont have mdapiano installed on fedora apparently
(demo (mda-piano))
(demo (piano))

;;weird mouse controlled instr, a bunch of them are i guess!
(demo (ks-stringer))
(inst-fx! ks-stringer fx-chorus)

(inst-fx! ks1-demo fx-echo)
(inst-fx! ks1-demo fx-resonz)
(inst-fx! ks1-demo fx-chorus)
(inst-fx! sampled-piano fx-echo)
(clear-fx ks1-demo)

(defsynth fx-resonz
  [bus 0 freq 200 bwr 0.05]
  (let [src (in bus)]
    (replace-out bus (resonz src freq bwr))))


(defn wavething []
  ;;(demo (sampled-piano))
    (ks1-demo )
    (barkfx-n (swap! barkfx-next #(if (= (dec (count barkfx-samples)) %) 0 (inc %))))
    (mystr2 110)
    (mystr2 (midi->hz (note :a2)))
    (mystr2 440)
    (mystr 440)
;;    (mystr 440)
    ;;(mystr 880)
  )

(wavething)
(inst-volume! mystr 0.1)


(definst mystr2 [freq 440]
  (let
      [fenv   (env-gen (perc 0.5 10) :action FREE)
       freqlfo  (lin-lin (lf-tri:kr  3.4) -1 1 1 1.01)
       apulsel (pulse:ar (* freqlfo freq) (lin-lin (sin-osc:kr 6) -1 1 0.5 0.8  ))
       apulser (pulse:ar (* freqlfo freq) (lin-lin (sin-osc:kr 6) -1 1 0.5 0.8  ))       
       asaw1 (lf-tri:ar (* 2 (* freqlfo freq)))
       asaw2 (lf-tri:ar (/ (* freqlfo freq) 2))
       srcl (+  (* 0.1 asaw1)  (* 0.1 asaw1)
                apulsel)
       srcr (+  (* 0.1 asaw1)  (* 0.1 asaw1)
                apulser)

       srcl (rlpf srcl (* fenv 1500) 2 )
       srcr (rlpf srcr (* fenv 1500) 2 )
       ]
    [srcl srcr]))
(mystr2)
(inst-volume! mystr2 1)
(inst-fx! mystr2 fx-chorus)
(inst-fx! mystr2 fx-echo)
(inst-fx! mystr2 fx-resonz)
(inst-fx! barkfx fx-g-verb )
(inst-fx! barkfx fx-echo)
(demo (fm-demo))
(demo (harmonic-swimming))
(demo (overtone.inst.drum/noise-snare))



(definst my-harmonic-swimming
  [amp 0.5]
  (let [freq     1000
        partials 200
        z-init   0
        offset   (line:kr 0 -0.02 60)
        snd (loop [z z-init
                   i 0]
              (if (= partials i)
                z
                (let [f (clip:kr (mul-add
                                   (lf-noise1:kr [(+ 16 (rand 8))
                                                  (+ 16 (rand 8))])
                                   0.2 offset))
                      src  (f-sin-osc (* freq (inc i)))
                      newz (mul-add src f z)]
                  (recur newz (inc i)))))]
     (pan2 (* amp snd))))

(demo (my-harmonic-swimming))

;; pitch shifting
(definst pitcher []
  (pitch-shift:ar (sound-in:ar [0 1])
                  0.1
                  (mouse-x:kr 0 1.5)
                  0
                  0.004)
  )
(demo (pitcher))

;;https://soundcloud.com/joakimv/cybernetic-brainwave-transformation-unit
(seq/set-drums
 {:I (fn [x] (sampled-piano x))
  :P (fn [x] (ctl pitchoid :pitch x))
  :W (fn [x] (do (kick) (wavething)  ))
  }
 )
(seq/set-beat
 {:I '[40 50 60 70 90]
  :P '[0.1 0.5 0.4 ]
  :W '[1 - - - - - - - - ]
  }
 )


(inst-fx! sampled-piano fx-echo)
(inst-fx! sampled-piano fx-chorus)
(seq/set-metro :bpm 40 )
(seq/play-metro)


(def pitchoid (inst-fx! sampled-piano fx-pitcher))

(ctl pitchoid :pitch 0.3)

(clear-fx sampled-piano)
(demo (sampled-piano 40))
(inst-fx! barkfx fx-pitcher)
(inst-fx! babble fx-pitcher)

(stop)
(clear-fx babble)
(demo (babble))
(demo (barkfx))

;; try with pitchshiftpa, but isnt here, need to load a quark
;;https://github.com/dyfer/PitchShiftPA
(definst pitcher2 []
(pitch-shift-pa)
  )

(definst pitcher3 []
    (pitch-shift:ar (sin-osc [100 200])
                    0.1
                    (mouse-x:kr 0 2)
                    0
                      0.004))
(demo (pitcher3))
(demo (sin-osc [100 200 400]))

(demo (sampled-piano))

(make-synth)


;;pitch voices instead
(def pitchoid (inst-fx! barkfx fx-pitcher))
(def pitchoid (inst-fx! babble fx-pitcher))
(clear-fx barkfx)

)


;; * song: ensol nevolen  (pacis)
(def pacis-samples (load-samples  "/home/joakim/roles/Creative/music/overtone-sylt/minisylt2024/pacis4_6_edit_split/*.wav"))

(definst pacis [bufnum 0]
    (play-buf :rate 0.5  :num-channels 1 :bufnum bufnum))


(defn pacis-n [n]
  (pacis (nth pacis-samples n)))

;;(pacis-n 2)
(def pacis-next (atom 0))

(defn pacis-n-next []    (pacis-n (swap! pacis-next #(if (= (dec (count pacis-samples)) %) 0 (inc %)))))
;(pacis-n-next)

(def pitchoid (inst-fx! pacis fx-pitcher))
;; (inst-fx! pacis fx-g-verb)
;; (inst-fx! pacis fx-chorus)
(inst-fx! dub-kick fx-echo)
(clear-fx pacis)
(clear-fx dub-kick)

(inst-fx! barkfx fx-echo)

(clear-fx pacis)

(def voc-a (buffer 2048))
(def voc-b (buffer 2048))


;;the vocoder is modulated on track 4 atm,
;; currently i think the version where you turn off the modulation and just have a low bass, and no
;; very little ha ha, on 7, is very cool

(do (clear-fx pacis)
    (defsynth fx-vocoder [bus 0 freq 440]
  (let [input (in bus)
        carrier (mix [(saw (* 1.01 freq)) (saw (* 0.99 freq))
                      (saw (* 2 freq)) (saw (* 4 freq)) (saw (* 8 freq)) ;;(saw (* 16 freq)) ;;(saw (* 32 freq)) ;; you can add these incrementally
                      ])
        fft-in (pv-mag-smear (fft voc-a input) 20) ;; vary the 0 from 0 to 40 for different sounds
        fft-carrier (pv-mag-smear (fft voc-b carrier) 1)
        formed (pv-mul fft-in fft-carrier)
        audio (ifft formed)
        normalized (normalizer audio)]
    (replace-out bus normalized)
    ))
    (def vocoid (inst-fx! pacis fx-vocoder)))

;; (def band-frequencies
;;   (let [bands 16]
;;     (map #(Math/exp (/ % bands)
;;                     (Math/log 100)
;;                     (Math/log 8000)) 
;;          (range 1 (inc bands)))))

(def band-frequencies
[100.0, 129.19693817759275, 166.81005372000593, 215.44346900318845,
278.2559402207124, 359.3813663804626, 464.15888336127773,
599.4842503189409, 774.263682681127,
1000.0, 1291.9693817759262, 1668.100537200059,
2154.434690031884, 2782.5594022071235,
3593.813663804626, 4641.588833612776]
)

(defsynth fx-vocoder2
  [bus 0 amp 0.1 freq 440 gated 1 pan 0]
  (let [input (in bus)
        ;;bands 16
        ;;band-frequencies (lin-exp (range 1 (inc bands)) 1 bands 100 8000)
        modulator input
        carrier (saw:ar freq)
        mod-localbuf (local-buf 2048)
        car-localbuf (local-buf 2048)
        env (env-gen:kr (env-asr 0.01 1 0.1) :gate gated :action 2)
        balance-voice (mix:ar (bpf:ar input band-frequencies :rq 0.2))
        analyse-freq
        (doall (for [buf [mod-localbuf car-localbuf]]
                 (pv-mag-smear (fft buf modulator) 1 )))
        mod-pv (first analyse-freq)
        car-pv (second analyse-freq)
        voice (ifft (pv-mag-mul car-pv mod-pv))
        sig (mix:ar [(white-noise:ar (balance-voice)) voice])]
    (replace-out bus (pan2:ar (* sig env) pan amp))
    ))

;; ;; Play the synth
;; (def inst (vocoder))

;; ;; Route audio input channel, adjust to match your setup
;; (connect s "in_1" inst "in")

;; ;; Add freq sweep
;; (def metro (metronome 60))
;; (def freqs (lin-lin (range) (- 10) 10 60 200))
;; (def mel-freq (demand:kr metro 0 (dseq freqs INF)))

;; (definst note () 
;;   (send inst "freq" mel-freq))





(defsynth fx-vocoder-stereo [bus 0 freq 440]
  (let [input (in bus)
        srcl (mix [(saw (* 1.03 freq)) (saw (* 0.97 freq))])
        srcr (mix [(saw (* 1.01 freq)) (saw (* 0.99 freq))])
        formedl (pv-mul (fft voc-a input) (fft voc-b srcl))
        formedr (pv-mul (fft voc-a input) (fft voc-b srcr))        
        audiol (ifft formedl)
        audior (ifft formedr)        
        normalizedl (normalizer audiol)
        normalizedr (normalizer audior)
        ]
    (replace-out bus [normalizedl normalizedr])
    ))

;;redefine, orig buggy
(definst my-grunge-bass
  [note 48 amp 1 dur 0.1 a 0.01 d 0.01 s 0.4 r 0.01 pan 0.0]
  (let [freq    (midicps note)
        env     (env-gen (adsr a d s r) (line:kr 1 0 (+ a d dur r 0.1))
                         :action FREE)
        src     (saw [freq (* 0.98 freq) (* 2.015 freq)])
        src     (clip2 (* 1.3 src) 0.9)
        sub     (sin-osc (/ freq 2))
        filt    (resonz (rlpf src (* 8.4 freq) 0.29) (* 2.0 freq) 2.9)
        meat    (ring4 filt sub)
        sliced  (rlpf meat (* 2 freq) 0.1)
        bounced (free-verb sliced 0.8 0.9 0.2)]
    (pan2 (* amp env bounced) pan 8          )
  ;;  (* amp env bounced)
    ))

;;(inst-fx! pacis fx-echo)
(inst-volume! pacis 0.2)
(inst-volume! closed-hat 0.2)
(seq/set-drums
 {:I (fn [x]     (pacis-n-next))
  :P (fn [x] (ctl pitchoid :pitch x))
  :PV (fn [x] (ctl vocoid :freq (midi->hz (note x))))
  ;;:W (fn [x] (do (kick) (wavething)  ))
  :B (fn [x]
                     (cond
                       (= 'c x)(electro-kick)
                       (= 'o x) (dance-kick)
                       (= 'd x) (dub-kick)
                       (= '4 x) (kick4)
                       (= '3 x) (kick3)
                       (= '2 x) (kick2)
                       (= '1 x) (kick)
                       :else (do (electro-kick) (dance-kick)))
       )
                  :H (fn [x] (cond
                            (= 'e x)(electro-hat)
                            (= 'c x)(closed-hat)
                            (= 's x)(snare)
                            (= 'o x)(open-hat :amp 1 :t 0.1 :low 10000 :hi  2000  )
                            :else (open-hat)))
  :bfx (fn[x] (barkfx-n x))
  :V2 (fn [x] (bass (midi->hz (note x))))
  :V4 (fn [x y z]
        ;;(bass :freq (midi->hz (note  x)) :amp y :t 0.1)
        (my-grunge-bass :note (note  x) :amp (* 4 y) :t 0.1 :pan z) ;; needs adapted grunge-bass
        ;;(ks-stringer :freq (midi->hz (note  x)) :amp y :t 0.1)
        ;;(ks1 :note (note  x) :amp y :t 0.1 )
        )
  :V3 (fn [x] (my-grunge-bass  (note x)))
  }
 )

(seq/set-beat
 {{:voice :I :il 0} '[ - - - -  x - - -  x - - -  - - - -]
  :P '[1.0 1.1 1.2 1.0    1.1 1.12 0.9 1.12    1.3 0.1]
  ;;:W '[1 - - - - - - - - ]
  :B '[3 4 2 1]
  :PV '[:a2 :b2 :c3]
  {:voice :B} '[d - - - d - - - d - - -  d - - -]
  :H '[c c s c  c c s c  c c s c  c c c c]
  {:voice :bfx :il 0} '[ - - - -   - - - -    - - - -  0 - - - ]
  
  }
 )


;; ensol hard techno?
;; movo ensol nevolen, dance in the peaceful universe

(inst-fx! bass fx-echo)
(inst-fx! bass fx-chorus)
(inst-volume! bass 2)
(clear-fx bass)

(inst-fx! grunge-bass fx-chorus)
(inst-volume! grunge-bass 2)

(seq/set-metro :bpm 240 :il 3)
(seq/set-beat
 {{:voice :I :il 3} '[ - - - -  x - - -  x - - -  - - - -]
  {:voice :V3 :il 1} '[:a4 :a4 -  :b4 :a4 :b4 :a4 -]
  :V2 '[:a2 :b2 :c2 :d2]
  {:voice :PV :il 3} (fn [] (conj (shuffle '[:a2 :b2 :c3 ]) ':d2))
  ;;{:voice :PV :il 1} '[:a2 - - :b2 - -  :c3 :d2 - -]
  ;;:W '[1 - - - - - - - - ]
  :B '[c -  ]
  {:voice :H :id 2} '[s s  ]  
  {:voice :B} '[d - - - d - - - d - - -  d - - -]
  :H '[ c o]
  ;;{:voice :bfx :il 0} '[ - - - -   - - - -    - - - -  0 - - - ]
  
  }
 )

;; i want to make a pseudo echo
;; this is a song, hyperhouse!
(seq/set-beat
 {
  :B '[2 3 3 3]
  ;; {:voice :V4 :il 3 :echo 1} '[[:a3 0.5] - - - [:b2 0.5] - - -  [:c2 0.5] - - - [:d2 0.5] - - - ]
  ;; {:voice :V4 :il 3 :echo 2} '[- [:a3 0.4] - - - [:b2 0.4] - - -  [:c2 0.4] - - - [:d2 0.4] - - ]
  ;; {:voice :V4 :il 3 :echo 3} '[ - - [:a3 0.2] - - - [:b2 0.1] - - -  [:c2 0.1] - - - [:d2 0.1] - ]
  ;; {:voice :V4 :il 3 :echo 4} '[ - - - [:a3 0.1] - - - [:b2 0.05] - - -  [:c2 0.05] - - - [:d2 0.05] ]  
  {:voice :V4 :il 3 :echo 1} '[[:a3 0.5 0] - - - [:b2 0.5 0] - - -  [:c2 0.5 0] - - - [:d2 0.5 0] - - - ]
  {:voice :V4 :il 3 :echo 2} '[- [:a3 0.4 -1] - - - [:b2 0.4 -1] - - -  [:c2 0.4 -1] - - - [:d2 0.4 -1] - - ]
  {:voice :V4 :il 3 :echo 3} '[ - - [:a3 0.2 1] - - - [:b2 0.1 1] - - -  [:c2 0.1 1] - - - [:d2 0.1 1] - ]
  {:voice :V4 :il 3 :echo 4} '[ - - - [:a3 0.1 0] - - - [:b2 0.05 0] - - -  [:c2 0.05 0] - - - [:d2 0.05 0] ]  
  ;; {:voice :V4 :il 3 :echo 1} '[[:a2 0.5] - - - ]
  ;; {:voice :V4 :il 3 :echo 2} '[- [:a2 0.2] - - ]
  ;; {:voice :V4 :il 3 :echo 3} '[ - - [:a2 0.1] -]
  ;; {:voice :V4 :il 3 :echo 4} '[ - - - [:a2 0.05]]  
  
; {:voice :V4 :il 0 :echo 2} '[  - [:d2 0.4] - - - [:a2 0.4] - -  - [:b2 0.4] - - -  [:c2 0.4] - -]
;  {:voice :V4 :il 0 :echo 3} '[ - - [:c2 0.3] - - - [:d2 0.3] - - - [:a2 0.3] [:b2 0.3] -]
                                        ;  {:voice :V4 :il 1 :echo 5} '[  [:b2 0.05] [:c2 0.05]  [:d2 0.05] [:a2 0.05]]
  {:voice :bfx :il 127 } '[ 0 1 2 3]
  }
 )








;; (defsynth fx-g-verb
;;   "can i copy paste program my own reverb?"
;;   [bus 0 wet-dry 0.5 room-size 0.5 dampening 0.5]
;;   (let [source (in bus)
;;         verbed (g-verb source 200 8)]
;;     (replace-out bus (* 1.4 verbed))))




(

(:group (:fx-group pacis))
(synth? (:mixer pacis))
(synth? fx-echo)
(:volume pacis)
(:bus pacis)
(volume 2)

(seq/play-metro)
(stop)

(show-graphviz-synth  (:synth (:mixer pacis)))
(:synth (:mixer pacis))
(show-graphviz-synth  pacis)
(show-graphviz-synth fx-echo)

;; workaround, since orig didnt work for unknown reaszons

;; seems cider can wind up in a bad state
;; then sh wont work
;; this fails in a bad 
(println (clojure.java.shell/sh "ls"))

;; it did work to start a cider repl from a shell, and then connect to the port
;;  /home/joakim/roles/Tools/bin/clojure -Sdeps \{\:deps\ \{nrepl/nrepl\ \{\:mvn/version\ \"1.0.0\"\}\ cider/cider-nrepl\ \{\:mvn/version\ \"0.45.0\"\}\}\ \:aliases\ \{\:cider/nrepl\ \{\:main-opts\ \[\"-m\"\ \"nrepl.cmdline\"\ \"--middleware\"\ \"\[cider.nrepl/cider-middleware\]\"\]\}\}\} -M:cider/nrepl
;;(cider-connect '(:host "localhost" :port 42329))

;; need bot host and port, otherwise gpg tries to run for mysterious reasons

)


