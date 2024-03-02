;; * namespace declaration
(ns insane-noises.core
  (:require [overtone.api])
  (:use
   [overtone.live] ;; overtone boots an external scsynth
   [overtone.inst synth piano drum]
   [overtone.examples.instruments space monotron]
   )
  )

;; just to see everything works and we get sound, rig connections with qjackctl
(demo (sin-osc))
;; * Some industrial soounding beats
;; I modified some of the dubstep in the tutorials to arrive at these nice harsh beats.

(definst industrial [bpm 250 wobble 1 note 32 snare-vol 1 kick-vol 1 v 1]
 (let [trig (impulse:kr (/ bpm 120))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (saw (* freq [0.99 1.01])))
       wob (lpf wob sweep)
       wob (* 0.8 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

       kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0
                                   (dseq [1 0 0 0
                                          1 0 0 0
                                          1 0 0 0
                                          1 0 0 0] INF)
                                   )) 0.7)
       bassenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0
                                   (dseq [1 0 1 0
                                          1 0 1 0
                                          1 0 1 0
                                          1 0 1 0] INF)
                                   )) 0.7)       

       kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       kick (clip2 kick 1)

       bass (* (* bassenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       bass (clip2 bass 1)

       snare (* 3 (pink-noise) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
       snare (+ snare (bpf (* 4 snare) 2000))
       snare (clip2 snare 1)]

      (* v (clip2 (+ wob bass (* kick-vol kick) (* snare-vol snare)) 1))))

;; to be used in "engine room" song

(definst industrial2 [bpm 250 wobble 1 note 32 snare-vol 1 kick-vol 1 v 1 wobble-vol 1 bass-vol 1]
 (let [trig (impulse:kr (/ bpm 120))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (saw (* freq [0.99 1.01])))
       wob (lpf wob sweep)
       wob (* 0.8 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

       kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0
                                   (dseq [1 0 0 0
                                          1 0 0 0
                                          1 0 0 0
                                          1 0 0 0] INF)
                                   )) 0.7)
       bassenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0
                                   (dseq [1 0 1 0
                                          1 0 1 0
                                          1 0 1 0
                                          1 0 1 0] INF)
                                   )) 0.7)       

       kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       kick (clip2 kick 1)

       bass (* (* bassenv 7) (lpf (saw 50)) 25)
       bass (clip2 bass 1)

       snare (* 3 (pink-noise) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
       snare (+ snare (bpf (* 4 snare) 2000))
       snare (clip2 snare 1)]

       (* v (clip2 (+ (* wobble-vol wob)
                             (* bass-vol bass)
                             (* kick-vol kick) (* snare-vol snare)) 1))))

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
;; * sample players
  ;;play a sample with buf-rd and a phasor, the phasor is a ramp
  (definst ab-player [bufnum 0 offset 0]
    (out [0 1] (buf-rd :bufnum bufnum
                   :phase (+ offset (phasor:ar :rate (buf-rate-scale bufnum)
                                               :end (buf-frames bufnum))))))


  ;;play a sample with buf-rd and sweep as phasor
  (definst ab-player-sweep [bufnum 0 offset 0 dur  1 t-trig 1 run 1]
    (let [phasor1 (sweep:ar :trig t-trig
                            :rate (/ run dur);;(buf-rate-scale bufnum)
                            )
          phasor2 (lin-lin phasor1 0 1 0 (buf-frames bufnum))]
      (buf-rd :bufnum bufnum
              :phase (+ offset phasor2))))


;; * In the Engine Room
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

(comment
  ;; start the industrial drone slow   ;; base bpm is 250, but since its an engine, i want to change bpm
  (do (def d (industrial2 :bpm 0))
      (truck)
      (inst-volume! truck 3)
      (ctl-ramp 0 250 16000  #(ctl d :bpm %))
      ;; start automatic mods of industrial
      (auto-ind2 0)
      )

  ;; TODO :dur should be set automatically somehow, now i have to figure it out by hand;;
  ;; investigate (:duration buf)   (:duration babble-samples)  (according to mail)
(reset! curoffset 0)
(do (def sw-babble (ab-player-sweep :dur 80))  (inst-volume! ab-player-sweep 10))

(inst-fx! ab-player-sweep fx-reverb)
(inst-fx! ab-player-sweep fx-echo)
(inst-volume! ab-player-sweep 10)
(clear-fx ab-player-sweep)
(ctl sw-babble :offset  (mkoffset -10000))  

(ctl sw-babble :trig 0)
(ctl sw-babble :trig 1)
;; pause sample
(ctl sw-babble :run 0)
;; restart sample
(ctl sw-babble :run 1)
;; rewind sample a bit

(stop)

  
(stop)
  

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
  (ctl-ramp 0 1 16000  #(inst-volume! industrial2 %))
  
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
  (stop)
  (inst-fx! babble fx-reverb)
  (inst-fx! babble fx-echo)
  (inst-volume! babble 10)
  (clear-fx babble)
  (keys babble);;(:name :params :args :sdef :group :instance-group :fx-group :mixer :bus :fx-chain :volume :pan :n-chans)
  (get 0 (:ugens (:sdef babble)))
  (get-in babble [:sdef :ugens ] )
  (ctl babble :vol 1)
  )

;; * First, we'll define some percussive instruments

;; this high hat instrument takes a white noise generator and shapes
;; it with a percussive envelope

(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 1 src env)))

(comment
  (hat)
  )

;; * some basic tutorial code

;;just a workaround because i didnt have stuff cached

(defn frp [x]

;  "/home/joakim/.config/google-chrome-unstable/Default/Extensions/bepbmhgboaologfdajaanbcjmnhjmhfn/0.1.1.5023_0/audio/1_short_Open_16_16.wav"
  (freesound-path x)
  )

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
  (stop)
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
  (stop)
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
  (stop)
  
  )

(defn mytb303 []
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

(definst myblip
  [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat 
                      (g-verb
                       (blip [freq note]))))
        audio (* amp env sig)]
    audio))

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


;; * simple beats
;; continues on plhat beats

(defn simple-beats [m beat-num]
  (at (m (+ 0 beat-num))   (mytb303) (myh1) (mykick) (myseq 50)
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

;; psy beats
;; here i tried to get a psytrance feeling, but it wound up as something else. still trancey though.


(defn psykick []
  (kick4 40 :decay 2 )
  (kick 50 :decay 2 )
  (dance-kick 40 :decay 0.25 )
  )
(defn psysnare []
  (noise-snare :decay 0.7 )
  )

(definst psybass
  [note 60 amp 0.7 attack 0.001 release 0.2 numharm 200]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat 
                      (g-verb
                       (blip [freq note ] :numharm numharm))))
        audio (* amp env sig)]
    audio))

(definst psybass2
  [note 60 amp 0.7 attack 0.001 release 0.2 numharm 200]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat 
                      (g-verb
                       (blip [freq note ] :numharm numharm))))
        audio (* amp env sig)]
    audio))

(definst psybass3
  [note 60 amp 0.7 attack 0.001 release 0.2 numharm 200]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        sig   (apply +
                     (concat 
                      
                      (bpf (saw [freq note ]) numharm )))
        sig2   (apply +
                     (concat 
                      
                      (bpf (saw [freq note ])  numharm )))

        audio (* amp env sig)
        audio2 (* amp env sig2)]
    [audio audio2]))


(defn psyh1 []
  (closed-hat 1 :low 1000 :hi 1500)
  )

(defn rand-int-range [a b]
  (+ a (rand-int (inc (- b a)))))

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



;; * In the clone vats
(comment
  (def babble-samples (load-samples  "/home/joakim/roles/music/overtone-sylt/sylt2023/bark_samples/*wav"))
  (def hyperrat-sample (load-sample "/home/joakim/roles/Creative/music/hyperrat2/output.wav"))
  (definst hyperrat [rate 0.5](play-buf :rate rate  :num-channels 1 :bufnum  hyperrat-sample :loop false))
  (hyperrat :rate 0.6)
  
  (definst ab-player-sweep [bufnum 0 offset 0 dur  1 t-trig 1 run 1]
    (let [phasor1 (sweep:ar :trig t-trig
                            :rate (/ run dur);;(buf-rate-scale bufnum)
                            )
          phasor2 (lin-lin phasor1 0 1 0 (buf-frames bufnum))]
      (buf-rd :bufnum bufnum
              :phase (+ offset phasor2))))
  
  (def hyperrat-inst (ab-player-sweep :bufnum hyperrat-sample :dur 180))
  
  (inst-volume! ab-player-sweep 32)
  (inst-fx! ab-player-sweep fx-echo)
  (inst-fx! ab-player-sweep fx-echo)
  (clear-fx ab-player-sweep)
  (stop)
  
  (metro :bpm 480)
  (psy-beats metro (metro))
;;  (psybass metro (metro))
  
  (inst-fx! overpad fx-echo)
  (inst-fx! overpad fx-chorus )
  (clear-fx overpad)
  
  (inst-fx! closed-hat fx-echo)
  (inst-fx! closed-hat fx-chorus)
  (clear-fx closed-hat)

  (apply (choose [(fn [] (inst-fx! psybass fx-echo))
                 (fn [] (inst-fx! psybass fx-chorus))
                 (fn [] (inst-fx! psybass fx-reverb))
           (fn []
             (clear-fx psybass)
             )
             ])
         nil)
  (inst-fx! psybass2 fx-distortion-tubescreamer)
  (clear-fx psybass2)
  (do
    (inst-fx! psybass fx-echo)
    (inst-fx! psybass fx-chorus)
    (inst-fx! psybass fx-reverb))
  (clear-fx psybass)

  
  (stop)
)


;; * Forest Dream

;; first i played with an old track called am i alive. this variation didnt turn out as much yet.

;; then i wrote an entirely new song around this beat that turned out rather good, called forest dream!


(def dreamidx (agent 0))


(definst dream [bufnum 0]
  (play-buf :num-channels 1 :bufnum bufnum :rate 0.8)
  ;;(send dreamidx inc )
  ;;(if (< 10 @dreamidx) (send dreamidx inc ) (  ))

  )
(defn dream-inc [] (send dreamidx inc )(dream @dreamidx))
(def choir (sample (frp 46712)))
(def choir2 (sample (frp 65801)))
(def choir3 (load-sample (frp 65801)))
(def choir4 (load-sample (frp 46712)))
(definst choir2s []
  (* 64 (play-buf :num-channels 1 :bufnum choir3 :rate 1)))

(definst choir4s []
  (* 64 (play-buf :num-channels 2 :bufnum choir4 :rate 1)))

(comment
  ;; [2024-02-03 Sat]
  ;;experimenterar med extremt långsam beat
  ;; på nått sätt råkade technobabble komma in uppitchat vid (dream-inc), med effekter var det kul
  ;; oklart om det går att reproducera
  ;; I will christen this funny little interlude in my space opera:
  ;; spaceships meeting at relative velocities

  
  (metro :bpm 24)
  (simple-beats metro (metro))  
  (stop)
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

(comment
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
  (stop)


  )


;; * some dubstep varation
;; mostly copied

;; and combining ideas from sounds.clj with the rhythm ideas here:

;; first we bring back the dubstep inst

(definst dubstep [freq 100 wobble-freq 5]
  (let [sweep (lin-exp (lf-saw wobble-freq) -1 1 40 5000)
        son   (mix (saw (* freq [0.99 1 1.01])))]
    (lpf son sweep)))

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

;; * sam aaron examples from #emacs
;; some snippets which sam aaron on #emacs shared.

(comment
(demo 60 (g-verb (blip (mouse-y 24
           48) (mouse-x 1 100)) 200 8))

 (demo 60 (g-verb (sum (map #(blip (* (midicps (duty:kr % 0 (dseq
           [24 27 31 36 41] INF))) %2) (mul-add:kr (lf-noise1:kr 1/2) 3 4)) [1
           1/2 1/4] [1 4 8])) 200 8))
)

;; * dnb and amen beats
;; here i want some simple drum machine code to play with.

;; you can find lots of drum patterns on wikipedia, and you can convert them rather easily to lisp constructs.

;; then i combined with my other instruments to to form ... something.

;; reverse a beat: (map #(list (first %) (reverse (second %)) ) amen-beat)

;;the dummy beat thing is a workaround, because i really wanted some kind of forward declaration
(def dummy-beat)
(def *beat (ref dummy-beat))

(def *beat-count (ref 0))

(defn drum-set-beat [beat]
  (dosync (ref-set *beat beat)))





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
   ;:R2 '[ - :c4  :c4 - :c4 :c4 :c4 - :c4 :c4 :c4 - :c4 :c4 :c4 ]
;   :R2 '[ - :c2  :c2 :a2 - :c2 :c2 :a2 - :c2 :c2 :a2 - :c2 :c2 :b2 ]
   :R2 '[ - :c2  :c2 :c2 - :c2 :c2 :c2 - :c2 :c2 :c2 - :c2 :c2 :c2 ]
   :R  '[ - - - - 0 - - - - - - - 0 - - - ]   
   }
  )
  (drum-set-beat psy-beat)
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

(def *drums (ref my-drums))
(defn drum-set-drums [drums]
  (dosync (ref-set *drums drums)))

(defn drum-reverse-beat [pattern]
  (map #(list (first %) (reverse (second %)) ) pattern))


(comment
  (metro :bpm 400)
  (drum-set-drums my-drums)
  (play-drums-metro metro (metro))
  (drum-set-beat amen-beat)
  (drum-set-drums my-drums)
  (drum-set-beat psy-beat)
  (drum-set-beat (drum-reverse-beat psy-beat))  
    (drum-set-beat dnb-beat)
    (drum-set-beat (drum-reverse-beat amen-beat))
  (drum-set-beat dnb-beat)
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
  (stop)
  )

;re-def works, but not ref-set?
;;(sync (ref-set *beat amen-beat))
;;(def *beat (ref amen-beat))


(defn drum-reverse-beat [pattern]
  (map #(list (first %) (reverse (second %)) ) pattern))

(defn drum [voice pattern]
  (dosync (alter *beat conj [voice pattern])))

(defn get-drum-fn [voice]
  "get the drum function for the voice"

  (let* [voice1
         (if (map? voice)
           (get @*drums (:alias voice)) ;; this was dumb TODO remove
           (get @*drums voice)
           
           )]
        (if (fn? voice1) ;;TODO this isnt complete yet, 
          voice1
           (get @*drums (:alias voice1))
         )))

(defn clear-drums []
  (dosync (ref-set *beat [])))



;;allows for voice patterns of different length
;;undefined voices are dropped
(defn drum-fn2 []
              (let [i   @*beat-count]
                (doseq [[voice pattern]
                        @*beat
                        ]
                  (let* [index (mod i (count pattern) )
                        drum (get-drum-fn voice)]
                  (if (and drum (not (= '- (nth pattern index))))
                    (do
                      (apply drum [(nth pattern  index)])
                      )
                     )
                  ))
                (dosync (ref-set *beat-count (inc @*beat-count) ))))




;;play drums using a metronome strategy, which has advantages over the periodic strategy
(defn play-drums-metro [m beat-num]
  ;; reschedule next call to the sequencer
  (apply-at (m (+ 1 beat-num))
            play-drums-metro
            m
            (+ 1 beat-num)
            [])
  ;; schedule the drum voice
  (at (m beat-num)(drum-fn2))
  )

;; * Revenue Inspector
;; a totaly different kind of song!
;; it sounds a little bit like a train
;; start the monotrons, occasionally start the theremin, end with some rythms(somewhere else in the code)
(comment
  ;; do a couple of these with different pan
  (def N1 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0 1))
  (def N2 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0 0))
(ctl N1 :cutoff 600.0)
 ;; then occasionally these, which you then kill
(def st1 (space-theremin :out-bus 10 :amp 0.8 :cutoff 1000))
(space-reverb [:after st1] :in-bus 10)
(kill space-reverb)

;;then stop
(stop)
)


;; * Glassheads
;; previously called "escape from synthmen"



(definst grainy2 [b 0]
  (let [
        trate (mouse-y:kr 1 30)
        dur (/ 2 trate)]
    ;;i do this to get stereo compatibility ith the fx
    [
     (t-grains 2 (impulse:ar trate) b 1 (mouse-x:kr 0 (buf-dur:kr b)) dur 0 0.8 2)
     (t-grains 2 (impulse:ar trate) b 1 (mouse-x:kr 0 (buf-dur:kr b)) dur 0 0.8 2)
     ]
    ))

(definst grainy3 [b 0 vol 1]
  (let [
        trate (mouse-y:kr 1 30)
        dur (/ 2 trate)]
    ;;i do this to get stereo compatibility ith the fx
    [
     (* vol (t-grains 2 (impulse:ar trate) b 0.5 (mouse-x:kr 0 (buf-dur:kr b)) dur 0 0.8 2))
     (* vol (t-grains 2 (impulse:ar trate) b 0.5 (mouse-x:kr 0 (buf-dur:kr b)) dur 0 0.8 2))
     ]
    ))


(definst grainy4 [b 0]
  (let [
        trate (mouse-y:kr 1 30)
        dur (/ 2 trate)]
    ;;i do this to get stereo compatibility ith the fx
    [
     (t-grains 2 (impulse:ar trate) b 1 (mouse-x:kr 0 (buf-dur:kr b)) dur 0 0.8 2)
     (t-grains 2 (impulse:ar trate) b 1 (mouse-x:kr 0 (buf-dur:kr b)) dur 0 0.8 2)
     ]
    ))


;;20240205, smurf.wav is gone? dunno where ~/samples is now
;;(def organ-sample (load-sample "~/samples/organ.wav"))
;;(def organ-sample (load-sample "/home/joakim/smurf.wav"))
;;(def orgloop (sample "/home/joakim/smurf.wav"))
                                        ;(orgloop)
                                        ;(stop)
(def dr1 (load-sample   "/home/joakim/roles/Creative/music/am_i_alive_all/am_i_alive/01.wav"))
;(def glasscrash (sample (frp 221528)))
                                        ;(play-buf 1 organ-sample)
(def glasscrashsample (load-sample (frp 221528)))
(definst glasscrash [vol 1]
  (* vol (play-buf :num-channels 2 :bufnum glasscrashsample :rate 2)))


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
    (drum-set-beat silent-beat)
    ;;(play-drums 100 16)
    (metro :bpm 500)
    (drum-set-drums my-drums)
    (play-drums-metro metro (metro))

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
    (drum-set-beat dnb-beat)
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
      (drum-set-beat amen-beat)
)

      
    (drum-set-beat silent-beat)

    (do
      (kill grainy4) 
         (drum-set-beat silent-beat)
    (inst-fx! glasscrash fx-echo)
           (inst-fx!   glasscrash fx-reverb)
        (glasscrash)
)
  
 (stop) 


  
  (inst-fx! grainy3 fx-feedback-distortion)
      (clear-fx grainy3)
  (kill grainy2)
  (inst-fx! grainy2 fx-echo)  
  (inst-fx! grainy2 fx-feedback-distortion)
  (inst-fx! grainy2 fx-distortion)

  (glasscrash)
  
    (clear-fx grainy2)
  (stop)
)

;; * Industrial Wastelands
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

;;my monotron is the same as the example monotron except you can apply effects to it
(definst jvmonotron
  "Korg Monotron from website diagram:
   http://korg.com/services/products/monotron/monotron_Block_diagram.jpg."
  [note     60            ; midi note value
   volume   0.7           ; gain of the output
   attack 0.001 release 0.2 
   mod_pitch_not_cutoff 1 ; use 0 or 1 only to select LFO pitch or cutoff modification
   pitch    0.0           ; frequency of the VCO
   rate     4.0           ; frequency of the LFO
   int      1.0           ; intensity of the LFO
   cutoff   1000.0        ; cutoff frequency of the VCF
   peak     0.5           ; VCF peak control (resonance)
   pan      0             ; stereo panning
   ]
  (let [note_freq       (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        pitch_mod_coef  mod_pitch_not_cutoff
        cutoff_mod_coef (- 1 mod_pitch_not_cutoff)
        LFO             (* int (saw rate))
        VCO             (saw (+ note_freq pitch (* pitch_mod_coef LFO)))
        vcf_freq        (+ cutoff (* cutoff_mod_coef LFO) note_freq)
        VCF             (moog-ff VCO vcf_freq peak)
        ]
    ;(out 0 (pan2 (* volume env VCF) pan))
    [(* volume env VCF)  (* volume env VCF) ]
    ))




(comment
  (drum-set-beat wasteland-beat)
  (drum-set-drums wasteland-drums)
  ;;  (play-drums 200 32)
  (metro :bpm 300)
  (play-drums-metro metro (metro))
    ;(play-drums 200 33)
  ;;120 is nice, and 200 is also nice
(stop)
  (def syntheticmachine(sample (frp 249879))) ;synthetic machine
  ;;(syntheticmachine)
;  (def tst (sample (frp 257021))) ;mp3 cant be handled
(def hiss (sample (frp 130291  )))
(hiss)
  (hiss2)
  (steel)
  
  (door)
  (hiss3)
  (chain)
(steel)


(stop)
  (inst-fx! psybass3 fx-reverb)
    (inst-fx! psybass3 fx-echo)
  (inst-fx! psybass3 fx-chorus)
  (clear-fx psybass3 )
  
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
(stop)
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

(def lockstep-beat
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
  (drum-set-beat dnb-beat)
  (drum-set-beat wasteland-beat)
;;  (play-drums 200 16)
    (metro :bpm 400)
  (play-drums-metro metro (metro))

  (drum-set-beat lockstep-beat)
  (drum-set-drums lockstep-drums)

  (clear-fx noise-snare)
  (stop)
(kill simple-flute)


;;another qy to play with the beat
(drum-set-beat
  {
   :B  '[ 0 - - - - - - - - - 0 - - - - - ]
   :S  '[ - - - - 0 - - - - - - - 0 - - - ]
   :H  '[ c c c 0 c c c c c c c 0 c 0 c c ]
 ; {:voice :R2 :seq 2 }  '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
   :R2 '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
;      :R3 '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
   }
  )

  (drum-set-drums my-drums)
  (drum-set-drums lockstep-drums)
  (drum-set-drums wasteland-drums)
  (stop)

)



(def test-beat
  {
   :B  '[ 0 - - - - - - - - - 0 - - - - - ]
   :S  '[ - - - - 0 - - - - - - - 0 - - - ]
   :H  '[ c 0 c c]
   {:H :2}  '[ c 0 c c]
   :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
   {:voice :R2} '[ :c2 - - - :d#2 - - - - - :c2 - :d#2 :c2   - -   ]
   }
  )

  
(definst grainy5 [b 0 trate 20 dur3 1 attack 0.1 release 0.1]
  (let [
;;        trate (mouse-y:kr 1 30)
        dur (/ 2 trate)
        dur2  (* dur3 (buf-dur:kr b))
        env   (env-gen (perc attack release) :action FREE)
        ]
    ;;i do this to get stereo compatibility ith the fx
    [
     (* env (t-grains 2 (impulse:ar trate) b 1 dur2 dur 0 0.8 2))
     (* env (t-grains 2 (impulse:ar trate) b 1 dur2 dur 0 0.8 2))
     ]
    ))

;;drum system tests
(comment
  ;;all beats here
  (drum-set-beat silent-beat)
  (drum-set-beat psy-beat)
  (drum-set-beat dnb-beat)
  (drum-set-beat amen-beat)
  (drum-set-beat wasteland-beat)
  (drum-set-beat lockstep-beat)
  (drum-set-beat test-beat)

;; you dont need a symbol of course
(drum-set-beat  {
                 :B  '[ 0 - - - - - - - - - 0 - - - - - ]
                 {:voice :H :seq 2}  '[ 0 0 0 - - - - - - - 0 0 0 - - - ]
                 :S  '[ - - - 0 - - - - - - - 0 - - - ]
                 :H  '[ c 0 c ]
                 :Hsilent  '[ c 0 c  ]
                 :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
                 {:voice :R2 :seq 2}  '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]
    }
)


(drum-set-beat  [
                 [:B  '[ 0 - - - - - - - - - 0 - - - - - ]]
                 [:B  '[ - 0 - -  - - - - - - - 0 - - - -  ]]
                 [:S  '[ - - - 0 - - - - - - - 0 - - - ]]
                 [:R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]]
                 [{:voice :R2 }  '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]]
    ]
                )


(drum-set-beat  [
                 [:B  '[ 0 - - - - - - - - - 0 - - - - - ]]
                 [:B  '[ - 0 - - - - - - - - - 0 - - - -  ]]
                 [:S  '[ - - - 0 - - - - - - - 0 - - - ]]
                 [:R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]]
                 [{:voice :R2 :seq 3}  '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]]
    ]
                )


  ;; the drums
  (drum-set-drums my-drums)
  (drum-set-drums lockstep-drums)
  (drum-set-drums wasteland-drums)



(drum-set-beat  {
                 :C  '[ 0 - - - - - - - - - 0 - - - - - ]
 ;                {:voice :H :seq 2}  '[ 0 0 0 - - - - - - - 0 0 0 - - - ]
  ;               :S  '[ - - - 0 - - - - - - - 0 - - - ]
   ;              :H  '[ c 0 c ]
                 :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 :c2 :c2 :c2 :c2 :c2 ]
                 :R3 '[ :c3 :d#3 :c3 :d#3 :c3   :c3 :c3 :c3 :f3 :d#3 :c3 :c3 :c3   ]
    }
)


(drum-set-drums
  {:C (fn [x] (hat-demo))
   :R2 (fn [x]
        (tb303  :note (+ 12 (note x)) :r 0.9 :wave 0 :release 0.1)
        )
   :R3 {:alias :R2}
  })
  (inst-fx!  tb303 fx-echo)

;;flawed?
(drum-set-beat  {
                 :H_ '[ c  c c O ]
                 :B  '[ 0 - - - ]
                 :R2 '[ :e4 - - - :c4  - - - :d4 :e4 - - :d4 :c4 - -  ]
                 :R4 '[ :e6  :c6 :d6 :e6  :d6 :c6    ]
    }
)

  (drum-set-drums lockstep-drums)
  (inst-fx!  psybass3 fx-chorus)
  (inst-fx!  psybass3 fx-freeverb)
    (inst-fx!  psybass3 fx-echo)

  (clear-fx psybass3 )
  
  ;; use metronome strategy
  (metro :bpm 250)
  (metro :bpm 300)
  (metro :bpm 400)
  (metro :bpm 4000)
  ;;what does even the bpm mean?
  (play-drums-metro metro (metro))

  (cs80lead :freq 110 :fatt 0.9)
  (ctl cs80lead :freq 220 )
    (ctl cs80lead :freq 110)
    (ctl cs80lead :gate 0)

    (ctl cs80lead :fatt 0.1)
    (kill cs80lead)

    ;;this requires cs80lead to be started in advance, so it can then be controlled
(drum-set-beat  {
                 :C  '[ 0 - - - - - - - - - 0 - - - - - ]
 ;                {:voice :H :seq 2}  '[ 0 0 0 - - - - - - - 0 0 0 - - - ]
  ;               :S  '[ - - - 0 - - - - - - - 0 - - - ]
                 :H  '[ c 0 c ]
                 :R2 '[ :c2 :d#2 :c2 :d#2 :c2   :c2 :c2 :c2 :f2 :d#2 :c2 x :c2 x :c2 x ]
                 :R3 '[ :c4 :d#4 :c4 :d#4 :c4   :c4 :c4 :c4 :f4 :d#4 :c4 x :c4 x :c4 x ]
                 :B  '[ 0 - - - ]
;                :Q '[ 0 - - - - - - - -]
                 
    }
)
(drum-set-drums
 {  
  ;; here R2 is intended to be cs80lead. 'x turns it of, otherwise the freq is changed
  :R2 (fn [x]
        (if (= 'x x)
          (ctl cs80lead :gate 0)
          (do
                     (ctl cs80lead :gate 1)
                     (ctl cs80lead :freq         (midi->hz (+ 0 (note  x)))))))
  :R3    {:alias :R2}
  :B (fn [x](dub-kick) (kick    :amp-decay 2))
  :B2 (fn [x]  (tb303 :note (note x)) )
  :B3 (fn [x]  (grunge-bass :note (note x)) )
  :Q (fn [x]   (apply (choose [ (fn [] (steel))(fn [] (chain)) (fn [] (hiss3)) ]) []))
  :H (fn [x] (cond
               (= 'c x)(closed-hat)
               (= 'r ) (apply (choose [ (fn [] (closed-hat))(fn [] (open-hat)) (fn [] (hat-demo)) ]) [])
               :else (open-hat)))
  :H2 {:alias :H}
  :V (fn [x] (grainy5 dr1 (rand-int-range 1 10)  (rand)))
  :S (fn [x] (snare :freq 440 :decay 20 ))
  :KS (fn [x] (ks1   :note (note x) :decay 0.1 :coef 0.1))
  })

  (inst-fx!  cs80lead fx-chorus)
  (inst-fx!  cs80lead fx-freeverb)
  ;;  (inst-fx!  cs80lead fx-g-verb)
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

(drum-set-beat  {
                 :H  '[ c r - -   c - c c   - c - -  c - - -]
  ;              :H2  '[ r r r - - - - - - - r r r - -  ]
    ;            :H2  '[ r  ]
  ;               :S  '[ - - - 0 - - - - - - - 0 - - - ]
   ;              :H  '[ c 0 c ]
                 :R2 '[ :g#2  :g#2 :g#2 :a#2 :a#2 :g#2 x]
                 :B  '[ 0 - - - ]
                 :B2  '[ :c2 - - - :e2 - - -]
   ;              :Q '[ 0 - - - - - - - -]
             ;:V '[ 0 - - -]
                                        ;              :V '[ 0 ]
                               :S '[0 - - - - - - -]            
                 
    }
)  


(drum-set-beat  {
                                        ;                :H '[c x c c]
  ;               :H '[c ]
                 :B '[0 - - -]
                 :S '[0 - - - - - - -]                 
                 :B2 '[- :c2 :c2 :c2]
                 :B3 '[- :c3 :c3 :c3 ]
;                 :V '[- :c3 :c3 :c3 ]
                 
   ;              :V '[ - 0 0 0 ]                 
                 
    }
)  


(defn il [num seq]
  "interleave sequences with nops. for tempo experiments."
  (mapcat (fn [x] (concat (list x) (repeat num '-) )) seq))

;; experiment
(metro :bpm 800)
(play-drums-metro metro (metro))
(drum-set-beat  {
                 ;;:H  (il 0 '[c])
                 :KS (il 1 (shift (into [] (map #(note %) '(:c3 :e3 :g3  :c3))) '[ 0 1 2 3  ] -12 ))
                 ;;                 :KS (il 0 '[:c3 :e3 :g3  :c3])                 
                 :B  (il 1 '[O - -  -] )
                 :S  (il 1 '[0 - - - - - - -])
                 :B2 (il 1 '[- :c2 :c2 :c2])
                 :B3 (il 1 '[- :c3 :c3 :c3 ])
                 
    }
)  

(do
  (clear-fx grunge-bass)
  (clear-fx ks1 )
  (clear-fx closed-hat )
  )

(defsynth fx-g-verb
  "can i copy paste program my own reverb?"
  [bus 0 wet-dry 0.5 room-size 0.5 dampening 0.5]
  (let [source (in bus)
        verbed (g-verb source 200 8)]
    (replace-out bus (* 1.4 verbed))))

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
(stop)
  (kill simple-flute)

  ;;some more features id like:
  ;; mute a voice
  ;; somehow allow more patterns for a voice(perhaps with syms like :B:2
  (overtone.helpers.string/split-on-char (name :b:2) ":")  
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
(comment
  (demo (sin-osc))
  (def a (line-reset 400 800 2))
  (ctl a :run 0)
  (ctl a :run 1)
  (ctl a :t-trig 0)
  (ctl a :t-trig 1)
  
  (stop)

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




  
  )
