(ns sylt.inst
  (:require [overtone.api])
  (:use
   [overtone.live] ;; for the internal sc server
   ;;   [overtone.core]
   )
  )


;;just a workaround because i didnt have stuff cached

;; #+BEGIN_SRC clojure
(defn frp [x]

                                        ;  "/home/joakim/.config/google-chrome-unstable/Default/Extensions/bepbmhgboaologfdajaanbcjmnhjmhfn/0.1.1.5023_0/audio/1_short_Open_16_16.wav"
  (freesound-path x)
  )
;; #+END_SRC


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


;; * First, we'll define some percussive instruments

;; this high hat instrument takes a white noise generator and shapes
;; it with a percussive envelope

;; #+BEGIN_SRC clojure
(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 1 src env)))

(comment
  (hat)
  )
;; #+END_SRC

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
(definst dream [bufnum 0]
  (play-buf :num-channels 1 :bufnum bufnum :rate 0.8)
  ;;(send dreamidx inc )
  ;;(if (< 10 @dreamidx) (send dreamidx inc ) (  ))

  )


(def choir (sample (frp 46712)))
(def choir2 (sample (frp 65801)))
(def choir3 (load-sample (frp 65801)))
(def choir4 (load-sample (frp 46712)))
(definst choir2s []
  (* 64 (play-buf :num-channels 1 :bufnum choir3 :rate 1)))

(definst choir4s []
  (* 64 (play-buf :num-channels 2 :bufnum choir4 :rate 1)))

;; * some dubstep varation
;; mostly copied

;; and combining ideas from sounds.clj with the rhythm ideas here:

;; first we bring back the dubstep inst
;; #+BEGIN_SRC clojure
(definst dubstep [freq 100 wobble-freq 5]
  (let [sweep (lin-exp (lf-saw wobble-freq) -1 1 40 5000)
        son   (mix (saw (* freq [0.99 1 1.01])))]
    (lpf son sweep)))
;; #+END_SRC


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

(def glasscrashsample (load-sample (frp 221528)))
(definst glasscrash [vol 1]
  (* vol (play-buf :num-channels 2 :bufnum glasscrashsample :rate 2)))


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



;;;; new insts 20170112
(definst radio [ freq  440 amp  0.5 gate  1]
  (let 
      [oscfreq  (repeat 3 (* freq (lin-lin (lf-noise2:kr 0.5) 0.98 1.02))) ; !3, seems to mean repeat 3 times? weird
       snd      (splay (* (lf-saw oscfreq ) amp))
       env      (env-gen (adsr 0.7 4 0 0.1) :action FREE)
       output   (lpf snd (+ (* env freq) (* 2 freq)))
       ] [output output]))



;;http://sccode.org/1-523
(definst electro-kick [out  0 pan  0 amp  0.3]
  (let
      [bodyFreq (env-gen (envelope [261 120 51] [0.035 0.08] :exp))
       bodyAmp (env-gen (lin 0.005 0.1 0.3 ):action FREE)
       body (* (sin-osc bodyFreq)  bodyAmp)

       popFreq (x-line:kr 750, 261, 0.02)
       popAmp (* (env-gen (lin 0.001 0.02 0.001))  0.15 )
       pop  (* (sin-osc popFreq) popAmp)

       clickAmp (* ( env-gen (perc 0.001 0.01)) 0.15)
       click    (* (lpf (formant 910 4760 2110) 3140)  clickAmp)

       snd (tanh (+  body pop click))
       ]
    [snd snd]))


(definst electro-hat
  [out  0, pan 0, amp  0.3]

  ;;// noise -> resonance -> expodec envelope
  (let
      [noiseAmp  (env-gen (perc 0.001, 0.3, :curve -8) :action FREE)
       noise (* (mix (bpf  (clip-noise)   [4010, 4151]
                           [0.15, 0.56]
                           ;;[1.0, 0.6] ;;why doeesnt this work?
                           ))
                0.7  noiseAmp)

       snd  noise]
    [snd snd]))


(definst electro-clap [out  0 amp  0.5 pan  0 dur  1]
  (let
      [env1   (env-gen   (envelope
                          [0, 1, 0, 0.9, 0, 0.7, 0, 0.5, 0],
                          [0.001, 0.009, 0, 0.008, 0, 0.01, 0, 0.03],
                          [0, -3, 0, -3, 0, -3, 0, -4]) :action FREE)
       noise1 (bpf (lpf (hpf (*  env1 (white-noise)) 600) (x-line 7200, 4000, 0.03)) 1620 3)

       env2 ( env-gen (envelope [0, 1, 0], [0.02, 0.18], [0, -4]))
       noise2  (bpf (lpf (hpf (*  env2 (white-noise)) 1000) 7600) 1230 0.7 ;0.7
                    )
       ]
    (softclip (* 2 (+ noise1 noise2)))
    ))

;;questions:
;; - bpf last arg
;; - get after a while: FAILURE /s_new too many nodes, so im missing some reclamation

;; some other basses

(definst tstbass [atk 0.01, dur 0.15, freq 50, amp 0.8]

  (* (bpf (lf-saw freq), freq, 2)  (env-gen(perc atk, dur, amp, 6 )))
  )



(definst bazz[ dur 0.15, freq 50, amp 0.8, index 10 ]
  (* (pm-osc freq, (+ freq  5), index) (env-gen(triangle dur ))))


(definst basicfm [out  0, gate  1, amp  1, carFreq  1000, modFreq  100, modAmount  2000, clipAmount  0.1]
  (let [modEnv (env-gen (adsr 0.5 0.5 0.7 0.1 :peak-level modAmount) gate)
        mod (* (sin-osc modFreq) modEnv)
        car (+ mod (sin-osc carFreq))
        ampEnv (env-gen (adsr 0.1, 0.3, 0.7, 0.2, :peak-level amp) gate )
        clipv (* clipAmount 500)
                                        ;snd (clip (* car ampEnv clipv) -0.7 0.7)
        snd (* 0.1 (clip:ar (* car ampEnv clipv) -0.7 0.7))
        ]
    [snd snd]
    ))




;; SynthDef(\vocali, { arg f1, f2, fund = 70, amp = 0.25 ;
;; 		var source = Saw.ar(fund);
;; 		var vowel = Normalizer.ar(BPF.ar(source, f1, 0.1))
;; 		+
;; 		Normalizer.ar(BPF.ar(source, f2, 0.1))
;; 		* amp ;
;; 		Out.ar(0, vowel.dup)
;; 	}).add ;
;; \i:[2300, 300], \e: [2150, 440], \E: [1830, 580],
;; 		\a: [1620, 780], \O: [900, 580], \o: [730, 440],
;; 		\u: [780, 290],\y: [1750, 300],\oe: [1600, 440],
;; 		\OE: [1400, 580]



(def vocali-x {:i [2300, 300], :e [2150, 440], :E [1830, 580],
               :a [1620, 780], :O [900, 580], :o [730, 440],
               :u [780, 290], :y [1750, 300],:oe [1600, 440],
               :OE [1400, 580]})





(definst vocali [f1 2300 f2 300   fund 70 amp 0.25]
  (let [
        src (saw fund)
        env (env-gen (perc 0.01 0.5) :action FREE)
        vowel (* env amp (+ (normalizer (bpf src f1 0.09))
                            (normalizer (bpf src f2 0.09))))
        ]
    [vowel vowel])
  
  )

(defn vocali2 [x fund amp]
  (let [[f1 f2] (get  vocali-x x)
        ]
    (vocali f1 f2 fund amp)
    )
  )
;;(vocali2 :i 70 0.25)  



;;sounds pleasant with echo
(definst my-formant [fund 100]
  (let
      [
       src1 (formant fund (x-line 2000 400 ) 200)
       src2 (formant fund (x-line 2000 400 ) 400)
       env (env-gen (perc 0.01 0.8) :action FREE)
       ]
    [ (* env src1) (* env src2) ]))


(definst myformlet [freq 50 phase 0.5]
  (let
      [        env (env-gen (perc 0.01 0.5) :action FREE)
       src (impulse:ar freq, phase ) 
       src (formlet  src 300 0.01 0.1)
       src (* env src)
       ]
    [src src]
    ))



;; SynthDef(\risset, {|out = 0, pan = 0, freq = 400, amp = 0.1, dur = 2, gate = 1|
;; 		var amps = #[1, 0.67, 1, 1.8, 2.67, 1.67, 1.46, 1.33, 1.33, 1, 1.33];
;; 		var durs = #[1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075];
;; 		var frqs = #[0.56, 0.56, 0.92, 0.92, 1.19, 1.7, 2, 2.74, 3, 3.76, 4.07];
;; 		var dets = #[0, 1, 0, 1.7, 0, 0, 0, 0, 0, 0, 0];
;; 		var doneActionEnv = EnvGen.ar(Env.linen(0, dur, 0), gate, doneAction: 2);
;; 		var src = Mix.fill(11, {|i|
;; 			var env = EnvGen.ar(Env.perc(0.005, dur*durs[i], amps[i], -4.5), gate);
;; 			SinOsc.ar(freq*frqs[i] + dets[i], 0, amp*env);
;; 		});
;; 		src = src * doneActionEnv * 0.5; // make sure it releases node after the end.
;; 		Out.ar(out, Pan2.ar(src, pan));
;; 	}).add;

(definst risset [out  0, pan  0, freq  400, amp  0.1, dur  2, gate  1]
  (let
      [
       amps  [1, 0.67, 1, 1.8, 2.67, 1.67, 1.46, 1.33, 1.33, 1, 1.33]
       durs  [1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075]
       frqs  [0.56, 0.56, 0.92, 0.92, 1.19, 1.7, 2, 2.74, 3, 3.76, 4.07]
       dets  [0, 1, 0, 1.7, 0, 0, 0, 0, 0, 0, 0]
       doneActionEnv  (env-gen(lin 0, dur, 0), gate, :action FREE)


       src  (mix (map (fn [ampi duri frqsi detsi]
                        (* amp (env-gen(perc 0.005, (* dur duri), ampi, -4.5), gate)
                           (sin-osc (+ (* freq frqsi)  detsi, 0, (* amp 0.1))))) ;;0.1 should be amp
                      amps durs frqs dets))
       src (* doneActionEnv src 0.5)
       ]
    [src src]
    ))
;;sounds nice with some echo and chorus
;;(risset :freq 100 :amp 0.5)(risset :freq 800 :amp 0.1)

;;;;;;;       


;; i want to make a string sound
;;https://www.attackmagazine.com/technique/tutorials/analogue-style-string-synthesis/2/
;; my attempt sounds nothing like theirs :( but it sounds nice anyway
(definst mystr [freq 440]
  (let
      [fenv   (env-gen (perc 0.5 5) :action FREE)
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


;;now i want a rim shot
;; https://www.freesound.org/people/Sajmund/sounds/132418/
;; again my rimshot sounds nothing like i want it to
(definst rimshot
  [;freq   100
   amp    0.3
   decay  0.1]
  (let [env  (env-gen (adsr-ng :attack 0 :attack-level 1 :decay 0.1 :level 0.01 :sustain 0.001 :release 2 ) :action FREE)
        fenv (lin-lin env 1 0 1800 1);(* freq 1.5) (/ freq 1.5))
        snd0 (white-noise);(white-noise:ar)
        snd (lpf snd0 fenv)
        snd1 (* 0.1 (bpf snd0 300 4))
        snd2 (* 0.1 (bpf snd0 1000 4))
        snd3 (* 0.1 (bpf snd0 1800 4))

        ]
    (* (+ snd
          snd1
          snd2
          snd3
          ) env amp)))
;;(rimshot)
;;(snare)

;; now i want to make a sound like a percussive bass, with a certain freq env
(definst tsts [freq 110 amp 1]
  (let [
        env  (env-gen (perc 0.01 0.5)  :action FREE)
        fenv (env-gen (lin 0.5 0.5 0.5 0.5))
        src  (pulse:ar freq fenv)
        src2  (formlet src (/ freq 2) 0.01 0.05)
        src (+ (* 0.5 src2) src)
        src  (* env src amp)

        ]
    [src src]
    ))

(definst noise-flute [freq 880
                      amp 0.5
                      attack 0.4
                      decay 0.5
                      sustain 0.8
                      release 1
                      gate 1
                      out 0
                      rq 0.1]
  (let [env  (env-gen (adsr attack decay sustain release) gate :action FREE)
        sig (white-noise)
        sig (resonz sig freq rq)
        sig  (* env sig)
        sig2 (sin-osc freq)
        sig (+ sig (* 0.1 sig2))
        ]

    sig))


;;trying to get rid of the click in tb303
;;so far i moved the vol-env to the end rather than the middle where it was
(definst mytb303
  [note       {:default 60}
   wave       {:default 1}
   r          {:default 0.8}
   attack     {:default 0.01}
   decay      {:default 0.1}
   sustain    {:default 0.6}
   release    {:default 0.01}
   cutoff     {:default 100}
   env-amount {:default 0.01}
   amp        {:default 0.5}]
  (let [freq       (midicps note)
        freqs      [freq (* 1.02 freq)]
        vol-env    (env-gen (adsr attack decay sustain release)
                            (line:kr 1 0 (+ attack decay release))
                            :action FREE)
        fil-env    (env-gen (perc))
        fil-cutoff (+ cutoff (* env-amount fil-env))
        waves      (* 1
                      [(saw freqs)
                       (pulse freqs 0.5)
                       (lf-tri freqs)])
        selector   (select wave waves)
        filt       (* vol-env (rlpf selector fil-cutoff r))]
    (* amp filt)))

(definst el-snap []
  	;; snap = 0!2;
	;; snap[0] = EnvGen.ar(Env.new([0,1,0], [0, 0.5]), trig);
	;; snap[0] = snap[0] * snap[0] * snap[0] * WhiteNoise.ar * 0.5;
	;; snap[1] = EnvGen.ar(Env.new([0,1,0], [0, 10/1000]), trig);
	;; snap[1] = SinOsc.ar(snap[1] * 7000 + 20);
  ;; snap = snap.sum * 0.05;
  (let [
      snap0 (env-gen (envelope [0 1 0] [0 0.5] :exp))
      snap0 (* snap0  snap0  snap0  (white-noise) 0.5)
      snap1 (env-gen (envelope [0 1 0] [0 (/ 10 1000)] :exp))
      snap1 (sin-osc (+ (* snap1 7000) 20))
        snap (+(+ snap0 snap1 ) 0.05)
        ]
      snap
      )

)


(definst asawbass [freq 440 amp 0.5 gate 1]
  (let
      [osfreq  [ (* freq 0.998) (* freq 0.999) (* freq 1) (* freq 1.001)] ;;(lin-lin :in (lf-noise2:kr 0.5) :dstlo 0.98 :dsthi 1.02 )
       snd (splay (* amp (lf-saw osfreq)))
       env (env-gen (adsr 1 2 0 0.5) gate  :action FREE )
       ;;output (lpf snd (+ (* env freq) (* 4 freq)))
       output snd
       output (* output env)
       ]
    output))
                  
;;not too bad like this:
;;(asawbass 110)(asawbass 220)
;; and some reverbs and stuffs

;; now for  sync-saw experiments
;;(demo [(sync-saw 50 (* 50 (line 1 1.5 1)) ) (sync-saw 51 (* 51 (line 1 1.5 1)) )])

(definst asyncsaw
  [freq 50 amp 0.5 gate 1]
  (let
      [env (env-gen (adsr 1 2 0 0.5) gate  :action FREE )
       syncenv (line 1 1.5 1)
       saw     (map #(sync-saw (*  (+ freq %)) (*  syncenv (+ % freq))) [0 1 2 3 4 5 6 ])
    ]
  (* env (splay saw))))

;; some piano would be nice
;;(demo (mda-piano 880 ))

;; and sax
;;(demo (stk-saxofony 110 40 20 10 ))



  
  

(defsynth fx-g-verb
  "can i copy paste program my own reverb?"
  [bus 0 wet-dry 0.5 room-size 0.5 dampening 0.5]
  (let [source (in bus)
        verbed (g-verb source 200 8)]
    (replace-out bus (* 1.4 verbed))))

(defsynth fx-echo2
  [bus 0 max-delay 1.0 delay-time 0.4 decay-time 2.0 echo-level 1.0]
  (let [source (in bus)
        echo (comb-n source max-delay delay-time decay-time)]
    (replace-out bus (pan2 (+ (* echo-level echo) source) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; i want a nicer sample player interface


;; cant macroize definst for some reawson?
;; (defmacro sample-player-inst-maker [inst-name]
;;   `(definst ~inst-name [bufnum 0 rate 1.0]
;;      (play-buf :rate rate :num-channels 1 :bufnum bufnum)))


;; (sample-player-inst-maker sample-player-inst)

;; (definst sample-player-inst [bufnum 0 rate 1.0]
;;   (play-buf :rate rate :num-channels 1 :bufnum bufnum))

(defn create-sample-player [path sample-rate amp sample-player-inst]
  (let [samples (load-samples path)
        next-idx (atom 0)
        play-sample (fn [samples n rate amp]
                      (sample-player-inst (nth samples n) :rate rate :amp amp))]
    {:play 
     (fn [] (play-sample samples @next-idx sample-rate amp))
     :play-n 
     (fn [n] (play-sample samples n sample-rate amp))
     :set-index
     (fn [n] (reset! next-idx 0))
     :play-next 
     (fn []
       (play-sample
        samples
        @next-idx
        sample-rate
        amp)
       (swap! next-idx #(if (= (dec (count samples)) %) 0 (inc %)))
       )}))
