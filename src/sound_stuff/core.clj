(ns sound-stuff.core
  (:use [overtone.live]
        [overtone.inst.piano]))

(stop)


(* 0.8
       (env-gen (perc 0.05 1.8) 1 1 0 1 :action FREE)
       (+ (sin-osc (/ 440 2))
          (rlpf (sin-osc 440) (* 1.1 440) 0.4)))


(piano)
(piano (note :C5))

(note :C3)

(piano (note (chord :C4 :minor)))

(piano 50 :decay 0 :release 0 :sustain 0) 

(doseq [n (chord :C3 :major7)] (piano n))

(definst steel-drum [note 60 amp 0.8]
  (let [freq (midicps note)]
    (* amp
       (env-gen (perc 0.05 1.8) 1 1 0 1 :action FREE)
       (+ (sin-osc (/ freq 2))
          (rlpf (sin-osc freq) (* 1.1 freq) 0.4)))))

(steel-drum (note :C5))
(map steel-drum (map note (chord :c5 :minor)))

;; model a plucked string. this is really cool!
(definst plucked-string [note 60 amp 0.3 dur 1.3 decay 60 coef 0.3 gate 1]
  (let [freq   (midicps note)
        noize  (* 0.8 (white-noise))
        dly    (/ 0.3 freq)
        plk    (pluck noize gate dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action FREE) reverb)))

(map plucked-string (map note (chord :c4 :major)))

(* 1.0 (env-gen (perc 0.01 10)) (sin-osc 440))

(let [time (now)]
  (at time (plucked-string (note :c1)))
  (at (+ 1000 time) (plucked-string (note :c2)))
  (at (+ 2000 time) (plucked-string (note :a1)))
  (at (+ 3000 time) (plucked-string (note :e1)))
  )

(definst bell [frequency 440 duration 10
               h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonic-series [ 1  2  3  3.8  4.6  5.4]
                                        ;[ 1  2  3  4.2 5.4 6.8]
        proportions     [h0 h1 h2 h3 h4 h5]
        component
        (fn [harmonic proportion]
          (* 1/2
             proportion
             (env-gen (perc 0.01 (* proportion duration)))
             (sin-osc (* harmonic frequency))))
        whole
        (mix (map component harmonic-series proportions))]
    (detect-silence whole :action FREE)
    whole))

(bell)

(defn play-chord [the-chord]
  '(map steel-drum (map note the-chord)))

(let [time (now)]
  (at time (piano (note :c3)))
  (at (+ 1000 time) (piano (note :c4)))
  (at (+ 2000 time) (piano (note :a3)))
  (at (+ 3000 time) (piano (note :e3)))
  )


(defn scale-degrees [] (shuffle [:i :ii :iii :iv :v :vi :vii ]))
(defn high-pitches [] (degrees->pitches (scale-degrees) :pentatonic :c2))
(defn low-pitches [] (degrees->pitches (scale-degrees) :pentatonic :c1))

(defn play [instrument time notes sep]
  (let [note (first notes)]
    (when note
      (at time (instrument note)))
    (let [next-time (+ time sep)]
      (apply-at next-time play instrument [next-time (rest notes) sep]))))

(let [t (+ 300 (now))]
  (play plucked-string t (flatten (repeatedly 20 high-pitches)) 150)
  (play plucked-string t (flatten (repeatedly 10 low-pitches)) 300)
  )


; define a metronome at a given tempo, expressed in beats per minute.
(def metro (metronome 140))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst o-hat [amp 0.7 t 0.5]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(defn swinger [beat]
  (at (metro beat) (o-hat))
  (at (metro (inc beat)) (c-hat))
  (at (metro (+ 1.68 beat)) (c-hat))
  (apply-at (metro (+ 2 beat)) #'swinger (+ 2 beat) []))

(metro)
(metro 7390)



(swinger (metro))

(stop)

(piano 36)

(defn rolling-bass-blues [base-note] (degrees->pitches [:i :iii :v :vi :vi# :vi :v :iii] :major base-note))
(defn half-rolling-bass-blues [base-note] (degrees->pitches [:i :iii :v :vi] :major base-note))

(def bass-pitches
  (concat
   (rolling-bass-blues :c2)
   (rolling-bass-blues :c2)
   (rolling-bass-blues :g2)
   (rolling-bass-blues :c2)
   (half-rolling-bass-blues :a2)
   (half-rolling-bass-blues :g2)
   (rolling-bass-blues :c2)))

(defn play-on-beat [beat pitches]
  (println (str "play-on-beat " beat))
  (doseq [[i p] (map-indexed vector pitches)]
    (println (str i " " p " @ " (+ i beat)))
    (at (metro (+ i beat)) (piano p :decay 0.0001))))

(defn repeat-on-beat [beat pitches]
  (println (str "repeat-on-beat " beat))
  (play-on-beat beat pitches)
  (apply-at (metro (+ (count pitches) beat)) #(repeat-on-beat  (+ (count pitches) beat) pitches)))

(play-on-beat (metro) bass-pitches)
(repeat-on-beat (metro) bass-pitches)
