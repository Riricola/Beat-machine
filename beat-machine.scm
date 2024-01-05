;; CSC-151-NN (TERM)
;; Mini-Project 3: Beat Machine
;; Maria Rodriguez
;; 2022-09-21
;; ACKNOWLEDGEMENTS:
;;   ....

(import music)

;------------------------------------------------
;;; PART ONE                                    |
;------------------------------------------------

;;; (accent midi-note dur) -> composition?
;;; midi-note -> integer?
;;; dur -> dur?
;;; accent takes the midi-note and dur to create a note. After creating the note,
;;; accent modifies it to make it louder than the regular 64
(define accent
    (lambda (midi-note dur)
        (let ((note (note midi-note dur)))
        (mod (dynamics 105) note))))
(accent 38 qn)

;;; (ghost midi-note dur) -> composition?
;;; midi-note -> integer?
;;; dur -> dur?
;;; ghost takes the midi-note and dur to create a note. After creating the note,
;;; ghost modifies it to make it quieter than the regular 64
(define ghost
    (lambda (midi-note dur)
        (let ((note (note midi-note dur)))
        (mod (dynamics 20) note))))
(ghost 38 qn)

;;; (snare-bar) -> composition?
;;; snare-bar creates one bar of 4 quarter notes
;;; one accented, followed by two ghosts, followed by one normal note
(define snare-bar
    (seq (accent 38 qn)(ghost 38 qn)(ghost 38 qn)(note 38 qn)))

;;; (snare-sequence) -> composition?
;;; snare-sequence takes the two snare-bars and puts them together as a sequence
(define snare-sequence
    (seq snare-bar snare-bar))

;;; (strokes) -> composition
;;; strokes has the two bars and mods them into percussion notes
(define strokes
    (mod percussion snare-sequence))

;;; (tremolo slashes midi-note duration) -> composition?
;;; slashes -> integer , 0 <= x <= 3
;;; midi-note -> integer
;;; duration -> dur?
;;; tremolo takes the midi-note and creates notes that play for the duration of dur cut by the amount of slashes given
;;; to cut the duration in half, denomintor turns it into an integer and multiplies it by the product of the amount of slashes and 2
;;; the dur function creates a duration from numerator 1 and the newly calculated denominator 
;;; then it repeates this note for the amount of slashes * 2 (because we are cutting the note in half for every slash)
(define tremolo 
    (lambda (slashes midi-note duration)
        (repeat (expt 2 slashes )(note midi-note (dur 1 (* (denominator duration)(expt 2 slashes )))))))

(tremolo 2 60 hn)
(tremolo 2 60 qn)
(tremolo 3 60 hn)
    
;;;(roll midi-note duration) ->composition?
;;; midi-note -> integer?
;;; duration -> dur?
;;; roll takes the duration and divides it into 4 equally-spaced notes of the given midi-value
(define roll
    (lambda (midi-note duration)
        (repeat 4 (note midi-note (dur 1 (/ (denominator duration) 4))))))
(roll 60 hn)

;;;(flam midi-note duration) -> composition
;;; midi-note -> integer?
;;; duration -> dur?
;;;creates a grace note of half the duration played before the given note. The given note is also accented.
(define flam
    (lambda (midi-note duration)
        (let ((accent-note (accent midi-note duration)))
            (pickup (note midi-note (dur 1 (* (denominator duration) 2))) accent-note))))
(flam 60 hn)

;;;(single-drag-tap midi-note) -> composition?
;;; midi-note -> integer?
;;;creates a pair of sixteenth note grace notes, followed by a regular eighth note, and then an accented eighth note.
(define single-drag-tap
    (lambda (midi-note)
        (pickup (seq (note midi-note sn)(note midi-note sn))(seq (note midi-note en)(accent midi-note en)))))
(single-drag-tap 60)

;------------------------------------------------
;;; PART TWO                                    |
;------------------------------------------------

;;; this is the horizontal decomposition of the simple rock beat
(define horizontal-simple-rock-beat
    (mod percussion (par (seq (note 67 qn)(note 67 qn)(note 67 qn)(note 67 qn))
        (seq (note 65 qn)(rest qn)(note 65 qn)(rest qn))
        (seq (rest qn)(note 60 qn)(rest qn)(note 60 qn)))))
horizontal-simple-rock-beat

;;; this is the vertical decomposition of the simple rock beat
(define vertical-simple-rock-beat
    (mod percussion (seq (par (note 35 qn) (note 42 qn))
        (par (note 38 qn)(note 42 qn))
        (par (note 35 qn) (note 42 qn))
        (par (note 38 qn)(note 42 qn)))))
vertical-simple-rock-beat

;;; (beat-machine snare-sequence n1 hi-hat-sequence n2 bass-drum-sequence n3) ->composition?
;;; (beat-machine) takes the horizontal decomposition approach
;;; I chose horizontal decomposition because it would be much easier and less redundant
;;; to have to take three patterns, the snares, hi-hats, and bass drums, and put them together
;;; rather than having to specify what is being played on EACH pulse
(define beat-machine
    (lambda (snare-sequence n1 hi-hat-sequence n2 bass-drum-sequence n3)
        (mod percussion (par
                (repeat n1 snare-sequence)
                (repeat n2 hi-hat-sequence)
                (repeat n3 bass-drum-sequence)))))

;;; this is the snare of simple rock 
(define simple-snare
    (seq (rest qn)(note 38 qn)))
;;; this is the hi hat of simple rock
(define simple-hi-hat
    (seq (note 42 qn)(note 42 qn)))
;;; this is the bass of simple rock
(define simple-bass
    (seq (note 35 qn)(rest qn)))
;;; this is simple rock as defined by my beat machine
(define simple-rock-beat
    (beat-machine simple-snare 2 simple-hi-hat 2 simple-bass 2))
simple-rock-beat

;;; this is the snare of elaborate rock
(define elaborate-snare
    (seq (ghost 38 en)(ghost 38 en)(accent 38 en)(ghost 38 en)(ghost 38 en)(accent 38 en)(ghost 38 en)(ghost 38 en)))
;;; this is the hi hat of elaborate rock
(define elaborate-hi-hat
    (seq (note 42 en)(note 42 en)))
;;; this is the bass drum of elaborate rock
(define elaborate-bass
    (seq (note 35 en)(note 35 en)(rest en)(note 35 en)(rest en)(rest en)(rest en)(note 35 en)))
;;; this is elaborate rock as defined with my beat machine
(define elaborate-rock-beat
    (beat-machine elaborate-snare 1 elaborate-hi-hat 4 elaborate-bass 1))
elaborate-rock-beat

;------------------------------------------------
; PART THREE                                    |
;------------------------------------------------

;;; this is the snare of latin beat
(define latin-snare
    (seq(rest en)(rest en)(note 38 en)(rest en)(rest en)(note 38 en)(rest en)(rest en)))
;;; this is the hi-hat of latin beat
(define latin-hi-hat
    (seq(note 42 en)(note 42 en)))
;;; this is the bass of latin beat
(define latin-bass
    (seq(note 35 en)(rest en)(rest en)(note 35 en)))
;;; this is latin beat as defined by my beat machine
(define latin-beat
    (beat-machine latin-snare 1 latin-hi-hat 4 latin-bass 2))
latin-beat

;;; this is the snare of swing beat
(define swing-snare
    (seq (rest qn)(note 38 qn)))
;;; this is the hi hat of swing beat
(define swing-hi-hat
    (seq(note 42 qn)(note 42 (dur 1 (* (denominator qn) 3)))(rest (dur 1 (* (denominator qn) 3)))(note 42 (dur 1 (* (denominator qn) 3)))(note 42 qn)))
;;; this is the bass of swing beat
(define swing-bass
    (seq(note 35 qn)))
;;; this is swing beat as defined by my beat machine
(define swing-beat
    (beat-machine swing-snare 2 swing-hi-hat 1 swing-bass 4))
swing-beat

;;; this is the snare of funk beat
(define funk-snare
    (seq(rest qn)(accent 38 sn)(rest en)(ghost 38 sn)(rest sn)(ghost 38 sn)(ghost 38 sn)(rest sn)(accent 38 sn)(rest en)(ghost 38 sn)))
;;; this is the hi hat of funk beat
(define funk-hi-hat
    (seq(note 42 en)(note 42 en)))
;;; this is the bass of funk beat
(define funk-bass
    (seq (note 35 en)(note 35 en)(rest qn)(rest qn)(note 35 sn)(rest en)(note 35 sn)))
;;; this is funk beat as defined by my beat machine
(define funk-beat
    (beat-machine funk-snare 1 funk-hi-hat 4 funk-bass 1))
funk-beat

;;; this is the snare of garba beat
(define garba-snare
    (seq (rest en)(note 38 en)(rest en)(note 38 en)(rest en)(rest tn)(rest tn)
    (rest tn)(note 38 tn)(rest tn)(rest tn)(note 38 tn)(rest tn)(rest qn)(note 38 qn)))
;;; this is the hi hat of garba beat
(define garba-hi-hat
    (seq (rest qn)(rest qn)(rest qn)(note 42 (dur (* 2 1) (* 8 3)))(note 42 (dur (* 2 1) (* 8 3)))(note 42 (dur (* 2 1) (* 8 3)))))
;;; this is the bass of garba beat
(define garba-bass
    (seq (note 35 en)(rest en)(note 35 en)(rest en)(note 35 en)))
;;; this is garba as defined by my beat machine
(define garba-beat
    (beat-machine garba-snare 1 garba-hi-hat 1 garba-bass 1))
garba-beat

;;; here is the snare of my beat
(define my-snare
    (seq(rest qn)(ghost 38 en)(accent 38 en)))
;;; here is the hi hat of my beat
(define my-hi-hat
    (seq(note 42 en)(rest en)(accent 42 en)(ghost 42 en)))
;;; here is the bass of my beat
(define my-bass
    (seq(note 35 sn)(rest sn)(note 35 sn)(accent 35 sn)))
;;; here is my beat
(define my-beat
    (beat-machine my-snare 2 my-hi-hat 2 my-bass 4))
my-beat