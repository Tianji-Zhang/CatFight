#lang scheme


; All cmd return two things:
;; 1. whether the cmd is successful,
;; 2. If successful, acquire the info and side effect
;; 3. If not, acquire the info

; The commands are executed in the below way:
;; 1. change the inner status of units
;; 2. Create change to the environment

; Events
;; 1. Events are interactions between units, including weapons
;; 2. Events are passive, processed by the game than inside units.



(define (unit mount)
  (define (create-weapon-unit target n_mount)
    (weapon target mount))
  (define (shoot target n_mount)
    (if (> mount n_mount)
        (begin
          (set! mount (- mount n_mount))
          (list #t
                (string-append (number->string n)
                               " weapons are deployed")
                (create-weapon-unit target n_mount)))
        (#f . "Insufficient weapon mount")))
  (define fire
    (lambda(target n_shoot) (shoot target n_shoot)))
  (define cmd_exec
    (lambda(info)
      (cond ((eq? 'fire) fire)
            (else (error "Invalid operation" info)))))
  cmd_exec)


(define (system n)
  (define me (unit 10))
  (define enemy (unit 10))
  (begin
    ((me 'attack) enemy n
         )
    "Game Over"))

(system 3)
(system 10)
(system 11)