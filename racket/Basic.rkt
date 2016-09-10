#lang racket


;;parts: event_handler & object status change loop
;;event structure: id + targetid + info (SAM_range, CIWS_range, hit)

;;info structure: interruptive (true or false) + event


(define (generate-info normal id target info info_string time)
  (list normal id target info info_string time))

(define (snt string number)
  (string-append string (string->number number)))

;;unit

(define (unit id mount loc)
  (define type 'unit)
  ;;Get properties
  (define get
    (lambda(op)
      (cond 
        [(eq? op 'type) unit]
        [(eq? op 'id) id]
        [(eq? op 'loc) loc]
        [else "Error"])))
  ;;Actions to commands (active)
  (define act_mov
    (lambda(v)
      (begin
        (set! loc (- loc v))
        (generate-info #f id 'NULL 'loc_stand "CG is standing by...\n" loc))))
  (define act_fire
    (lambda(n_mount target)
      (begin
        (set! mount (- mount n_mount))
        (generate-info #f id target 'act_fire "CG is firing!\n" loc))))
  (define act
    (lambda(op)
        (cond 
          [(eq? op 'mov) act_mov]
          [(eq? op 'mov) act_fire]
          [else "Error"])))
  ;;Action to situations (passive)
  (define evt_hit_by_weapon
    (lambda (weapon)
      (generate-info #f id (weapon 'id) 'evt_hit_by_weapon "CG is destroyed!\n" loc)))
  (define evt_attacked
    (lambda (weapon)
      (generate-info #t id (weapon 'id) 'attacked "We are attacked by missile!\n" loc)))
  (define evt
    (lambda(op)
      (cond 
        [(eq? op 'hit_by_weapon) evt_hit_by_weapon]
        [else "Error"])))
  ;;Selector
  (define dispatch
    (lambda(op)
      (cond 
        [(eq? op 'get) get]
        [(eq? op 'act) act]
        [(eq? op 'evt) evt]
        [else "Error"])))
  dispatch)


;; 1D range (location func)

(define (range x y) (abs (- x y)))

;;weapon

(define (weapon id number target loc)
  (define type 'weapon)
  ;;Get properties
  (define get
    (lambda(op)
      (cond 
        [(eq? op 'num) number]
        [(eq? op 'id) id]
        [(eq? op 'target) target]
        [(eq? op 'loc) loc]
        [else "Error"])))
  ;;Actions to commands (active)
  (define act_mov
    (lambda(v)
      (begin
        (set! loc (- loc v))
        (cond [(<= (range loc ((target 'get) 'loc)) 1)
               (generate-info #t id ((target 'get) 'id) 'loc_c "closing in...\n" loc)]
              [(<= (range loc ((target 'get) 'loc)) 3)
               (generate-info #t id ((target 'get) 'id) 'loc_ciws "CIWS range to target!\n" loc)]
              [(<= (range loc ((target 'get) 'loc)) 10)
               (generate-info #t id ((target 'get) 'id) 'loc_sam "SAM range to target!\n" loc)]
              [else 
               (generate-info #f id ((target 'get) 'id) 'loc_mov "moving toward target...\n" loc)]))))
  (define act
    (lambda(op)
      (cond 
        [(eq? op 'mov) act_mov]
        [else "Error"])))
  ;;Reactions to situations & events (passive)
  (define evt_intercepted
    (lambda (intercept_weapon)
      (if (> (intercept_weapon 'num) (weapon 'num))
          (begin
            (set! number 0)
            (generate-info #f id (intercept_weapon 'id) 'evt_inter_done "All weapons are intercepted...\n" loc))
          (begin 
            (set! number (- (intercept_weapon 'num) (weapon 'num)))
            (generate-info #f id (intercept_weapon 'id) 'evt_inter "Some weapons are intercepted, moving toward target...\n" loc)))))
  (define evt_hit
    (generate-info #f id (target 'id) 'evt_hit "Target destroyed\n" loc))
  (define evt
    (lambda(op)
      (cond 
        [(eq? op 'intercepted) evt_intercepted]
        [(eq? op 'hit) evt_hit]
        [else "Error"])))
  ;;Selector
  (define dispatch
    (lambda(op)
      (cond 
        [(eq? op 'get) get]
        [(eq? op 'act) act]
        [(eq? op 'evt) evt]
        [else "Error"])))
  dispatch)

;; The pool to save units

(define (pool unit_list)
  (define add_unit
    (lambda(new_unit)
      (begin
        (set! unit_list (append unit_list new_unit))
        unit_list)))
    (define rm_unit
      (lambda(destroyed_unit)
        (begin
          (set! unit_list (remove unit_list destroyed_unit))
          unit_list)))
  ;;Selector
  (define dispatch
    (lambda(op)
      (cond 
        [(eq? op 'rm) rm_unit]
        [(eq? op 'add) add_unit]
        [else "Error"])))
  dispatch)

;; Test units
(define CG (unit 1 2))
(define ASM (weapon 1 11 CG 50))
;(define SAM (weapon 2 11 ASM 50))

;; Main Game
;(define (main_timeloop)
;  (define battle_units (pool (list CG ASM)))
;  (let loop ((n 50))
;    (display
;     (let ([info (((ASM 'act) 'mov) 1)])
;       (if (car info)
;           (car (cddddr info)) 
;           "")))
;    (if (> n 0) 
;        (loop (- n 1))
;        "Scenario end")))

(define (parse-info1 info)
  (car info));;Emergency
(define (parse-info5 info)
  (car (cddddr info)));;Info
(define (parse-info4 info)
  (car (cdddr info)));;Info tag
(define (parse-info3 info)
  (caddr));Target

(define (main_timeloop)
  (define battle_units (pool (list CG ASM)))
  (let loop ((n 50))
    (display
     (for-each 
      (lambda(unit)
        (if)) 
      battle_units))
    (if (> n 0) 
        (loop (- n 1))
        "Scenario end")))

(display (main_timeloop))

;;Note: pay attention to the domain of nested function, it should be unable to be evaluated!
;loadlouts:
;
;1.warhead
;	weapon,sensor,fuel,propulsion,communicator
;	loadout,mount
;2.units: aircraft,ship,submarine,satellite
;3. Facility
;
;Action:
;
;electromag_field = electro(units)
;discovered_identified = unit.Recon()
;location = unit.Move()
;mount_loadout = unit.fire()
;mount_loadout = facility.supply()
;1.move:weapon_warhead,target_obj
;touched,missed
;damage
;2.Recon:sensor_obj,signal_target
;found, lost
;identity,location,moving direction
;3.Fire:sensor_weapon_unit
;can fire, cannot(signal,location,readiness)
;4.prepare:unit, facility,mount
;supply, repair,prepare