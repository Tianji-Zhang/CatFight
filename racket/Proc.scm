#lang scheme

;This is an event-based processor (passive)
;Another should be an queryor (active)

;;Data types:
;;id, service_program
;;event = (id . current_serv_program)
;;pool = a list of events
;;req_id_list = a list of sent requests by order
;;info = (bool . current_serv_program) (note: to identify this is whether a new event?)


;;event

(define (generate-event id proc_req)
  (cons id proc_req))

(define (get-id-from-event event)
  (car event))

(define (get-proc-from-event event)
  (cdr event))

;;info

(define (generate-info boolval event)
  (cons boolval event))

(define (new-event? info)
  (car info))

(define (get-event-from-info info)
  (cdr info))

(define (get-proc-from-info info)
  (get-proc-from-event
   (get-event-from-info info)))


;;pool: pure and side-effect operations (change the pool)

;;;No side effect
(define (get-first-event-from-pool pool)
  (if (empty? pool) '()
      (car pool)))

(define (next-event-in-pool pool)
  (cdr pool))

(define (lookup id pool)
  (let ([event (get-first-event-from-pool pool)])
    (cond [(eq? '())
         (generate-info #f '())]
          ;; The new event is created here without defining the processing proc.
        [(eq? id (get-id-from-event event))
         (generate-info #t (get-proc-from-event event))]
        [else (lookup id (next-event-in-pool pool))])))

;;;side effect on pool
(define (extract id pool)
  (let ([event (get-first-event-from-pool pool)])
    (cond [(eq? '() event)
         (generate-info #t "No object")]
          ;; The new event is created here without defining the processing proc.
        [(eq? id (get-id-from-event event))
         (begin
           (set! pool (next-event-in-pool pool))
            (generate-info #f (get-proc-from-event event)))]
        [else (extract id (next-event-in-pool pool))])))

(define (add-id-to-pool id serv_proc pool)
  (if (boolean? serv_proc)
      (string-append "Process end, id: "
                     (number->string id))
      (begin
         (set! pool
               (cons(generate-event id serv_proc)
                    pool))
          (string-append
           "Handled event from id: "
           (number->string id)))))




;;The callback service program
;;It has several steps need to wait

(define serv_req
  (lambda(quit)
    (begin
      (display "Hello meow!")
      (set! quit (call/cc quit))
      (display "Hello again meow!")
      (set! quit (call/cc quit))
      (display "Finally meow, bye!")
      (quit #t))))


;; The program handling the request based on the current condition status in pool
;; Three conditions: 1. a newly-created serv program; 2. a executing serv program; 3. the sev program is normally ended. 

(define (proc_req pool)
  (lambda(req_id)
  (let ([current_info (extract req_id pool)])
    (cond [(new-event? current_info)
           (add-id-to-pool req_id serv_req pool)]
          [else
           (add-id-to-pool
            req_id
            (call/cc (get-proc-from-info current_info))
            pool)]))))

(define (server req_id_list)
  (define pool '())
  (map (proc_req pool) req_id_list))

;; Note: the return file related to proc should be processed in proc.
;;(server '(1 2 3))

;;below are debugging info
(define (proc_req2 pool)
  (lambda(req_id)
  (let ([current_info (extract req_id pool)])
    (cond [(new-event? current_info)
           (add-id-to-pool req_id serv_req pool)]
          [else
           (add-id-to-pool
            req_id
            (call/cc (get-proc-from-info current_info))
            pool)]))))



(define pool '())
;(add-id-to-pool 1 serv_req pool)
;((proc_req2 pool) 1)
;(server '(1 2 1 1 1 1))
;(define return (call/cc (serv_req)))

(define return (call/cc serv_req))
(define meow 12)
(set! meow return)
