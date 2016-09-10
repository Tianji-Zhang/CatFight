#lang scheme

;This is a request-based server (passive)

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
    (cond [(eq? '() event)
         (generate-info #t '())]
          ;; The new event is created here without defining the processing proc.
        [(eq? id (get-id-from-event event))
         (generate-info #f event)]
        [else (lookup id (next-event-in-pool pool))])))

;;;side effect on pool: defined inside the server

;;The callback service program
;;It has several steps need to wait

(define serv_req
  (lambda(n)
    (lambda(quit)
      (begin
        (display "Hello meower No.")
        (display n)
        (display "!")
        (newline)
        (set! quit (call/cc quit))
        (display "Hello again meower No. ")
        (display n)
        (display "!")
        (newline)
        (set! quit (call/cc quit))
        (display "Last time meower No.")
        (display n)
        (display ", bye!")
        (newline)
        (quit #t)))))


;; The program handling the request based on the current condition status in pool
;; Three conditions: 1. a newly-created serv program; 2. a executing serv program; 3. the sev program is normally ended. 



(define (server req_id_list serv_req)
  ;;Data
  (define event_list '())
  (define (pool)
    (define del_list
      (lambda(event)
        (begin
          (set! event_list
              (remove event event_list))
          "Updating...")))

    (define add-id-to-pool
      (lambda(id serv_proc)
        (if (boolean? serv_proc)
            (string-append "Process end, id: "
                           (number->string id))
            (begin
              (set! event_list
                    (cons(generate-event id serv_proc)
                         event_list))
              (string-append
               "Handled event from id: "
               (number->string id))))))
    (define create-id-to-pool
      (lambda(id serv_proc)
        (begin
          (set! event_list
                    (cons (generate-event id (serv_proc id))
                         event_list))
              (string-append
               "Created event for id: "
               (number->string id)))))
    (define update-pool
      (lambda (event)
        (begin
          (set! event_list (remove event event_list))
          (add-id-to-pool
           (get-id-from-event event)
           (call/cc (get-proc-from-event event) )))))

    (define dispatch
      (lambda(info)
        (cond [(eq? info 'update) update-pool]
              [(eq? info 'create) create-id-to-pool]
              [(eq? info 'get) event_list])))
    dispatch)
    
  ;;Procedure for each request
  (define proc_req
    (lambda(req_id)
      (let ([current_info (lookup req_id ((pool) 'get))])
        (cond [(new-event? current_info)
               (((pool) 'create) req_id serv_req)]
              [else
               (((pool) 'update)
                (generate-event req_id
                                (get-proc-from-info current_info)))]))))


  
  (map proc_req req_id_list))

;;Note: the return file related to proc should be processed in proc.
;;(server '(1 2 3))


(server '(1 1 1 2 2 2 2 2 1 1 1 1 1) serv_req)

;;More thoughts
;;1. Interaction between server and I/O interface can only be implemented in real time.
;;2. event_list cannot be defined under pool.

;;Some future implementations
;;1. Add priority for each request
;;2. Add more service programs, analogous to IST
