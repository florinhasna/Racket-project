#lang racket
(require racket/gui/base)
(require racket/trace)
(define block2 (new frame%
                    [label  "Move-On"]
                    [width 400] [height 200]))
(define panel (new horizontal-panel%
                   [parent block2]
                   [alignment '(center center)]))

(struct metline (station))
(define npark (metline "Northwick Park (R)"))
(define kent-met (metline "Kenton (R)"))
(define baker (metline "Baker Street (A) (T)"))
(define euston (metline "Euston Square (R)"))
(define wemcen-met (metline "Wembley Central (T)"))
(define liverpool (metline "Liverpool Street"))
(define wempark (metline "Wembley Park (A) (T)"))

(struct bkrline (station))
(define harrow (bkrline "Harrow (A) (T)"))
(define kent-bkr (bkrline "Kenton (R)"))
(define sken (bkrline "South Kenton (A)"))
(define nwem (bkrline "North Wembley (R)"))
(define wemcen-bkr (bkrline "Wembley Central (T)"))

(define stations (list "Select"
                        (bkrline-station harrow)
                        (bkrline-station kent-bkr)
                        (bkrline-station sken)
                        (bkrline-station nwem)
                        (bkrline-station wemcen-bkr)
                        (metline-station npark)
                        (metline-station baker)
                        (metline-station euston)
                        (metline-station liverpool)
                        (metline-station wempark)))
                        
(define bkr-graph (list (list (bkrline-station harrow) (bkrline-station kent-bkr))
                    (list (bkrline-station kent-bkr) (bkrline-station sken))
                    (list (bkrline-station sken) (bkrline-station nwem)) 
                    (list (bkrline-station nwem) (bkrline-station wemcen-bkr))))

(define met-graph (list (list (metline-station npark) (bkrline-station kent-bkr))
                    (list (bkrline-station kent-bkr) (metline-station baker))
                    (list (metline-station baker) (metline-station euston))
                    (list (metline-station euston) (bkrline-station wemcen-bkr))
                    (list (bkrline-station wemcen-bkr) (metline-station liverpool))
                    (list (metline-station liverpool) (metline-station wempark))
                    (list (metline-station wempark) (metline-station npark))))

(define empty-list '())

(define reverse-line (lambda (a-graph empty-lst)
                      (cond
                        ((empty? a-graph) empty-lst)
                        (#t (reverse-line (rest a-graph) (append (cons (reverse (first a-graph)) '()) empty-lst))))))

(define bakerloo (append bkr-graph (reverse (reverse-line bkr-graph empty-list))))
(define metropolitan (append met-graph (reverse (reverse-line met-graph empty-list))))
(define forward-backward (append bkr-graph met-graph (reverse (reverse-line bkr-graph empty-list)) (reverse (reverse-line met-graph empty-list))))

(define edge (lambda (x graph)
               (map (lambda (y)(first (rest y))) (filter (lambda (y) 
                                                           (eq? (first y) x)) graph))))

(define path-check (lambda (x y graph vertex-set) 
               (cond
                 ((eq? x y) #t)
                 ((not (set-member? vertex-set x)) #f)
                 ((not (set-member? vertex-set y)) #f)
                 (#t (ormap (lambda (z) (path-check z y graph (set-remove vertex-set x))) (edge x graph)))
               )
               )
  )

(define create-route (lambda (from to route graph vertex-set) 
                       (cond 
                         ((eq? from to) (reverse (cons from route)))
                         ((not (set-member? vertex-set from)) #f) 
                         ((not (set-member? vertex-set to)) #f)
                         (#t (ormap (lambda (z) (create-route z to (cons from route) graph (set-remove vertex-set from))) (edge from graph)))
                         )
                       )
  )

(define reachable (lambda (from to line) (path-check from to line (list->set (flatten line)))))
(define plan1 (lambda (from to line) (create-route from to empty-list line (list->set (flatten line)))))

(define plan (lambda (from to)  
                 (cond      
                   ((reachable from to bakerloo) (plan1 from to bakerloo))
                   ((reachable from to metropolitan) 
                    (cond
                      ((< (length (plan1 from to metropolitan)) (length (plan1 from to (reverse metropolitan)))) (plan1 from to metropolitan))
                      (#t (plan1 from to (reverse metropolitan)))
                      ))
                   ((reachable from to forward-backward)
                    (cond
                      ((= (length (plan1 from to forward-backward)) (length (plan1 from to (reverse forward-backward))))
                       (append (plan1 from to forward-backward) (plan1 from to (reverse forward-backward))))
                      ((< (length (plan1 from to forward-backward)) (length (plan1 from to (reverse forward-backward)))) (plan1 from to forward-backward)) 
                      (#t (plan1 from to (reverse forward-backward)))))
                   (#t "Please select your leaving station and destination!")
                   )
               )
  )

(define plan-string (lambda (from to)
                      (for/list ([i (plan from to)])
                        (cond
                          ((eq? i (last (plan from to))) i)
                          (#t (string-append i " -> "))
                          )
                        )
                      )
  )

(define from (new choice%
                    [label "Location: "]
                    [choices stations]
                    [parent panel]))

(define to (new choice%
                     [label "Destination: "]
                     [choices stations]
                     [parent panel]))

(define invert-stations (new button%
                             [label "Invert"]
                             [parent block2]
                             [callback (lambda (o e)
                                         (let ([i (send from get-selection)])
                                         (send from set-selection (send to get-selection))
                                         (send to set-selection i)))]))

(define faster (new check-box%
                       [label "Fastest route"] [parent block2]                      
                       [value #t]
                       )) ;if value #f the box is not ticked

(define get-me-there (new button%
                    [parent block2]
                    [label "Get me there!"]
                    [callback (lambda (o e)
                                (send route-plan set-value
                                      (cond
                                        ((string? (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection)))) (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection))))
                                        (#t (apply string-append (plan-string (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection))))))))]))

(define route-plan (new text-field%
                        [label "Route plan: "]  [parent block2]
                        [min-width 500]	 
                        [min-height 50]))

(define (total-time station-list)
  (let loop ([stations station-list] [total-time 0])
    (if (null? (cdr stations))
        total-time
        (loop (cdr stations) (+ total-time (time-between (car stations) (cadr stations)))))))

(define (time-between station1 station2)
  (cond
    [(equal? station1 "Northwick Park (R)") (cond [(equal? station2 "Baker Street (A) (T)") 7]
                                               [(equal? station2 "Kenton (R)") 4]
                                               [(equal? station2 "Wembley Central (T)") 16]
                                               [(equal? station2 "Euston Square (R)") 12]
                                               [(equal? station2 "Liverpool Street") 13]
                                               ((equal? station2 "Harrow (A) (T)") 7)
                                               ((equal? station2 "South Kenton (A)") 7)
                                               [(equal? station1 "North Wembley (R)") 10]
                                               [(equal? station2 "Wembley Park (A) (T)") 8])] 
    
    [(equal? station1 "Baker Street (A) (T)") (cond [(equal? station2 "Northwick Park (R)") 8]
                                               [(equal? station2 "Kenton (R)") 4]
                                               [(equal? station2 "Wembley Central (T)") 9]
                                               [(equal? station2 "Euston Square (R)") 4]
                                               [(equal? station2 "Liverpool Street") 13]
                                               ((equal? station2 "Harrow (A) (T)") 7)
                                               ((equal? station2 "South Kenton (A)") 7)
                                               [(equal? station1 "North Wembley (R)") 10]
                                               [(equal? station2 "Wembley Park (A) (T)") 8])] 
    
    [(equal? station1 "Kenton (R)") (cond [(equal? station2 "Northwick Park (R)") 4]
                                               [(equal? station2 "Wembley Central (T)") 9]
                                               [(equal? station2 "Baker Street (A) (T)") 4]
                                               [(equal? station2 "Euston Square (R)") 8]
                                               [(equal? station2 "Liverpool Street") 9]
                                               ((equal? station2 "Harrow (A) (T)") 3)
                                               ((equal? station2 "South Kenton (A)") 3)
                                               [(equal? station1 "North Wembley (R)") 6]
                                               [(equal? station2 "Wembley Park (A) (T)") 4])] 
    
    [(equal? station1 "Wembley Central (T)") (cond [(equal? station2 "Northwick Park (R)") 16]
                                               [(equal? station2 "Baker Street (A) (T)") 13]
                                               [(equal? station2 "Euston Square (R)") 5]
                                               [(equal? station2 "Liverpool Street") 3]
                                               [(equal? station2 "Kenton (R)") 9]
                                               ((equal? station2 "Harrow (A) (T)") 12)
                                               ((equal? station2 "South Kenton (A)") 6)
                                               [(equal? station1 "North Wembley (R)") 3]
                                               [(equal? station2 "Wembley Park (A) (T)") 8])] 
    
    [(equal? station1 "Euston Square (R)") (cond [(equal? station2 "Northwick Park (R)") 12]
                                               [(equal? station2 "Baker Street (A) (T)") 4]
                                               [(equal? station2 "Liverpool Street") 13]
                                               [(equal? station2 "Kenton (R)") 8]
                                               ((equal? station2 "Harrow (A) (T)") 11)
                                               ((equal? station2 "South Kenton (A)") 11)
                                               [(equal? station1 "North Wembley (R)") 8]
                                               [(equal? station2 "Wembley Central (T)") 5]
                                               [(equal? station2 "Wembley Park (A) (T)") 13])] 
    
    
    [(equal? station1 "Liverpool Street") (cond ((equal? station2 "Northwick Park (R)") 13)
                                               [(equal? station2 "Baker Street (A) (T)") 13]
                                               [(equal? station2 "Euston Square (R)") 12]
                                               [(equal? station2 "Wembley Central (T)") 3]
                                               [(equal? station2 "Kenton (R)") 9]
                                               ((equal? station2 "Harrow (A) (T)") 12)
                                               ((equal? station2 "South Kenton (A)") 12)
                                               [(equal? station1 "North Wembley (R)") 15]
                                               [(equal? station2 "Wembley Park (A) (T)") 5])] 
                                            
[(equal? station1 "Wembley Park (A) (T)") (cond ((equal? station2 "Northwick Park (R)") 8)
                                               [(equal? station2 "Baker Street (A) (T)") 8]
                                               [(equal? station2 "Liverpool Street") 5]
                                               [(equal? station2 "Kenton (R)") 4]
                                               ((equal? station2 "Harrow (A) (T)") 7)
                                               ((equal? station2 "South Kenton (A)") 7)
                                               [(equal? station1 "North Wembley (R)") 10]
                                               [(equal? station2 "Wembley Central (T)") 8]
                                               [(equal? station2 "Euston Square (R)") 12] 
                                            
 [(equal? station1 "Harrow (A) (T)") (cond ((equal? station2 "Kenton (R)") 3)
                                               [(equal? station2 "Baker Street (A) (T)") 7]
                                               [(equal? station2 "Liverpool Street") 12]
                                               [(equal? station2 "Wembley Park (A) (T)") 7]
                                               ((equal? station2 "South Kenton (A)") 6)
                                               ((equal? station2 "Northwick Park (R)") 7)
                                               [(equal? station1 "North Wembley (R)") 9]
                                               [(equal? station2 "Wembley Central (T)") 12]
                                               [(equal? station2 "Euston Square (R)") 11] 
                                        
                                        
                                            
 [(equal? station1 "South Kenton (A)") (cond ((equal? station2 "North Wembley (R)") 3)
                                               [(equal? station2 "Baker Street (A) (T)") 7]
                                               [(equal? station2 "Liverpool Street") 12]
                                               [(equal? station2 "Kenton (R)") 3]
                                               [(equal? station2 "Wembley Park (A) (T)") 7]
                                               ((equal? station2 "Harrow (A) (T)") 6)
                                               ((equal? station2 "Northwick Park (R)") 7)
                                               [(equal? station2 "Wembley Central (T)") 6]
                                               [(equal? station2 "Euston Square (R)") 11] 
                                            
 [(equal? station1 "North Wembley (R)") (cond ((equal? station2 "South Kenton (A)") 3)
                                               [(equal? station2 "Baker Street (A) (T)") 10]
                                               [(equal? station2 "Liverpool Street") 6]
                                               [(equal? station2 "Kenton (R)") 6]
                                               [(equal? station2 "Wembley Park (A) (T)") 12]
                                               ((equal? station2 "Harrow (A) (T)") 9)
                                               ((equal? station2 "Northwick Park (R)") 12)
                                               [(equal? station2 "Wembley Central (T)") 3]
                                               [(equal? station2 "Euston Square (R)") 14] 
)])])])]))

  






(total-time '("Northwick Park (R)" "Baker Street (A) (T)" "Wembley Central (T)"))
(total-time '("Northwick Park (R)" "Wembley Central (T)"))




(send block2 show #t) 
