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
(define npark (metline "Northwick Park (R)")) ;;created two structs, one for each line
(define kent-met (metline "Kenton (R)"))      ;;(R) = ramp access
(define baker (metline "Baker Street (A) (T)")) ;; (A) = accessible through lift
(define euston (metline "Euston Square (R)"))   ;; (T) = the station has a toilet
(define wemcen-met (metline "Wembley Central (T)"))
(define liverpool (metline "Liverpool Street"))
(define wempark (metline "Wembley Park (A) (T)"))

(struct bkrline (station))
(define harrow (bkrline "Harrow (A) (T)"))     ;;throughout our code we call the elements of the structs to get the stations
(define kent-bkr (bkrline "Kenton (R)"))
(define sken (bkrline "South Kenton (A)"))
(define nwem (bkrline "North Wembley (R)"))
(define wemcen-bkr (bkrline "Wembley Central (T)"))

(define stations (list "Select"
                        (bkrline-station harrow)           ;; defined a list with all stations
                        (bkrline-station kent-bkr)         ;; that are later used in the choice object
                        (bkrline-station sken)
                        (bkrline-station nwem)
                        (bkrline-station wemcen-bkr)
                        (metline-station npark)
                        (metline-station baker)
                        (metline-station euston)
                        (metline-station liverpool)
                        (metline-station wempark)))

(define bkr-graph (list (list (bkrline-station harrow) (bkrline-station kent-bkr))  ;; defined a graph for bakerloo line
                    (list (bkrline-station kent-bkr) (bkrline-station sken))
                    (list (bkrline-station sken) (bkrline-station nwem)) 
                    (list (bkrline-station nwem) (bkrline-station wemcen-bkr))))

(define met-graph (list (list (metline-station npark) (bkrline-station kent-bkr))   ;; defined another graph for metropolitan line
                    (list (bkrline-station kent-bkr) (metline-station baker))
                    (list (metline-station baker) (metline-station euston))
                    (list (metline-station euston) (bkrline-station wemcen-bkr))
                    (list (bkrline-station wemcen-bkr) (metline-station liverpool))
                    (list (metline-station liverpool) (metline-station wempark))
                    (list (metline-station wempark) (metline-station npark))))

(define empty-list '())    ;; we defined an empty list to easy use later in the functions

(define reverse-line (lambda (a-graph empty-lst)     ;;the function reverse-line is returning the backward links of a line
                      (cond                          ;;we created this considering are the links are forward-backward
                        ((empty? a-graph) empty-lst) ;; we can easily manipulate the graphs in this way
                        (#t (reverse-line (rest a-graph) (append (cons (reverse (first a-graph)) '()) empty-lst))))))

(define bakerloo (append bkr-graph (reverse (reverse-line bkr-graph empty-list)))) ;;defined bakerloo graph forward-backward
(define metropolitan (append met-graph (reverse (reverse-line met-graph empty-list)))) ;;define metropolitan graph forward-backward
(define forward-backward (append bkr-graph met-graph (reverse (reverse-line bkr-graph empty-list)) (reverse (reverse-line met-graph empty-list))))

(define edge (lambda (x graph)                                       ;;we use the edge function from the handout to get the connections
               (map (lambda (y)(first (rest y))) (filter (lambda (y) ;;between stations
                                                           (eq? (first y) x)) graph))))

(define path-check (lambda (x y graph vertex-set)          ;; the path-check function, from the handout as well is used to check
               (cond                                       ;; if there is a path between the "from" station and "to" station
                 ((eq? x y) #t)
                 ((not (set-member? vertex-set x)) #f)
                 ((not (set-member? vertex-set y)) #f)
                 (#t (ormap (lambda (z) (path-check z y graph (set-remove vertex-set x))) (edge x graph)))
               )
               )
  )

(define create-route (lambda (from to route graph vertex-set)         ;; the create-route function is the same as the path-check function
                       (cond                                          ;; but instead of returning #t, will return the desired route
                         ((eq? from to) (reverse (cons from route)))  ;; to this function we added another argument 'route' which is an empty-list
                         ((not (set-member? vertex-set from)) #f)     ;; the function will append "from" station to the empty list everytime
                         ((not (set-member? vertex-set to)) #f)       ;; if "from" is equivalent with "to" then it will cons it and reverse the list to have the right order
                         (#t (ormap (lambda (z) (create-route z to (cons from route) graph (set-remove vertex-set from))) (edge from graph)))
                         )
                       )
  )

(define reachable (lambda (from to line) (path-check from to line (list->set (flatten line)))))
;; the reachable function is defined to simplify the code in the next function, it is given three arguments and in calls on path-check function
;; it does convert the given line from a list to a set and it flattens it to remove the pairs and duplicates
(define plan1 (lambda (from to line) (create-route from to empty-list line (list->set (flatten line)))))
;; just like the reachable function, we created plan1 to call on create-route function to simplify the code, it is given three arguments "from", "to"
;; and "line", to the create-route function is given "empty-list" defined earlier and coverts the list to a set and flattens it 

(define plan (lambda (from to)   ;;the plan is taking only two arguments "from" station and "to destination"  
                 (cond      
                   ((reachable from to bakerloo) (plan1 from to bakerloo)) ;; using reachable to identify a path, if yes will return it
                   ((reachable from to metropolitan) ;; if not on bakerloo line, then will check the metropolitan line
                    (cond
                      ((< (length (plan1 from to metropolitan)) (length (plan1 from to (reverse metropolitan)))) (plan1 from to metropolitan))
                      ;; it checks the length of the returned list by applying plan1 function against the length of the reverse
                      ;; if the length of forward returned list is smaller, will return it, otherwise the condition below will return the reverse
                      ;; this was neccessary because metropolitan line is a loop, so we want to make sure will take the fewer stations and will not go
                      ;; all the way arround
                      (#t (plan1 from to (reverse metropolitan)))
                      ))
                   ((reachable from to forward-backward)  ;; this condition checks the reachability between both connected lines,
                     (cond                                ;;e.g leave from a metropolitan station and to a bakerloo one
                      ((< (length (plan1 from to forward-backward)) (length (plan1 from to (reverse forward-backward)))) (plan1 from to forward-backward)) 
                      (#t (plan1 from to (reverse forward-backward)))))
                   (#t "Please select your leaving station and destination!") ;; if the input of stations is incorrect, this string will be returned
                   )
               )
  )

(define plan-string (lambda (from to)                          ;; the plan-string function will take the list of strings and append the string " -> " to every string
                      (for/list ([i (plan from to)])          
                        (cond
                          ((eq? i (last (plan from to))) i)    ;; when it gets to the last element, will return the iterator which will be the last string and it does
                          (#t (string-append i " -> "))        ;; not append " -> " to it
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

(define invert-stations (new button%                   ;; a button was implemented to switch the leaving station and the destination in case of returning
                             [label "Reverse"]         ;; we called it "Reverse"
                             [parent block2]           ;; we use the let construct to declare "i" a local variable to save the value of "from" choice
                             [callback (lambda (o e)   ;; we set the selection of "from" choice with the value of "to" choice
                                         (let ([i (send from get-selection)]) ;; and we set the value of "to" choice with the value of "from" saved in "i"
                                         (send from set-selection (send to get-selection))
                                         (send to set-selection i)))]))

(define get-me-there (new button%            ;; the button "Get me there!" calls the route-planning functions, first checks if the result is a single string
                    [parent block2]          ;; which is the case where the input is incorrect described above and if it is not a string then will 
                    [label "Get me there!"]  ;; will append all the strings returned into one and display the route into the "route-plan" text-field
                    [callback (lambda (o e)  ;; as (send from get-selection) returns the position of the element in the list, we used list-ref to get
                                (send route-plan set-value ;; the right input
                                      (cond
                                        ((string? (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection)))) (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection))))
                                        (#t (apply string-append (plan-string (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection))))))))]))

(define route-plan (new text-field%
                        [label "Route plan: "]  [parent block2]
                        [min-width 500]	 
                        [min-height 20]))

(define Time-route (new text-field%
                  [label "Time travel:"] [parent block2]
                  ))  
                        
(define clear (new button%
                   [parent block2]
                   [label "clear"]
                   [callback (lambda (o e)
                             (send route-plan set-value " ")  )]))

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
