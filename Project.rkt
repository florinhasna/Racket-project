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
                             (send route-plan set-value " ")
                             (send Time-route set-value " ")]))

(define-struct station (name time-to-Northwick-Park time-to-Baker-Street time-to-Kenton time-to-Wembley-Central time-to-Euston-Square
                             time-to-Liverpool-Street time-to-Harrow time-to-South-Kenton time-to-North-Wembley time-to-Wembley-Park))

(define NP (make-station "Northwick Park (R)" 0 7 4 16 12 13 7 7 10 3))
(define BS (make-station "Baker Street (A) (T)" 8 0 4 9 4 13 7 7 10 8))
(define K (make-station "Kenton (R)" 4 4 0 9 8 9 3 3 6 4))
(define WC (make-station "Wembley Central (T)" 16 9 9 0 5 3 9 12 6 8))
(define ES (make-station "Euston Square (R)" 12 4 4 5 0 13 8 11 8 13))
(define L (make-station "Liverpool Street" 13 13 9 3 13 0 13 13 13 13))
(define H (make-station "Harrow (A) (T)" 7 7 3 9 11 13 7 0 3 7))
(define SK (make-station "South Kenton (A)" 7 7 3 6 6 11 13 3 0 7))
(define NW (make-station "North Wembley (R)" 10 10 6 3 3 8 8 3 7 0))
(define WP (make-station "Wembley Park (A) (T)" 3 8 4 8 13 8 13 7 7 0))

(define (time-between station1 station2)
  (cond [(equal? station1 NP) (station-time-to-Northwick-Park station2)]
        [(equal? station1 BS) (station-time-to-Baker-Street station2)]
        [(equal? station1 K) (station-time-to-Kenton station2)]
        [(equal? station1 WC) (station-time-to-Wembley-Central station2)]
        [(equal? station1 ES) (station-time-to-Euston-Square station2)]
        [(equal? station1 L) (station-time-to-Liverpool-Street station2)]
        [(equal? station1 H) (station-time-to-Harrow station2)]
        [(equal? station1 SK) (station-time-to-South-Kenton station2)]
        [(equal? station1 NW) (station-time-to-North-Wembley station2)]
        [(equal? station1 WP) (station-time-to-Wembley-Park station2)]
        [else 0]))

(define (total-time station-list)
  (let loop ([stations station-list] [total-time 0]) 
    (if (null? (cdr stations))
        total-time
        (loop (cdr stations) (+ total-time (time-between (car stations) (cadr stations)))))))

(time-between H K)
(time-between K H)
(time-between L ES)



(send block2 show #t) 
