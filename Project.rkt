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
                                        ((string? (plan (list-ref stations (send from get-selection))
                                                        (list-ref stations (send to get-selection))))
                                         (plan (list-ref stations (send from get-selection))
                                               (list-ref stations (send to get-selection))))
                                        (#t (apply string-append (plan-string (list-ref stations (send from get-selection))
                                                                              (list-ref stations (send to get-selection)))))))
                                 (send Time-route set-value
                                   (cond
                                     ((string? (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection))))
                                      " - ") 
                                     (#t (string-append  (number->string (- (+ (time-between (list (first (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection)))))
                                                                                 (list (first (reverse (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection)))))))
                                                                      (length (plan (list-ref stations (send from get-selection)) (list-ref stations (send to get-selection))))) 1))
                                                   " minutes" )))))]))

(define route-plan (new text-field%
                        [label "Route plan: "]  [parent block2]
                        [min-width 500] 
                        [min-height 20]))

(define Time-route (new text-field%
                  [label "Time travel:"] [parent block2]
                  ))  
                        
(define clear (new button%
                   [parent block2]
                   [label "Clear"]
                   [callback (lambda (o e)
                               (send route-plan set-value " ")
                               (send Time-route set-value " ")
                               (send from set-selection 0)
                               (send to set-selection 0))]))

(define-struct station (name time-to-Northwick-Park time-to-Baker-Street time-to-Kenton time-to-Wembley-Central time-to-Euston-Square    ;; This is a struct of each station
                             time-to-Liverpool-Street time-to-Harrow time-to-South-Kenton time-to-North-Wembley time-to-Wembley-Park))   ;; that the user could travel to.

(define NP (make-station "Northwick Park (R)" 0 7 4 16 12 13 7 7 10 3))   ;; This here is each station and how long they would take to reach another station.
(define BS (make-station "Baker Street (A) (T)" 8 0 4 9 4 13 7 7 10 8))
(define K (make-station "Kenton (R)" 4 4 0 9 8 9 3 3 6 4))
(define WC (make-station "Wembley Central (T)" 16 9 9 0 5 3 9 12 6 8))
(define ES (make-station "Euston Square (R)" 12 4 4 5 0 13 8 11 8 13))
(define L (make-station "Liverpool Street" 13 13 9 3 13 0 13 13 13 13))
(define H (make-station "Harrow (A) (T)" 7 7 3 9 11 13 0 6 9 7))
(define SK (make-station "South Kenton (A)" 7 7 3 6 6 11 6 0 3 7))
(define NW (make-station "North Wembley (R)" 10 10 6 3 3 8 9 3 0 7))
(define WP (make-station "Wembley Park (A) (T)" 3 8 4 8 13 8 13 7 7 0)) 

(define (time-between station1 station2)
  (cond [(equal? station1 '("Northwick Park (R)"))
           (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Northwick-Park NP))  ;; this code here is connected the two different struct of the stations that we have made
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Northwick-Park BS))  ;; the first struct we made was at the very top starting at line 12.
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Northwick-Park K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Northwick-Park WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Northwick-Park ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Northwick-Park L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Northwick-Park H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Northwick-Park SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Northwick-Park NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Northwick-Park WP)))]
        [(equal? station1 '("Baker Street (A) (T)")) 
          (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Baker-Street NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Baker-Street BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Baker-Street K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Baker-Street WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Baker-Street ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Baker-Street L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Baker-Street H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Baker-Street SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Baker-Street NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Baker-Street WP)))] 
       [(equal? station1 '("Kenton (R)"))          
          (cond
             ((equal? station2 '("Northwick Park (R)"))    (station-time-to-Kenton NP))
             ((equal? station2 '("Baker Street (A) (T)"))  (station-time-to-Kenton BS))
             ((equal? station2 '("Kenton (R)"))            (station-time-to-Kenton K))
             ((equal? station2 '("Wembley Central (T)"))   (station-time-to-Kenton WC))
             ((equal? station2 '("Euston Square (R)"))     (station-time-to-Kenton ES))
             ((equal? station2 '("Liverpool Street"))      (station-time-to-Kenton L))
             ((equal? station2 '("Harrow (A) (T)"))        (station-time-to-Kenton H))
             ((equal? station2 '("South Kenton (A)"))      (station-time-to-Kenton SK))
             ((equal? station2 '( "North Wembley (R)"))    (station-time-to-Kenton NW))
             ((equal? station2  '("Wembley Park (A) (T)")) (station-time-to-Kenton WP)))] 
        [(equal? station1 '("Wembley Central (T)"))  
            (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Wembley-Central NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Wembley-Central BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Wembley-Central K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Wembley-Central WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Wembley-Central ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Wembley-Central L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Wembley-Central H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Wembley-Central SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Wembley-Central NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Wembley-Central WP)))] 
       [(equal? station1 '("Euston Square (R)"))    
           (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Euston-Square NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Euston-Square BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Euston-Square K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Euston-Square WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Euston-Square ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Euston-Square L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Euston-Square H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Euston-Square SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Euston-Square NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Euston-Square WP)))]
      [(equal? station1 '("Liverpool Street"))   
        (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Liverpool-Street NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Liverpool-Street BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Liverpool-Street K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Liverpool-Street WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Liverpool-Street ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Liverpool-Street L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Liverpool-Street H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Liverpool-Street SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Liverpool-Street NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Liverpool-Street WP)))]
      [(equal? station1 '("Harrow (A) (T)") )      
         (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Harrow NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Harrow BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Harrow K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Harrow WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Harrow ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Harrow L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Harrow H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Harrow SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Harrow NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Harrow WP)))]
     [(equal? station1 '("South Kenton (A)"))     
           (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-South-Kenton NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-South-Kenton BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-South-Kenton K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-South-Kenton WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-South-Kenton ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-South-Kenton L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-South-Kenton H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-South-Kenton SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-South-Kenton NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-South-Kenton WP)))]
       [(equal? station1 '( "North Wembley (R)"))  
           (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-North-Wembley NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-North-Wembley BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-North-Wembley K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-North-Wembley WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-North-Wembley ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-North-Wembley L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-North-Wembley H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-North-Wembley SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-North-Wembley NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-North-Wembley WP)))] 
        [(equal? station1 '("Wembley Park (A) (T)")) 
             (cond
             ((equal? station2 '("Northwick Park (R)"))   (station-time-to-Wembley-Park NP))
             ((equal? station2 '("Baker Street (A) (T)")) (station-time-to-Wembley-Park BS))
             ((equal? station2 '("Kenton (R)"))           (station-time-to-Wembley-Park K))
             ((equal? station2 '("Wembley Central (T)"))  (station-time-to-Wembley-Park WC))
             ((equal? station2 '("Euston Square (R)"))    (station-time-to-Wembley-Park ES))
             ((equal? station2 '("Liverpool Street"))     (station-time-to-Wembley-Park L))
             ((equal? station2 '("Harrow (A) (T)"))       (station-time-to-Wembley-Park H))
             ((equal? station2 '("South Kenton (A)"))     (station-time-to-Wembley-Park SK))
             ((equal? station2 '( "North Wembley (R)"))   (station-time-to-Wembley-Park NW))
             ((equal? station2  '("Wembley Park (A) (T)"))(station-time-to-Wembley-Park WP)))]
        [else 0])) 

(send block2 show #t)
