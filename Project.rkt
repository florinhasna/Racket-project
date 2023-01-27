#lang racket
(require racket/gui/base)
(define block2 (new frame%
                    [label  "Move-On"]
                    [width 400] [height 200]))
(define panel (new horizontal-panel%
                   [parent block2]
                   [alignment '(center center)]))

(struct metline (station accessibility toilets shops))
(define npark (metline "Northwick Park" "(R)" #f #f))
(define kent-met (metline "Kenton" "(R)" #f #f))
(define baker (metline "Baker Street" "(A)" #t #t))
(define euston (metline "Euston Square" "(R)" #f #f))
(define wemcen-met (metline "Wembley Central" #f #t #f))
(define liverpool (metline "Liverpool Street" #f #f #t))
(define wempark (metline "Wembley Park" "(A)" #t #t))

(struct bkrline (station accessibility toilets shops))
(define harrow (bkrline "Harrow" "(A)" #t #f))
(define kent-bkr (bkrline "Kenton" "(R)" #f #f))
(define sken (bkrline "South Kenton" "(A)" #f #f))
(define nwem (bkrline "North Wembley" "R" #f #f))
(define wemcen-bkr (bkrline "Wembley Central" #f #t #f))

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

(define links (list (list (bkrline-station harrow) (bkrline-station kent-bkr))
                    (list (bkrline-station kent-bkr) (bkrline-station sken))
                    (list (bkrline-station sken) (bkrline-station nwem))
                    (list (bkrline-station nwem) (bkrline-station wemcen-bkr))
                    (list (metline-station npark) (metline-station kent-met))
                    (list (metline-station kent-met) (metline-station baker))
                    (list (metline-station baker) (metline-station euston))
                    (list (metline-station euston) (metline-station wemcen-met))
                    (list (metline-station wemcen-met) (metline-station liverpool))
                    (list (metline-station liverpool) (metline-station wempark))
                    (list (metline-station wempark) (metline-station npark))
                    (list (bkrline-station kent-bkr) (bkrline-station harrow))
                    (list (bkrline-station sken) (bkrline-station kent-bkr))
                    (list (bkrline-station nwem) (bkrline-station sken))
                    (list (bkrline-station wemcen-bkr) (bkrline-station nwem))
                    (list (metline-station kent-met) (metline-station npark))
                    (list (metline-station baker) (metline-station kent-met))
                    (list (metline-station euston) (metline-station baker))
                    (list (metline-station wemcen-met) (metline-station euston))
                    (list (metline-station liverpool) (metline-station wemcen-met))
                    (list (metline-station wempark) (metline-station liverpool))
                    (list (metline-station npark) (metline-station wempark))
                    )
  )

(define route '())

(define edge (lambda (x graph)
               (map (lambda (y)(first (rest y))) (filter (lambda (y) 
                                                           (eq? (first y) x)) graph))))

(define path (lambda (x y route graph vertex-set) 
               (cond
                 ((eq? x y) (reverse (cons x route)))
                 ((not (set-member? vertex-set x)) "Something is wrong")
                 ((not (set-member? vertex-set y)) "Something is wrong")
                 (#t (ormap (lambda (z) (path z y (cons x route) graph (set-remove vertex-set x))) (edge x graph)))
                 )
               )
  )

(define plan (lambda (x y) ;; (plan "Harrow" "Liverpool Street")
                    (path x y route links (list->set (flatten links)))))

;(define c1 (new choice%
                 ;   [label "Location: "]
                 ;   [choices stations]
                 ;   [parent panel]))

;(define c2 (new choice%
            ;         [label "Destination: "]
                  ;   [choices stations]
                  ;   [parent panel]))

(define from (new text-field%
                        [label "From: "] [parent block2]))

(define to (new text-field%
                        [label "To:     "] [parent block2]))

(define faster (new check-box%
                       [label "Fastest route"] [parent block2]                      
                       [value #t]
                       )) ;if value #f the box is not ticked

(define button (new button%
                    [parent block2]
                    [label "Get me there!"]
                    [callback (lambda (o e)
                                (send text-field set-value (plan (send from get-value) (send to get-value))))]))

(define text-field (new text-field%
                        [label "Route plan: "] [parent block2]))



(send block2 show #t)
