#lang racket
(require racket/gui/base)
(define block2 (new frame%
                    [label  "Move-On"]
                    [width 200] [height 200]))
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

(struct links (from to time) #:mutable)
(define list-links (list (links (bkrline-station harrow) (bkrline-station kent-bkr) 3)
                      (links (bkrline-station kent-bkr) (bkrline-station sken) 3)
                      (links (bkrline-station sken) (bkrline-station nwem) 3)
                      (links (bkrline-station nwem) (bkrline-station wemcen-bkr) 3)
                      (links (metline-station npark) (metline-station kent-met) 4)
                      (links (metline-station kent-met) (metline-station baker) 4)
                      (links (metline-station baker) (metline-station euston) 4)
                      (links (metline-station euston) (metline-station wemcen-met) 5)
                      (links (metline-station wemcen-met) (metline-station liverpool) 3)
                      (links (metline-station liverpool) (metline-station wempark) 5)
                      (links (metline-station wempark) (metline-station kent-met) 4)
                      (links (bkrline-station wemcen-bkr) (bkrline-station nwem) 3)
                      (links (bkrline-station nwem) (bkrline-station sken) 3)
                      (links (bkrline-station sken) (bkrline-station kent-bkr) 3)
                      (links (bkrline-station kent-bkr) (bkrline-station harrow) 3)
                      (links (metline-station kent-met) (metline-station wempark) 4)
                      (links (metline-station wempark) (metline-station liverpool) 5)
                      (links (metline-station liverpool) (metline-station wemcen-met) 3)
                      (links (metline-station wemcen-met) (metline-station euston) 5)
                      (links (metline-station euston) (metline-station baker) 4)
                      (links (metline-station baker) (metline-station kent-met) 4)
                      (links (metline-station kent-met) (metline-station npark) 4)))


(define choice (new choice%
                    [label "Location: "] [parent panel]
                    [choices stations]))

(define choice2 (new choice%
                     [label "Destination: "] [parent panel]
                      [choices stations]))

(define faster (new check-box%
                       [label "Fastest route"] [parent block2]                      
                       [value #t]
                       )) ;if value #f the box is not ticked

(define button (new button%
                    [parent block2]
                    [label "Get me there!"]
                    [callback (lambda (choice choice2)
                             (links-from b1) )]))

(define text-field (new text-field%
                        [label "Route plan: "] [parent block2]))

(define (travel-time From-Location To-Destination)
  (define total-time 0)
  (for ([link (list f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 b1 b2 b3 b4 b5 b6 b7 b8 b9)])
    (when (or (and (eq? (links-from link) From-Location) (eq? (links-to link) To-Destination))
            (and (eq? (links-from link) To-Destination) (eq? (links-to link) From-Location)))
      (set! total-time (+ total-time (links-time link)))))
  total-time)

(send block2 show #t)
