(define (lys:compile-file dir . opts)
  (let ((t1 (get-internal-real-time)))
    (chdir dir)
    (apply system* "lilypond" opts)
    (lys:display-elapsed t1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lys:typeset music output-filename)
  (let* ((t1 (get-internal-real-time))
         (score (scorify-music music)))
    (set! (paper-variable #f 'output-filename) output-filename)
    (lys:bookify-score score)
    (lys:display-elapsed t1)
  ; restore music length
))

(define (lys:typeset-slice music m1 m2 output-filename)
  (let* ((t1 (get-internal-real-time))
         (m1 (apply ly:make-moment m1))
         (m2 (apply ly:make-moment m2))
         (music-length (ly:music-length music))
         (score (scorify-music (lys:slice-music music m1 m2))))
    (set! (paper-variable #f 'output-filename) output-filename)
    (lys:bookify-score score)
    (lys:display-elapsed t1)
    ; important: restore music length, otherwise it will stay chopped
    (set! (ly:music-property music 'length) music-length)))

(define (lys:slice-music music m1 m2) (let* (
    (music-length (ly:music-length music))
    (m2 (if (moment<=? music-length m2) music-length m2))
    (show-beginning? (moment<=? m1 ZERO-MOMENT))
    (show-end? (moment<=? music-length m2))
    (beginning-length (make-duration-of-length (ly:moment-sub m1 ZERO-MOMENT)))

    (skip-music (if show-beginning?
      '()
      (list (context-spec-music (make-property-set 'skipTypesetting #t) 'Score)
            (make-music 'SkipMusic 'duration beginning-length)
            (context-spec-music (make-property-set 'skipTypesetting #f) 'Score)))))

  (if (not show-end?)
    (set! (ly:music-property music 'length) m2))

  (if (null? skip-music)
    music
    (make-simultaneous-music (list
      (make-sequential-music skip-music)
      music)))))
  
(define (lys:bookify-score score)
  (let* ((book-handler (if (defined? 'default-toplevel-book-handler)
                           default-toplevel-book-handler
                           toplevel-book-handler))
         (book (ly:make-book $defaultpaper $defaultheader)))
    (ly:book-add-score! book score)
    (book-handler book)))
         
