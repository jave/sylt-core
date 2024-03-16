;; just some stuff to help with emacs and overtone
(defun sylt-test-sinus ()
     (interactive)
     (cider-nrepl-sync-request:eval  "(overtone.live/demo (overtone.live/sin-osc))"))

(defun sylt-stop ()
     (interactive)
     (cider-nrepl-sync-request:eval  "(overtone.sc.server/stop)"))


(defun sylt-play-metro ()
     (interactive)
     (cider-nrepl-sync-request:eval  "(sylt.loseq/play-metro)"))




(define-key cider-mode-map (kbd "<kp-enter>") 'sylt-stop)
(define-key cider-mode-map (kbd "<kp-separator>") 'sylt-play-metro)

(defmacro sylt-song-event-map-key (key num)
  `(define-key cider-mode-map (kbd ,key)
              (lambda  () (interactive)   (cider-nrepl-sync-request:eval  ,(concat "(sylt.core/song-event \"" num "\")")))))

(sylt-song-event-map-key "<kp-0>" "init")
(sylt-song-event-map-key "<kp-1>" "1")
(sylt-song-event-map-key "<kp-2>" "2")
(sylt-song-event-map-key "<kp-3>" "3")
(sylt-song-event-map-key "<kp-4>" "4")
(sylt-song-event-map-key "<kp-5>" "5")
(sylt-song-event-map-key "<kp-6>" "6")
(sylt-song-event-map-key "<kp-7>" "7")
(sylt-song-event-map-key "<kp-8>" "8")
(sylt-song-event-map-key "<kp-9>" "9")
