;; just some stuff to help with emacs and overtone
(defun sylt-test-sinus ()
     (interactive)
     (cider-nrepl-sync-request:eval  "(demo (sin-osc))"))

(defun sylt-stop ()
     (interactive)
     (cider-nrepl-sync-request:eval  "(stop)"))


(defun sylt-play-metro ()
     (interactive)
     (cider-nrepl-sync-request:eval  "(seq/play-metro)"))


(define-key cider-mode-map (kbd "<kp-enter>") 'sylt-stop)
(define-key cider-mode-map (kbd "<kp-separator>") 'sylt-play-metro)
