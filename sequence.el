(defun sequence ()
  "formats a sequence diagram"
  (interactive)
  (let ((startpos (point))
        top
        bottom
        line
        linestart
        timelines)
    (save-excursion
      (beginning-of-line)
      ;; find the top of the diagram
      (setq line (buffer-substring (point) (line-end-position)))
      (while (and
              (not (string-match "^[\s+]*$" line))
              (eq 0 (forward-line -1)))
        (setq line (buffer-substring (point) (line-end-position))))
      (forward-line)
      (setq top (point))

      ;; find the bottom of the diagram
      (setq line (buffer-substring (point) (line-end-position)))
      (while (and
              (not (string-match "^[\s+]*$" line))
              (eq 0 (forward-line)))
        (setq line (buffer-substring (point) (line-end-position))))
      (forward-line -1)
      (setq bottom (point))
      ;; (message "top: %d bottom: %d" top bottom)
      ;; (message "top: %d bottom: %d" (count-lines (point-min) top) (count-lines (point-min) bottom))

      ;; get timeline labels
      ;; build list of timelines like this:
      ;; ( ((name . "person1") (center . 6)) ((name . "person2") (center . 18)) ... )
      (goto-char top)
      (setq line (buffer-substring (point) (line-end-position)))
      (setq names (split-string line))
      (dolist (ii (number-sequence 0 (- (length names) 1)) timelines)
              (setq timelines (append timelines (list (list (cons 'name (nth ii names))
                                                            (cons 'center (+ 6 (* 12 ii))))))))
      ;; space out timelines
      (forward-line 2)
      (setq linestart (point))
      (dolist (elt timelines)
        (let* ((target (cdr (assoc 'center elt)))
              (pos (- (point) linestart))
              (halfname (floor (/ (length (cdr (assoc 'name elt))) 2)))
              (tot (- (- target halfname) pos))) ;; target-pos-len/2
          (insert (format "%s%s" (make-string tot ? ) (cdr (assoc 'name elt))))
          ))
      (insert "\n")

      ;; write vertical space
      (setq linestart (point))
      (dolist (elt timelines)
        (let* ((target (cdr (assoc 'center elt)))
              (pos (- (point) linestart))
              (tot (- target pos))) ;; target-pos
          (insert (format "%s|" (make-string tot ? )))
          ))
      (insert "\n")

      (forward-line -2)

      ;; get timeline centers
      

      ;; (goto-char top)
      ;; (search-forward "@startuml")
      ;; (while (< (point) bottom)
      ;;   (forward-line)
      ;;   (setq line (buffer-substring (point) (line-end-position)))
      ;;   (message (format "read: %s" line))
      ;;   (if (string-match "participant\s+\\(\w+\\)\s+as\s+\\(\w+\\)" line)
      ;;       (message (format "found: %s as %s" (match-string 1) (match-string 2))
      ;;       )
      ;;   )
      ;;   (message (format " match %s" (string-match "participant \\(.*\\)" line)))
      ;;   (if (string-match "participant +\\([0-9a-zA-Z]+\\)" line)
      ;;       (message (format "  found: %s" (match-string 1))))
      ;;   (if (string-match "(.*)" line)
      ;;       (message (format "  stuf: %s" (match-string 1)))
      ;;   )
      ;; )
    )
  )
)
(provide 'sequence)
