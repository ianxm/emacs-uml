(defun write-text-centered-on (text target linestart)
  "writes given text centered on the given column"
  (let* ((pos (- (point) linestart))
         (halfname (floor (/ (length text) 2)))
         (tot (- (- target halfname) pos))) ;; target-pos-len/2
    (insert (format "%s%s" (make-string tot ? ) (cdr (assoc 'name elt))))))

(defun write-vertical-space (timelines linestart)
  "write a row of timelines"
  (dolist (elt timelines)
    (let* ((target (cdr (assoc 'center elt)))
           (pos (- (point) linestart))
           (tot (- target pos))) ;; target-pos
      (insert (format "%s|" (make-string tot ? ))))))

(defun sequence ()
  "formats a sequence diagram"
  (interactive)
  (let (top
        bottom
        line
        timelines
        messages)
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
      ;; ( ((name . "person1") (center . 6)) ((name . "person2") (origcenter . 12) (center . 18)) ... )
      (goto-char top)
      (setq line (buffer-substring (point) (line-end-position)))

      (let ((start 0)
            (ii 0))
        (while (string-match "\\([a-zA-Z0-9]+\\)" line start)
          (setq timelines (append timelines (list (list (cons 'name (match-string 1 line))
                                                        (cons 'origcenter (floor (/ (+ (match-beginning 1) (match-end 1)) 2)))
                                                        (cons 'center (+ 6 (* 12 ii)))))))
          (setq ii (1+ ii))
          (setq start (match-end 1))))

      ;; messages is a mixed list of arrows and separators
      ;; arrows look like
      ;;   ((from . 0) (to . 2) (text . "doIt()") (dashed . f))
      ;; separators look like
      ;;   ((text . "title for next part"))

      (while (not (eq (point) bottom))
        (forward-line 1)
        (setq line (buffer-substring (point) (line-end-position)))
        (message "checking %s" line)
        
        (when (string-match "\\([a-zA-Z0-9][a-zA-Z0-9 ]*?[a-zA-Z0-9]?\\)[ |]*$" line)
          (message "  found words %s" (match-string 1 line))
          )
        (when (string-match "|\\-.*>|" line)
          (message "  found right arrow")
          )
        (when (string-match "|<.*\\-|" line)
          (message "  found left arrow")
          )
        )

      ;; space out timelines
      (forward-line 2)
      (let ((linestart (point)))
        (dolist (elt timelines)
          (write-text-centered-on (cdr (assoc 'name elt))
                                  (cdr (assoc 'center elt))
                                  linestart)))
      (insert "\n")

      ;; write vertical space
      (write-vertical-space timelines (point))
      (insert "\n")

      (forward-line -2)

    )
  )
)
(provide 'sequence)
