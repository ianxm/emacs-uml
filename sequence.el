(defun sequence ()
  "formats a sequence diagram"
  (interactive)
  (let ((startpos (point))
        top
        bottom
        line
        lastmatch
        participants
        commands)
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
      (message "top: %d bottom: %d" top bottom)
      (message "top: %d bottom: %d" (count-lines (point-min) top) (count-lines (point-min) bottom))

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
