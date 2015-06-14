(defun write-text-centered-on (text target)
  "write given text centered on the given column"
  ;; (message "write-text-centered-on %s %d" text target)
  (let* ((halfname (floor (/ (length text) 2)))
         (col (- target halfname))) ;; target-pos-len/2
    (move-to-column col t)
    (insert (format "%s" text))))

(defun write-vertical-space (timelines)
  "write a row of only timelines"
  ;; (message "write-vertical-space")
  (dotimes (ii (length timelines))
    (let* ((col (plist-get (elt timelines ii) 'center)))
      (move-to-column col t)
      (insert (format "|")))))

(defun find-nearest-timeline (timelines col)
  "return the index of the nearest timeline to the given col"
  ;; (message "find-nearest-timeline")
  (let (ret
        delta
        olddelta)
    (dotimes (ii (length timelines))
      (setq delta (abs (- col (plist-get (elt timelines ii) 'origcenter))))
      (when (or (not ret) (< delta olddelta))
          (setq ret ii)
          (setq olddelta delta)))
    ret))

(defun write-arrow (from to dashed)
  "write arrow over row"
  (let ((delta (abs (- to from)))
        (ii 0)
        on) ;; bool to toggle between dash or space
    (move-to-column (1+ (min to from)))
    (if (> from to) ;; <---
        (insert-char ?<))
    (while (< ii (- delta 2))
      (insert-char (if (or (not dashed) on) ?- ? ))
      (if on (setq on nil) (setq on t)) ;; toggle dash
      (setq ii (1+ ii)))
    (if (< from to) ;; --->
        (insert-char ?>))
    (delete-char (- delta 1)))
  )

(defun fit-label-between (timelines left right width)
  "spread out timelines so that given label fits"
  ;; (message "fit-label-between")
  (let (leftcol
        rightcol
        delta
        ii
        elt)
    (setq leftcol (plist-get (elt timelines left) 'center))
    (setq rightcol (plist-get (elt timelines right) 'center))
    (setq needed (- (+ leftcol  width) rightcol))
    (when (> needed 0)
      (setq ii right)
      (while (< ii (length timelines))
        (setq elt (elt timelines ii))
        (plist-put elt 'center (+ (plist-get elt 'center) needed))
        (setq ii (1+ ii))))))

(defun sequence ()
  "formats a sequence diagram"
  (interactive)
  (let (top
        bottom
        line
        timelines
        messages)
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
    (end-of-line)
    (setq bottom (point))
    ;; (message "top: %d bottom: %d" top bottom)
    ;; (message "top: %d bottom: %d" (count-lines (point-min) top) (count-lines (point-min) bottom))

    ;; get timeline data
    ;; build array of plists like this:
    ;; [ (name "person1" origcenter 5 center 6) (name "person2" origcenter 12 center 18) ... ]
    (goto-char top)
    (setq line (buffer-substring (point) (line-end-position)))

    ;; count timelines
    (let ((start 0)
          (count 0))
      (while (string-match "\\([a-zA-Z0-9]+\\)" line start)
        (setq count (1+ count))
        (setq start (match-end 1)))
      (setq timelines (make-vector count nil)))

    ;; save timeline data
    (let ((start 0)
          (ii 0))
      (while (string-match "\\([a-zA-Z0-9]+\\)" line start)
        (aset timelines ii (list 'name       (match-string 1 line)
                                 'origcenter (floor (/ (+ (match-beginning 1) (match-end 1)) 2))
                                 'center     (+ 6 (* 12 ii))))
        (setq ii (1+ ii))
        (setq start (match-end 1))))

    ;; messages is a mixed list of plists of arrows and separators
    ;; arrows look like
    ;;   (from 0 to 2 label "doIt()" dashed f)
    ;; separators look like
    ;;   (text "title for next part")

    (let (label
          dashed
          from
          to
          found)
      (while (<= (point) bottom)
        (forward-line 1)
        (setq line (buffer-substring (point) (line-end-position)))
        (message "checking %s" line)
        (setq dashed (string-match "\- \-" line))

        (setq found nil)
        (when (string-match "\\([a-zA-Z0-9][a-zA-Z0-9\(\) ]*?[a-zA-Z0-9\(\)]?\\)[ |\-]*$" line)
          (setq label (match-string 1 line)))

        (when (and (not found) (string-match "\-.*>" line)) ;; ->
          (setq from (find-nearest-timeline timelines (match-beginning 0)))
          (setq to (find-nearest-timeline timelines (match-end 0)))
          (setq found t))

        (when (and (not found) (string-match "<.*\-" line)) ;; <-
          (setq from (find-nearest-timeline timelines (match-end 0)))
          (setq to (find-nearest-timeline timelines (match-beginning 0)))
          (setq found t))

        (when (and (not found) (string-match "|\-" line)) ;; |-
          (setq from (find-nearest-timeline timelines (match-beginning 0)))
          (setq to (1+ from))
          (if (< to (length timelines))
            (setq found t)))

        (when (and (not found) (string-match "\-|" line)) ;; -|
          (setq from (find-nearest-timeline timelines (match-beginning 0)))
          (setq to (- from 1))
          (if (>= to 0)
            (setq found t)))

        (when found
          (fit-label-between timelines (min to from) (max to from) (+ (length label) 4))
          (setq messages (append messages (list (list 'label  label
                                                      'from   from
                                                      'to     to
                                                      'dashed dashed))))
          (setq label nil))
        ))
    (goto-char top)
    (delete-char (- bottom top))


    ;; space out timelines
    (dotimes (ii (length timelines))
      (write-text-centered-on (plist-get (elt timelines ii) 'name)
                              (plist-get (elt timelines ii) 'center)))
    (newline)

    (dolist (elt messages)
      (write-vertical-space timelines)
      (newline)

      ;; write label
      (let ((text (plist-get elt 'label))
            center)
        (when text
          (write-vertical-space timelines)
          (newline)
          (forward-line -1)
          (setq center (floor (/ (+ (plist-get (elt timelines (plist-get elt 'from)) 'center)
                                    (plist-get (elt timelines (plist-get elt 'to)) 'center))
                                 2)))
          (write-text-centered-on text center)
          (delete-char (length text))
          (forward-line)))

      ;; write arrow
      (write-vertical-space timelines)
      (newline)
      (forward-line -1)
      (write-arrow (plist-get (elt timelines (plist-get elt 'from)) 'center)
                   (plist-get (elt timelines (plist-get elt 'to)) 'center)
                   (plist-get elt 'dashed))
      (forward-line))

    (write-vertical-space timelines)
    (newline)
    (goto-char top)))

(provide 'sequence)

(define-minor-mode uml-mode
  "Toggle UML mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When UML mode is enabled, C-c while the point is in a
sequence diagram cleans up the formatting of the diagram.
See the command \\[seqence]."
 ;; The initial value.
 :init-value nil
 ;; The indicator for the mode line.
 :lighter " uml"
 ;; The minor mode bindings.
 :keymap
 `((,(kbd "C-c C-c") . sequence))
 :group 'uml)
