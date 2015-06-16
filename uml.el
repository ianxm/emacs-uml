;;; uml.el --- Minor mode for writing ascii uml sequence diagrams

;; Copyright (C) 2015 Ian Martins

;; Version: 0.0.1
;; Keywords: uml sequence diagram
;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: http://github.com/ianxm/emacs-uml

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; provides functions that help in writing ascii uml sequence diagrams

(defun uml-forward-timeline ()
  "move point to the next timeline bar"
  (interactive)
  (let ((start (point))
        word)
    (forward-word)
    (setq word (point))
    (goto-char start)
    (forward-char)
    (while (and
            (not (eq ?| (char-after)))
            (< (point) word))
      (forward-char))))

(defun uml-back-timeline ()
  "move point to the previous timeline bar"
  (interactive)
  (let ((start (point))
        word)
    (forward-word -1)
    (setq word (point))
    (goto-char start)
    (forward-char -1)
    (while (and
            (not (eq ?| (char-after)))
            (> (point) word))
      (forward-char -1))))

(defun uml-swap-left ()
  "swap the timeline to the left"
  (interactive)
  (redraw-sequence-diagram (list 'name "swap left" 'col (point))))

(defun uml-swap-right ()
  "swap the timeline to the right"
  (interactive)
  (redraw-sequence-diagram (list 'name "swap right" 'col (point))))

(defun uml-sequence-diagram ()
  "formats a sequence diagram"
  (interactive)
  (redraw-sequence-diagram nil))

(defun write-text-centered-on (text target)
  "write given text centered on the given column"
  (let* ((halfname (floor (/ (length text) 2)))
         (col (- target halfname))) ;; target-pos-len/2
    (move-to-column col t)
    (insert (format "%s" text))))

(defun write-vertical-space (timelines prefix)
  "write a row of only timelines"
  (if prefix
      (insert prefix))
  (dotimes (ii (length timelines))
    (let* ((col (plist-get (elt timelines ii) 'center)))
      (move-to-column col t)
      (insert (format "|")))))

(defun find-nearest-timeline (timelines col)
  "return the index of the nearest timeline to the given col"
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
        (insert ?<))
    (while (< ii (- delta 2))
      (insert (if (or (not dashed) on) ?- ? ))
      (if on (setq on nil) (setq on t)) ;; toggle dash
      (setq ii (1+ ii)))
    (if (< from to) ;; --->
        (insert ?>))
    (delete-char (- delta 1)))
  )

(defun fit-label-between (timelines left right width)
  "spread out timelines so that given label fits"
  (let (leftcol
        rightcol
        needed
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

(defun swap-timelines (timelines messages col1 col2)
  "swap two timelines"
  (let (tmp)
    (setq tmp (elt timelines col1))
    (aset timelines col1 (elt timelines col2))
    (aset timelines col2 tmp))
  (dolist (elt messages)
    (if (= (plist-get elt 'from) col1) (plist-put elt 'from col2)
      (if (= (plist-get elt 'from) col2) (plist-put elt 'from col1)))
    (if (= (plist-get elt 'to) col1) (plist-put elt 'to col2)
      (if (= (plist-get elt 'to) col2) (plist-put elt 'to col1)))))

(defun redraw-sequence-diagram (adjust)
  "redraws a sequence diagram"
  (let (top         ;; first line in buffer of diagram
        bottom      ;; last line in buffer of diagram
        line        ;; current line content
        prefix      ;; comment character or nil
        timelines   ;; list of timeline data
        messages)   ;; list of arrow data
    (beginning-of-line)

    ;; find the top of the diagram
    (setq line (buffer-substring (point) (line-end-position)))
    (while (and
            (not (string-match "^[\s+/;#\*]*$" line))
            (not (= (point) (point-min))))
      (forward-line -1)
      (setq line (buffer-substring (point) (line-end-position))))
    (when (string-match "^[\s+/;#\*]*$" line)
      (forward-line))
    (setq top (point))

    ;; find the bottom of the diagram
    (setq line (buffer-substring (point) (line-end-position)))
    (while (and
            (not (string-match "^[\s+/;#\*]*$" line))
            (not (= (point) (point-max))))
      (forward-line)
      (setq line (buffer-substring (point) (line-end-position))))
    (if (not (= (point) (point-max)))
        (forward-line -1))
    (setq bottom (line-end-position))
    ;; (message "top: %d bottom: %d" top bottom)
    ;; (message "top: %d bottom: %d" (count-lines (point-min) top) (count-lines (point-min) bottom))

    ;; get timeline data
    ;; build array of plists like this:
    ;; [ (name "person1" origcenter 5 center 6) (name "person2" origcenter 12 center 18) ... ]
    (goto-char top)
    (setq line (buffer-substring (point) (line-end-position)))

    (if (string-match "^\\(.*?\\)\s*[a-zA-Z0-9]" line)
        (setq prefix (match-string 1 line)))

    ;; count timelines
    (let ((start 0)
          (count 0))
      (while (string-match "\\([a-zA-Z0-9]+\\)" line start)
        (setq count (1+ count))
        (setq start (match-end 1)))
      (setq timelines (make-vector count nil)))

    ;; create list of timelines
    (let ((start 0)
          (ii 0))
      (while (string-match "\\([a-zA-Z0-9]+\\)" line start)
        (aset timelines ii (list 'name       (match-string 1 line)
                                 'origcenter (floor (/ (+ (match-beginning 1) (match-end 1)) 2))))
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
      (while (< (point) bottom)
        (forward-line 1)
        (setq line (buffer-substring (point) (line-end-position)))
        ;; (message "checking %s" line)
        (setq dashed (string-match "\- \-" line))

        (if (string-match "\\([a-zA-Z0-9][a-zA-Z0-9\(\) ]*?[a-zA-Z0-9\(\)]?\\)[ |>\-]*$" line)
            (setq label (match-string 1 line)))

        (setq found nil)
        (cond
         ((string-match "\-.*>" line) ;; ->
          (setq from (find-nearest-timeline timelines (match-beginning 0)))
          (setq to (find-nearest-timeline timelines (match-end 0)))
          (if (not (= from to))
              (setq found t)
            (message "ignoring self message")))
         

         ((string-match "<.*\-" line) ;; <-
          (setq from (find-nearest-timeline timelines (match-end 0)))
          (setq to (find-nearest-timeline timelines (match-beginning 0)))
          (if (not (= from to))
              (setq found t)
            (message "ignoring self message")))

         ((string-match "|\-" line) ;; |-
          (setq from (find-nearest-timeline timelines (match-beginning 0)))
          (setq to (1+ from))
          (if (< to (length timelines))
              (setq found t)
            (message "ignoring out of bounds message")))

         ((string-match "\-|" line) ;; -|
          (setq from (find-nearest-timeline timelines (match-beginning 0)))
          (setq to (- from 1))
          (if (>= to 0)
              (setq found t)
            (message "ignoring out of bounds message"))))
      
        (when found
          (setq messages (append messages (list (list 'label  label
                                                      'from   from
                                                      'to     to
                                                      'dashed dashed))))
          (setq label nil))
        ))
    (goto-char top)
    (delete-char (- bottom top))


    ;; make adjustments
    (cond 
     ((string= "swap left" (plist-get adjust 'name)) 
      (let (current swapwith)
        (setq current (find-nearest-timeline timelines (plist-get adjust 'col)))
        (setq swapwith (- current 1))
        (if (or (< swapwith 0) (>= swapwith (length timelines)))
            (plist-put adjust 'movetocol current)
          (plist-put adjust 'movetocol swapwith)
          (swap-timelines timelines messages current swapwith))))
     ((string= "swap right" (plist-get adjust 'name)) 
      (let (current swapwith)
        (setq current (find-nearest-timeline timelines (plist-get adjust 'col)))
        (setq swapwith (1+ current))
        (if (or (< swapwith 0) (>= swapwith (length timelines)))
            (plist-put adjust 'movetocol current)
          (plist-put adjust 'movetocol swapwith)
          (swap-timelines timelines messages current swapwith))))
     )

    ;; space out timelines to fit labels
    (dotimes (ii (length timelines))
      (plist-put (elt timelines ii) 'center (+ (* 12 ii) 6 (length prefix))))
    (dolist (elt messages)
      (fit-label-between timelines 
                         (min (plist-get elt 'to) (plist-get elt 'from))
                         (max (plist-get elt 'to) (plist-get elt 'from))
                         (+ (length (plist-get elt 'label)) 4)))

    ;; write timeline names
    (if prefix
        (insert prefix))
    (dotimes (ii (length timelines))
      (write-text-centered-on (plist-get (elt timelines ii) 'name)
                              (plist-get (elt timelines ii) 'center)))
    (newline)

    (dolist (elt messages)
      (write-vertical-space timelines prefix)
      (newline)

      ;; write label
      (let ((text (plist-get elt 'label))
            center)
        (when text
          (write-vertical-space timelines prefix)
          (newline)
          (forward-line -1)
          (setq center (floor (/ (+ (plist-get (elt timelines (plist-get elt 'from)) 'center)
                                    (plist-get (elt timelines (plist-get elt 'to)) 'center))
                                 2)))
          (write-text-centered-on text center)
          (delete-char (length text))
          (forward-line)))

      ;; write arrow
      (write-vertical-space timelines prefix)
      (newline)
      (forward-line -1)
      (write-arrow (plist-get (elt timelines (plist-get elt 'from)) 'center)
                   (plist-get (elt timelines (plist-get elt 'to)) 'center)
                   (plist-get elt 'dashed))
      (forward-line))

    (write-vertical-space timelines prefix)
    (goto-char top)
    (if (plist-get adjust 'movetocol)
        (move-to-column (plist-get (elt timelines (plist-get adjust 'movetocol)) 'center)))))

(define-minor-mode uml-mode
  "Toggle uml mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When uml mode is enabled, C-c while the point is in a
sequence diagram cleans up the formatting of the diagram.
See the command \\[uml-seqence-diagram]."
 ;; The initial value.
 :init-value nil
 ;; The indicator for the mode line.
 :lighter " uml"
 ;; The minor mode bindings.
 :keymap
 `((,(kbd "C-c C-c") . uml-sequence-diagram)
   (,(kbd "<M-S-left>") . uml-swap-left)
   (,(kbd "<M-S-right>") . uml-swap-right)
   (,(kbd "M-f") . uml-forward-timeline)
   (,(kbd "M-b") . uml-back-timeline))
 :group 'uml)

(provide 'uml)


