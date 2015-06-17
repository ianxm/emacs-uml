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
  (redraw-sequence-diagram (list 'name "swap left" 'col (current-column))))

(defun uml-swap-right ()
  "swap the timeline to the right"
  (interactive)
  (redraw-sequence-diagram (list 'name "swap right" 'col (current-column))))

(defun uml-delete-left ()
  "delete the timeline to the left"
  (interactive)
  (redraw-sequence-diagram (list 'name "delete" 'col (current-column))))

(defun uml-insert-left ()
  "insert a timeline to the left"
  (interactive)
  (redraw-sequence-diagram (list 'name "insert" 'col (current-column))))

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
  (dolist (elt timelines)
    (let* ((col (plist-get elt 'center)))
      (move-to-column col t)
      (insert (format "|")))))

(defun find-nearest-timeline (timelines col)
  "return the index of the nearest timeline to the given col"
  (let ((ii 0)
        olddelta
        ret
        delta)
    (dolist (elt timelines)
      (setq delta (abs (- col (plist-get elt 'origcenter))))
      (when (or (not ret) (< delta olddelta))
        (setq ret ii)
        (setq olddelta delta))
      (setq ii (1+ ii)))
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

(defun write-self-arrow (col text)
  "write arrow over row"
  (move-to-column (1+ col))
  (insert " --.")
  (delete-char (min 4 (- (line-end-position) (point))))
  (forward-line)
  (move-to-column (1+ col))
  (if (not text)
      (setq text ""))
  (insert (format "<--' %s" text))
  (delete-char (min (+ 5 (length text)) (- (line-end-position) (point))))
)

(defun fit-label-between (timelines left right width)
  "spread out timelines so that given label fits"
  (let (leftcol
        rightcol
        needed
        delta
        ii
        elt)
    (setq leftcol (plist-get (nth left timelines) 'center))
    (setq rightcol (plist-get (nth right timelines) 'center))
    (setq needed (- (+ leftcol  width) rightcol))
    (when (> needed 0)
      (setq ii right)
      (while (< ii (length timelines)) ;; dont need nth
        (setq elt (nth ii timelines))
        (plist-put elt 'center (+ (plist-get elt 'center) needed))
        (setq ii (1+ ii))))))

(defun swap-timelines (timelines messages col1 col2)
  "swap two timelines"
  (let (tmp)
    (setq tmp (nth col1 timelines))
    (setcar (nthcdr col1 timelines) (nth col2 timelines))
    (setcar (nthcdr col2 timelines) tmp))
  (dolist (elt messages)
    (if (= (plist-get elt 'from) col1) (plist-put elt 'from col2)
      (if (= (plist-get elt 'from) col2) (plist-put elt 'from col1)))
    (if (= (plist-get elt 'to) col1) (plist-put elt 'to col2)
      (if (= (plist-get elt 'to) col2) (plist-put elt 'to col1)))))

(defun delete-timeline (timelines messages col)
  "delete the given timeline"
  
)

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

    ;; create list of timelines
    (let ((start 0))
      (while (string-match "\\([a-zA-Z0-9]+\\)" line start)
        (setq timelines (append timelines (list (list 'name       (match-string 1 line)
                                                'origcenter (floor (/ (+ (match-beginning 1) (match-end 1)) 2))))))
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
          (setq found t))

         ((string-match "<.*\-" line) ;; <-
          (setq from (find-nearest-timeline timelines (match-end 0)))
          (setq to (find-nearest-timeline timelines (match-beginning 0)))
          (setq found t))

         ((string-match "<" line) ;; <
          (setq from (find-nearest-timeline timelines (match-end 0)))
          (setq to (find-nearest-timeline timelines (match-beginning 0)))
          (setq found t))

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

     ((string= "delete" (plist-get adjust 'name))
      (let (current delete)
        (setq current (find-nearest-timeline timelines (plist-get adjust 'col)))
        (setq delete (- current 1))
        (when (> delete 0)
          (delete-timeline timelines messages delete))))
     )

    ;; space out timelines to fit labels
    (dotimes (ii (length timelines))
      (plist-put (nth ii timelines) 'center (+ (* 12 ii) 6 (length prefix))))
    (dolist (elt messages)
      (let* ((to    (plist-get elt 'to))
             (from  (plist-get elt 'from))
             (left  (min to from))
             (right (max to from)))
        (if (= left right)
            (if (< (1+ left) (length timelines))
                (fit-label-between timelines ;; self arrow
                                   left
                                   (1+ left)
                                   (+ (length (plist-get elt 'label)) 8)))
          (fit-label-between timelines
                             left
                             right
                             (+ (length (plist-get elt 'label)) 4)))))

    ;; write timeline names
    (if prefix
        (insert prefix))
    (dolist (elt timelines)
      (write-text-centered-on (plist-get elt 'name)
                              (plist-get elt 'center)))
    (newline)

    ;; write messages
    (dolist (elt messages)
      (write-vertical-space timelines prefix)
      (newline)

      (let* ((text       (plist-get elt 'label))
             (from       (plist-get elt 'from))
             (to         (plist-get elt 'to))
             (fromcenter (plist-get (nth from timelines) 'center))
             (tocenter   (plist-get (nth to timelines) 'center))
             center
             selfmessage)
        (setq selfmessage (= (plist-get elt 'from) (plist-get elt 'to)))

        ;; write label
        (when (and text (not selfmessage))
          (write-vertical-space timelines prefix)
          (newline)
          (forward-line -1)
          (setq center (floor (/ (+ fromcenter tocenter) 2)))
          (write-text-centered-on text center)
          (delete-char (length text))
          (forward-line))

        ;; write arrow
        (if selfmessage
            (progn
              (write-vertical-space timelines prefix)
              (newline)
              (write-vertical-space timelines prefix)
              (newline)
              (forward-line -2)
              (write-self-arrow fromcenter text)
              (forward-line))
          (write-vertical-space timelines prefix)
          (newline)
          (forward-line -1)
          (write-arrow fromcenter tocenter (plist-get elt 'dashed))
          (forward-line))))

    (write-vertical-space timelines prefix)
    (goto-char top)
    (when (plist-get adjust 'movetocol)
        (move-to-column (plist-get (nth (plist-get adjust 'movetocol) timelines) 'center)))))

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
   (,(kbd "<M-left>") . uml-swap-left)
   (,(kbd "<M-right>") . uml-swap-right)
   (,(kbd "<M-S-left>") . uml-delete-left)
   (,(kbd "<M-S-right>") . uml-insert-left)
   (,(kbd "M-f") . uml-forward-timeline)
   (,(kbd "M-b") . uml-back-timeline))
 :group 'uml)

(provide 'uml)


