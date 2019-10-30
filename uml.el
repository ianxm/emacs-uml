;;; uml.el --- Minor mode for ascii uml sequence diagrams -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: http://github.com/ianxm/emacs-uml
;; Version: 0.0.2
;; Keywords: docs
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; provides functions that help in writing ascii uml sequence diagrams.

;;; Code:

(require 'subr-x)

(defun uml-forward-timeline ()
  "Move the point to the next timeline bar."
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
  "Move the point to the previous timeline bar."
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
  "Swap the timeline at the point with the timeline to its left."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name :swapleft 'col (current-column))))

(defun uml-swap-right ()
  "Swap the timeline at the point with the timeline to its right."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name :swapright 'col (current-column))))

(defun uml-delete-timeline ()
  "Delete the timeline at point."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name :delete 'col (current-column))))

(defun uml-insert-timeline ()
  "Insert a timeline to the right of the point."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name :insert 'col (current-column))))

(defun uml-sequence-diagram ()
  "Formats a sequence diagram."
  (interactive)
  (uml--redraw-sequence-diagram nil))

(defun uml--write-text-centered-on (text target)
  "Write TEXT centered on the TARGET column."
  (let* ((halfname (floor (/ (length text) 2)))
         (col (- target halfname))) ; target-pos-len/2
    (move-to-column col t)
    (insert (format "%s" text))))

(defun uml--write-vertical-space (timelines prefix)
  "Write a row of empty timeline bars for TIMELINES after writing PREFIX."
  (if prefix
      (insert prefix))
  (dolist (elt timelines)
    (let* ((col (plist-get elt 'center)))
      (move-to-column col t)
      (insert (format "|")))))

(defun uml--find-nearest-timeline (timelines col)
  "Return the index of the nearest of TIMELINES to the column COL."
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

(defun uml--write-arrow (from to dashed)
  "Write an arrow from FROM timeline to TO timeline, possibly with a DASHED line."
  (let ((delta (abs (- to from)))
        (ii 0)
        on)                             ; bool to toggle between dash or space
    (move-to-column (1+ (min to from)))
    (if (> from to)                     ; <---
        (insert ?<))
    (while (< ii (- delta 2))
      (insert (if (or (not dashed) on) ?- ? ))
      (if on (setq on nil) (setq on t)) ; toggle dash
      (setq ii (1+ ii)))
    (if (< from to)                     ; --->
        (insert ?>))
    (delete-char (- delta 1))))

(defun uml--write-self-arrow (col text)
  "Write an arrow from and to the COL timeline, labeled with TEXT."
  (move-to-column (1+ col))
  (insert " --.")
  (delete-char (min 4 (- (line-end-position) (point))))
  (forward-line)
  (move-to-column (1+ col))
  (if (not text)
      (setq text ""))
  (insert (format "<--' %s" text))
  (delete-char (min (+ 5 (length text)) (- (line-end-position) (point)))))

(defun uml--fit-label-between (timelines left right width)
  "Spread out TIMELINES so that LEFT and RIGHT have WIDTH space between them."
  (let (leftcol
        rightcol
        needed)
    (setq leftcol (plist-get (nth left timelines) 'center))
    (setq rightcol (plist-get (nth right timelines) 'center))
    (setq needed (- (+ leftcol  width) rightcol))
    (if (> needed 0)
      (uml--shift-to-the-right timelines right needed))))

(defun uml--shift-to-the-right (timelines right needed)
  "Shift all TIMELINES greater than or equal to RIGHT to the right by NEEDED."
  (let ((ii right)
        elt)
    (while (< ii (length timelines))
      (setq elt (nth ii timelines))
      (plist-put elt 'center (+ (plist-get elt 'center) needed))
      (setq ii (1+ ii)))))

(defun uml--swap-timelines (timelines messages col1 col2)
  "Given all TIMELINES and MESSAGES, swap COL1 and COL2."
  (let (tmp)
    (setq tmp (nth col1 timelines))
    (setcar (nthcdr col1 timelines) (nth col2 timelines))
    (setcar (nthcdr col2 timelines) tmp))
  (dolist (elt messages)
    (if (= (plist-get elt 'from) col1) (plist-put elt 'from col2)
      (if (= (plist-get elt 'from) col2) (plist-put elt 'from col1)))
    (if (= (plist-get elt 'to) col1) (plist-put elt 'to col2)
      (if (= (plist-get elt 'to) col2) (plist-put elt 'to col1)))))

(defun uml--find-top-or-bottom (direction)
  "Return the position at the top or bottom of the diagram depending on DIRECTION ('top or 'bottom)."
  (let ((end-of-buffer (if (eq direction 'top) (point-min) (point-max)))
        (step (if (eq direction 'top) -1 1)))
    (while (and
            (not (= (point) end-of-buffer))
            (not (re-search-forward "^[\s+/;#\*]*$" (line-end-position) t)))
      (forward-line step))
    (cond
     ((eq direction 'top)
      (if (re-search-forward "^[\s+/;#\*]*$" (line-end-position) t)
          (forward-line))
      (point))
     ((eq direction 'bottom)
      (if (not (= (point) (point-max)))
          (forward-line -1))
      (line-end-position)))))

(defun uml--calc-middle (start end)
  "This just computes the integer mean of START and END."
  (floor (/ (+ start end) 2)))

(defun uml--determine-prefix ()
  "Determine the prefix (if there is one)."
    (if (looking-at "^[^ ]+")
        (match-string 0)
      nil))

(defun uml--parse-timelines (prefix)
  "Parse the timeline names.
Look at the current line after the PREFIX and for each timeline,
determine the name and center column.  The return structure looks
like:
[ (name \"timeline1\" origcenter 5) (name \"timeline2\" origcenter 12) ... ]"
  (let (timelines)
    (forward-char (length prefix))
    (while (looking-at "\s*\\([a-zA-Z0-9\-_]+\\)")
      (setq timelines (append timelines (list (list 'name (match-string 1)
                                                    'origcenter (uml--calc-middle (- (match-beginning 1) (line-beginning-position))
                                                                                  (- (match-end 1) (line-beginning-position)))))))
      (goto-char (match-end 1)))
    timelines))

(defun uml--parse-messages (timelines prefix bottom)
  "Parse the messages from the diagram.
Parse messages from the diagram given the TIMELINES and PREFIX
until we reach the BOTTOM.  Messages is a mixed list of plists of
arrows and separators.  Arrows look like:
    (from 0 to 2 label \"doIt()\" dashed f)
Separators look like:
    (text \"title for next part\")"
    (let (messages label dashed found)
      (while (< (line-end-position)
                (- bottom (length prefix)))
        (forward-line 1)
        (forward-char (length prefix))

        ;; the label may be above the message or on the same line
        (when (re-search-forward "[a-zA-Z0-9][a-zA-Z0-9\(\) ]*" (line-end-position) t)
          (setq label (string-trim-right (match-string 0)))
          (beginning-of-line))

        ;; FOUND is (from . to) where FROM and TO are timeline indices
        (setq found (uml--find-message-bounds-maybe timelines))

        (when found
          (beginning-of-line)
          (setq dashed (re-search-forward "\- \-" (line-end-position) t))
          (setq messages (append messages (list (list 'label  label
                                                      'from   (car found)
                                                      'to     (cdr found)
                                                      'dashed dashed))))
          (setq label nil)))
      messages))

(defun uml--find-message-bounds-maybe (timelines)
  "Find which timelines a message connects.
Return the indices in TIMELINES between which the message passes
as (from . to), else nil if there is no message on the current
line"
  (let (from to found)
    (cond
     ((re-search-forward "\-.*>" (line-end-position) t) ; ->
      (setq from (uml--find-nearest-timeline timelines (- (match-beginning 0) (line-beginning-position))))
      (setq to (uml--find-nearest-timeline timelines (- (match-end 0) (line-beginning-position))))
      (setq found t))

     ((re-search-forward "<.*\-" (line-end-position) t) ; <-
      (setq from (uml--find-nearest-timeline timelines (- (match-end 0) (line-beginning-position))))
      (setq to (uml--find-nearest-timeline timelines (- (match-beginning 0) (line-beginning-position))))
      (setq found t))

     ((re-search-forward "<" (line-end-position) t)     ; <
      (setq from (uml--find-nearest-timeline timelines (- (match-end 0) (line-beginning-position))))
      (setq to (uml--find-nearest-timeline timelines (- (match-beginning 0) (line-beginning-position))))
      (setq found t))

     ((re-search-forward "|\-" (line-end-position) t)   ; |-
      (setq from (uml--find-nearest-timeline timelines (- (match-beginning 0) (line-beginning-position))))
      (setq to (1+ from))
      (if (< to (length timelines))
          (setq found t)
        (message "Ignoring out of bounds message.")))

     ((re-search-forward "\-|" (line-end-position) t)   ; -|
      (setq from (uml--find-nearest-timeline timelines (- (match-beginning 0) (line-beginning-position))))
      (setq to (- from 1))
      (if (>= to 0)
          (setq found t)
        (message "Ignoring out of bounds message."))))
    (if found (cons from to) nil)))

(defun uml--apply-adjustments (adjust timelines messages)
  "Apply ADJUST to TIMELINES and MESSAGES.
Return TIMELINES since we might have changed its head."
  (cond
   ((eq :swapleft (plist-get adjust 'name))
    (let (current swapwith)
      (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
      (setq swapwith (- current 1))
      (if (or (< swapwith 0) (>= swapwith (length timelines)))
          (plist-put adjust 'movetocol current)
        (plist-put adjust 'movetocol swapwith)
        (uml--swap-timelines timelines messages current swapwith))))

   ((eq :swapright (plist-get adjust 'name))
    (let (current swapwith)
      (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
      (setq swapwith (1+ current))
      (if (or (< swapwith 0) (>= swapwith (length timelines)))
          (plist-put adjust 'movetocol current)
        (plist-put adjust 'movetocol swapwith)
        (uml--swap-timelines timelines messages current swapwith))))

   ((eq :delete (plist-get adjust 'name))
    (let (current col)
      (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
      (setq col current)
      (plist-put adjust 'movetocol (max 0 (1- col)))
      (when (>= col 0)
        (setq timelines (delete (nth col timelines) timelines))
        (dolist (elt messages)
          (let ((from (plist-get elt 'from))
                (to   (plist-get elt 'to)))
            (if (or (= from col) (= to col))
                (setq messages (delete elt messages))
              (if (> from col) (plist-put elt 'from (- from 1)))
              (if (> to col) (plist-put elt 'to (- to 1)))))))))

   ((eq :insert (plist-get adjust 'name))
    (let (current new rest)
      (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
      (plist-put adjust 'movetocol current)
      (setq current (1+ current))
      (setq new (list (list 'name "new"
                            'origcenter nil)))
      (setq rest (nthcdr current timelines))
      (setcdr (nthcdr (- current 1) timelines) new)
      (setcdr new rest)
      (dolist (elt messages)
        (let ((from (plist-get elt 'from))
              (to   (plist-get elt 'to)))
          (if (>= from current) (plist-put elt 'from (1+ from)))
          (if (>= to current) (plist-put elt 'to (1+ to))))))))
  timelines)

(defun uml--space-out-timelines (timelines messages prefix)
  "Space out TIMELINES to fit MESSAGES' labels and PREFIX."
    (dotimes (ii (length timelines))
      (plist-put (nth ii timelines) 'center (+ (* 12 ii) 6 (length prefix))))
    (let (elt needed)
      (dotimes (ii (length timelines))
        (setq elt (nth ii timelines))
        (setq needed (max 0 (floor (/ (- (length (plist-get elt 'name)) 8) 2))))
        (when (> needed 0)
            (uml--shift-to-the-right timelines
                                ii
                                needed)
            (uml--shift-to-the-right timelines
                                (1+ ii)
                                needed))))

    (dolist (elt messages)
      (let* ((to    (plist-get elt 'to))
             (from  (plist-get elt 'from))
             (left  (min to from))
             (right (max to from)))
        (if (= left right)
            (if (< (1+ left) (length timelines))
                (uml--fit-label-between timelines ; self arrow
                                        left
                                        (1+ left)
                                        (+ (length (plist-get elt 'label)) 8)))
          (uml--fit-label-between timelines
                                  left
                                  right
                                  (+ (length (plist-get elt 'label)) 4))))))

(defun uml--write-diagram (timelines messages prefix)
  "Write the TIMELINES and MESSAGES using PREFIX to the buffer."

    ;; write prefix and timeline names
    (if prefix
        (insert prefix))
    (dolist (elt timelines)
      (uml--write-text-centered-on (plist-get elt 'name)
                              (plist-get elt 'center)))
    (newline)

    ;; write messages
    (dolist (elt messages)
      (uml--write-vertical-space timelines prefix)
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
          (uml--write-vertical-space timelines prefix)
          (newline)
          (forward-line -1)
          (setq center (floor (/ (+ fromcenter tocenter) 2)))
          (uml--write-text-centered-on text center)
          (delete-char (length text))
          (forward-line))

        ;; write arrow
        (if selfmessage
            (progn
              (uml--write-vertical-space timelines prefix)
              (newline)
              (uml--write-vertical-space timelines prefix)
              (newline)
              (forward-line -2)
              (uml--write-self-arrow fromcenter text)
              (forward-line))
          (uml--write-vertical-space timelines prefix)
          (newline)
          (forward-line -1)
          (uml--write-arrow fromcenter tocenter (plist-get elt 'dashed))
          (forward-line))))

    (uml--write-vertical-space timelines prefix))

(defun uml--redraw-sequence-diagram (adjust)
  "Redraws a sequence diagram after applying ADJUST.  This is the main routine."
  (let (top         ; first line in buffer of diagram
        bottom      ; last line in buffer of diagram
        prefix      ; comment character or nil
        timelines   ; list of timeline data
        messages)   ; list of arrow data

    (beginning-of-line)

    ;; find the top and bottom of the diagram
    (setq top (uml--find-top-or-bottom 'top))
    (setq bottom (uml--find-top-or-bottom 'bottom))
    ;; (message "top: %d bottom: %d" top bottom)

    (goto-char top)
    (setq prefix (uml--determine-prefix))

    ;; parse timeline headers from old diagram
    (setq timelines (uml--parse-timelines prefix))

    ;; parse messages from old diagram
    (setq messages (uml--parse-messages timelines prefix bottom))

    ;; clear the old diagram content from the buffer
    (goto-char top)
    (delete-char (- bottom top))

    ;; apply adjustments such as shifts or swaps
    (setq timelines (uml--apply-adjustments adjust timelines messages))

    ;; calculate timeline center columns
    (uml--space-out-timelines timelines messages prefix)

    ;; render the diagram into the buffer
    (uml--write-diagram timelines messages prefix)

    ;; move the cursor back to the column where it was before we did anything
    (goto-char top)
    (when (plist-get adjust 'movetocol)
        (move-to-column (plist-get (nth (plist-get adjust 'movetocol) timelines) 'center)))))

;;;###autoload
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
   (,(kbd "<M-S-left>") . uml-delete-timeline)
   (,(kbd "<M-S-right>") . uml-insert-timeline)
   (,(kbd "M-f") . uml-forward-timeline)
   (,(kbd "M-b") . uml-back-timeline))
 :group 'uml)

(provide 'uml)

;;; uml.el ends here
