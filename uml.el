;;; uml.el --- Minor mode for writing ascii uml sequence diagrams

;; Copyright (C) 2015-2019 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: http://github.com/ianxm/emacs-uml
;; Version: 0.0.2
;; Keywords: uml sequence diagram

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
  (uml--redraw-sequence-diagram (list 'name "swap left" 'col (current-column))))

(defun uml-swap-right ()
  "Swap the timeline at the point with the timeline to its right."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name "swap right" 'col (current-column))))

(defun uml-delete-left ()
  "Delete the timeline to the left of the point."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name "delete" 'col (current-column))))

(defun uml-insert-left ()
  "Insert a timeline to the left of the point."
  (interactive)
  (uml--redraw-sequence-diagram (list 'name "insert" 'col (current-column))))

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

(defun uml--redraw-sequence-diagram (adjust)
  "Redraws a sequence diagram after applying ADJUST."
  (let (top         ; first line in buffer of diagram
        bottom      ; last line in buffer of diagram
        line        ; current line content
        prefix      ; comment character or nil
        timelines   ; list of timeline data
        messages    ; list of arrow data
        elt)
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

    (if (string-match "^\\(.*?\\)\s*[a-zA-Z0-9\-_]" line)
        (setq prefix (match-string 1 line)))

    ;; create list of timelines
    (let ((start 0))
      (while (string-match "\\([a-zA-Z0-9\-_]+\\)" line start)
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
         ((string-match "\-.*>" line) ; ->
          (setq from (uml--find-nearest-timeline timelines (match-beginning 0)))
          (setq to (uml--find-nearest-timeline timelines (match-end 0)))
          (setq found t))

         ((string-match "<.*\-" line) ; <-
          (setq from (uml--find-nearest-timeline timelines (match-end 0)))
          (setq to (uml--find-nearest-timeline timelines (match-beginning 0)))
          (setq found t))

         ((string-match "<" line)     ; <
          (setq from (uml--find-nearest-timeline timelines (match-end 0)))
          (setq to (uml--find-nearest-timeline timelines (match-beginning 0)))
          (setq found t))

         ((string-match "|\-" line)   ; |-
          (setq from (uml--find-nearest-timeline timelines (match-beginning 0)))
          (setq to (1+ from))
          (if (< to (length timelines))
              (setq found t)
            (message "ignoring out of bounds message")))

         ((string-match "\-|" line)   ; -|
          (setq from (uml--find-nearest-timeline timelines (match-beginning 0)))
          (setq to (- from 1))
          (if (>= to 0)
              (setq found t)
            (message "ignoring out of bounds message"))))

        (when found
          (setq messages (append messages (list (list 'label  label
                                                      'from   from
                                                      'to     to
                                                      'dashed dashed))))
          (setq label nil))))
    (goto-char top)
    (delete-char (- bottom top))

    ;; make adjustments
    (cond
     ((string= "swap left" (plist-get adjust 'name))
      (let (current swapwith)
        (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
        (setq swapwith (- current 1))
        (if (or (< swapwith 0) (>= swapwith (length timelines)))
            (plist-put adjust 'movetocol current)
          (plist-put adjust 'movetocol swapwith)
          (uml--swap-timelines timelines messages current swapwith))))

     ((string= "swap right" (plist-get adjust 'name))
      (let (current swapwith)
        (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
        (setq swapwith (1+ current))
        (if (or (< swapwith 0) (>= swapwith (length timelines)))
            (plist-put adjust 'movetocol current)
          (plist-put adjust 'movetocol swapwith)
          (uml--swap-timelines timelines messages current swapwith))))

     ((string= "delete" (plist-get adjust 'name))
      (let (current col)
        (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
        (setq col (- current 1))
        (plist-put adjust 'movetocol col)
        (when (>= col 0)
          (setq timelines (delete (nth col timelines) timelines))
          (dolist (elt messages)
            (let ((from (plist-get elt 'from))
                  (to   (plist-get elt 'to)))
              (if (or (= from col) (= to col))
                  (setq messages (delete elt messages))
                (if (> from col) (plist-put elt 'from (- from 1)))
                (if (> to col) (plist-put elt 'to (- to 1)))))))))

     ((string= "insert" (plist-get adjust 'name))
      (let (current new rest)
        (setq current (uml--find-nearest-timeline timelines (plist-get adjust 'col)))
        (plist-put adjust 'movetocol current)
        (setq new (list (list 'name       "new"
                              'origcenter nil)))
        (if (= current 0)
            (setq timelines (append new timelines))
          (setq rest (nthcdr current timelines))
          (setcdr (nthcdr (- current 1) timelines) new)
          (setcdr new rest))
        (dolist (elt messages)
          (let ((from (plist-get elt 'from))
                (to   (plist-get elt 'to)))
            (if (>= from current) (plist-put elt 'from (1+ from)))
            (if (>= to current) (plist-put elt 'to (1+ to))))))))

    ;; space out timelines to fit titles and labels
    (dotimes (ii (length timelines))
      (plist-put (nth ii timelines) 'center (+ (* 12 ii) 6 (length prefix))))
    (let (needed)
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
                                  (+ (length (plist-get elt 'label)) 4)))))

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

    (uml--write-vertical-space timelines prefix)
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
   (,(kbd "<M-S-left>") . uml-delete-left)
   (,(kbd "<M-S-right>") . uml-insert-left)
   (,(kbd "M-f") . uml-forward-timeline)
   (,(kbd "M-b") . uml-back-timeline))
 :group 'uml)

(provide 'uml)

;;; uml.el ends here
