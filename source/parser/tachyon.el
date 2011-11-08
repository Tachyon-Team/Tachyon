;; _________________________________________________________________________
;;
;;             Tachyon : A Self-Hosted JavaScript Virtual Machine
;;
;;
;;  This file is part of the Tachyon JavaScript project. Tachyon is
;;  distributed at:
;;  http://github.com/Tachyon-Team/Tachyon
;;
;;
;;  Copyright (c) 2011, Universite de Montreal
;;  All rights reserved.
;;
;;  This software is licensed under the following license (Modified BSD
;;  License):
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the Universite de Montreal nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;;  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
;;  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; _________________________________________________________________________

;;; -*- Mode:Emacs-Lisp -*-
;;; tachyon.el --- Run Tachyon in an [X]Emacs shell buffer

;; Copyright (C) 1998-2010 Universite de Montreal, All Rights Reserved.

;; Authors: Marc Feeley <feeley@iro.umontreal.ca>

;; To use this package, make sure this file is accessible from your
;; load-path and that you require tachyon.el in your ~/.emacs file.
;; For example, if the file tachyon.el has been copied to the ~/EMACS
;; directory, then you should have these lines in your ~/.emacs file
;;
;;   (setq load-path (cons "~/EMACS" load-path))
;;   (require 'tachyon)
;;
;; You can then start a shell with "M-x shell" and run the Tachyon
;; compiler from that shell.  To pinpoint the source code of an error,
;; simply move the cursor to the line with the error message.

;------------------------------------------------------------------------------

;; User overridable parameters.

(defvar tachyon-pinpoint-command "\M-;"
  "Emacs keybinding for pinpointing the Javascript source code of an error.")

(defvar tachyon-highlight-source-color "gold"
  "Color of the overlay for highlighting Javascript source code.")

(defvar tachyon-highlight-error-color "gray"
  "Color of the overlay for highlighting error messages.")

(defvar tachyon-highlight-source-face
  (let ((face 'tachyon-highlight-source-face))
    (condition-case nil
        (progn
          (make-face face)
          (if (x-display-color-p)
              (set-face-background face tachyon-highlight-source-color)
              (set-face-underline-p face t)))
        (error (setq face nil)))
    face)
  "Face of overlay for highlighting Javascript source code.")

(defvar tachyon-highlight-error-face
  (let ((face 'tachyon-highlight-error-face))
    (condition-case nil
        (progn
          (make-face face)
          (if (x-display-color-p)
              (set-face-background face tachyon-highlight-error-color)
              (set-face-underline-p face t)))
        (error (setq face nil)))
    face)
  "Face of overlay for highlighting error messages.")

(defvar tachyon-new-window-height 8
  "Height of a window opened to highlight Javascript source code.")

(defvar tachyon-move-to-highlighted (not tachyon-highlight-source-face)
  "Flag to move to window opened to highlight Javascript source code.")

(defvar tachyon-location-regexp-alist
  '(("^\\([^ :]+\\):\\([0-9]+\\):" -1 2)
    ("\\(\\\"\\(\\\\\\\\\\|\\\\\"\\|[^\\\"\n]\\)+\\\"\\)@\\([0-9]+\\)\\.\\([0-9]+\\)[^-0-9]" 1 3 4)
    ("\\(\\\"\\(\\\\\\\\\\|\\\\\"\\|[^\\\"\n]\\)+\\\"\\)@\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\):" 1 3 4 5 6))
  "Regular expressions to detect location information in error messages")

;------------------------------------------------------------------------------

;; Portable functions for FSF Emacs and Xemacs.

(defun window-top-edge (window)
  (if (fboundp 'window-edges)
      (car (cdr (window-edges window)))
      (car (cdr (window-pixel-edges window)))))

;; Xemacs calls its overlays "extents", so we have to use them to emulate 
;; overlays on Xemacs.  Some versions of Xemacs have the portability package
;; "overlays.el" for this, so we could simply do:
;;
;; (condition-case nil ; load "overlay.el" if we have it
;;     (require 'overlay)
;;   (error nil))
;;
;; Unfortunately some versions of Xemacs don't have this package so
;; we explicitly define an interface to extents.

(if (not (fboundp 'make-overlay))
    (defun make-overlay (start end)
      (make-extent start end)))

(if (not (fboundp 'overlay-put))
    (defun overlay-put (overlay prop val)
      (set-extent-property overlay prop val)))

(if (not (fboundp 'move-overlay))
    (defun move-overlay (overlay start end buffer)
      (set-extent-endpoints overlay start end buffer)))

;------------------------------------------------------------------------------

;; Procedures to intercept and process the location information output
;; by Tachyon.

(defun tachyon-pinpoint-error ()
  (interactive)
  (let ((locat
         (tachyon-extract-location (tachyon-get-current-line))))
    (if locat
        (tachyon-highlight-location locat))))

(defun tachyon-get-current-line ()
  (save-excursion
    (beginning-of-line)
    (let ((b (point)))
      (end-of-line)
      (buffer-substring b (point)))))

(defun tachyon-extract-location (str)
  (let ((location nil)
        (alist tachyon-location-regexp-alist))
    (while (and (not location) (not (null alist)))
      (let* ((regexp (car alist))
             (x (string-match (car regexp) str)))
        (if x
            (let* ((pos1 (nth 1 regexp))
                   (pos2 (nth 2 regexp))
                   (name (if (< pos1 0)
                             (substring str
                                        (match-beginning (- pos1))
                                        (match-end (- pos1)))
                             (read (substring str
                                              (match-beginning pos1)
                                              (match-end pos1)))))
                   (line1 (read (substring str
                                           (match-beginning pos2)
                                           (match-end pos2)))))
              (if (not (null (cdr (cdr (cdr regexp)))))
                  (let* ((pos3 (nth 3 regexp))
                         (column1 (read (substring str
                                                   (match-beginning pos3)
                                                   (match-end pos3)))))
                    (if (not (null (cdr (cdr (cdr (cdr regexp))))))
                        (let* ((pos4 (nth 4 regexp))
                               (pos5 (nth 5 regexp))
                               (line2 (read (substring str
                                                       (match-beginning pos4)
                                                       (match-end pos4))))
                               (column2 (read (substring str
                                                         (match-beginning pos5)
                                                         (match-end pos5)))))
                          (setq location
                                (list name line1 column1 line2 column2)))
                        (setq location
                              (list name line1 column1 '() '()))))
                  (setq location
                        (list name line1 '() '() '())))))
        (setq alist (cdr alist))))
    location))

(defun tachyon-highlight-location (locat)
  (let ((name (car locat))
        (line1 (car (cdr locat)))
        (column1 (car (cdr (cdr locat))))
        (line2 (car (cdr (cdr (cdr locat)))))
        (column2 (car (cdr (cdr (cdr (cdr locat)))))))
    (cond ((stringp name)
           (let ((buffer (find-file-noselect name)))
             (if buffer
                 (if column1
                     (let ((p1 (tachyon-file-pos-to-point buffer
                                                          line1
                                                          column1)))
                       (and p1
                            (let ((p2 (tachyon-file-pos-to-point buffer
                                                                 line2
                                                                 column2)))
                              (tachyon-highlight-source buffer p1 p2))))
                     (let ((p12 (tachyon-file-line-to-points buffer line1)))
                       (and p12
                            (tachyon-highlight-source buffer
                                                      (car p12)
                                                      (cdr p12)))))))))))

(defun tachyon-file-pos-to-point (buffer line column)
  (and line
       (save-excursion
         (set-buffer buffer)
         (goto-line line)
         (forward-char (- column 1))
         (point))))

(defun tachyon-file-line-to-points (buffer line)
  (and line
       (save-excursion
         (set-buffer buffer)
         (goto-line line)
         (beginning-of-line)
         (let ((b (point)))
           (end-of-line)
           (cons b (point))))))

(defun tachyon-highlight-source (location-buffer pos1 pos2)

"Highlight the source code at a specific location in a buffer.

The location buffer is the one that contains the region to highlight
and \"pos1\" points to the first character of the region and \"pos2\"
just past the region.  If the location buffer is not visible then we
must display it in a window.  We also have to make sure the
highlighted region is visible, which may require the window to be
scrolled.

Our approach is simple: if the location buffer is not visible then we
split the selected window in 2 and use the bottom window to display
the location buffer.  Before we do the split, we enlarge the window if
it is too small."

  (let* ((location-windows
          (tachyon-windows-displaying-buffer location-buffer))
         (initially-current-buffer
          (current-buffer))
         (initially-selected-window
          (selected-window)))

    ; "location-windows" is the list of windows containing
    ; the location buffer.

    (if (null location-windows)

        (let* ((window-to-split
                initially-selected-window)
               (height
                (window-height window-to-split)))
          (select-window window-to-split)
          (if (< height (* 2 tachyon-new-window-height))
              (enlarge-window
               (- (* 2 tachyon-new-window-height)
                  height)))
          (let ((bottom-window
                 (split-window
                  window-to-split
                  (- (window-height window-to-split)
                     tachyon-new-window-height))))
            (select-window bottom-window)
            (switch-to-buffer location-buffer)))

        (select-window (car (reverse location-windows))))

    ; Highlight the region in the location buffer.

    (save-excursion
      (set-buffer (window-buffer (selected-window)))
      (goto-char pos1)
      (if (not (pos-visible-in-window-p))
          (recenter (- (/ (window-height) 2) 1)))
      (tachyon-highlight-source-region
       location-buffer
       pos1
       (or pos2
           (progn
             (condition-case nil
                 (forward-sexp)
               (error ; if forward-sexp fails with this condition name
                (forward-char 1)))
             (point))))
      (tachyon-highlight-error-region
       initially-current-buffer)
      (goto-char pos1))

    (if (not (eq initially-selected-window (selected-window)))
        (progn
          (goto-char pos1)
          (if (not tachyon-move-to-highlighted)
              (select-window initially-selected-window))))))

(defun tachyon-windows-displaying-buffer (buffer)
  (let ((windows '()))
    (walk-windows (function
                   (lambda (w)
                     (if (eq buffer (window-buffer w))
                         (setq windows (cons w windows)))))
                  t
                  'visible)
    (sort windows
          (function
           (lambda (w1 w2)
             (< (window-top-edge w1)
                (window-top-edge w2)))))))

(defvar tachyon-highlight-source-overlay
  (let ((ovl (make-overlay (point-min) (point-min))))
    (overlay-put ovl 'face tachyon-highlight-source-face)
    ovl)
  "Overlay for highlighting Javascript source code.")

(defvar tachyon-highlight-error-overlay
  (let ((ovl (make-overlay (point-min) (point-min))))
    (overlay-put ovl 'face tachyon-highlight-error-face)
    ovl)
  "Overlay for highlighting error message.")

(defun tachyon-highlight-source-region (buffer start end)
  (if tachyon-highlight-source-overlay
      (move-overlay tachyon-highlight-source-overlay start end buffer)))

(defun tachyon-highlight-error-region (buffer)
  (if tachyon-highlight-error-overlay
      (save-excursion
        (set-buffer buffer)
        (beginning-of-line)
        (let ((b (point)))
          (end-of-line)
          (let ((e (point)))
            (move-overlay tachyon-highlight-error-overlay b e buffer))))))

;------------------------------------------------------------------------------

(defun tachyon-extend-mode-map (map)
  (define-key map tachyon-pinpoint-command 'tachyon-pinpoint-error))

; Redefine next-line and previous-line so that they call tachyon-pinpoint-error.

(defun next-line (arg)
  (interactive "p")
  (if (and next-line-add-newlines (= arg 1))
      (let ((opoint (point)))
        (end-of-line)
        (if (eobp)
            (newline 1)
          (goto-char opoint)
          (line-move arg)))
    (if (interactive-p)
        (condition-case nil
            (line-move arg)
          ((beginning-of-buffer end-of-buffer) (ding)))
      (line-move arg)))
  (tachyon-pinpoint-error)
  nil)

(defun previous-line (arg)
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (line-move (- arg))
        ((beginning-of-buffer end-of-buffer) (ding)))
    (line-move (- arg)))
  (tachyon-pinpoint-error)
  nil)

(eval-after-load "shell"
  '(tachyon-extend-mode-map shell-mode-map))

(provide 'tachyon)

;------------------------------------------------------------------------------
