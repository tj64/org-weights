;;; org-weights.el --- Show how heavy Org subtrees are.

;; Copyright © 2013 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Co-Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; URL: https://github.com/pinard/org-weights

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; Display the weights for every Org header which was visible at the
;; time the mode was activated.  The weights of a header are the
;; counts of subtrees and paragraphs for the the subtree starting with
;; that header.  Paragraphs includes items and other equivalent
;; structures.

;;; Code:

(defvar org-weights-heading-regexp ""
    "Regexp used to find position behind headline text.")
(make-variable-buffer-local 'org-weights-heading-regexp)

(defvar org-weights-display-weights-p t
  "If set to nil, hidden-lines-cookies instead of weights are shown.")

(defvar org-weights-hidden-lines-cookies-on-p nil
  "If non-nil, hidden-lines cookies are shown, otherwise hidden.")

(defgroup org-weights nil
  "Subtree weights and hidden-line-cookies for Org-mode and Outshine."
  :prefix "org-weights-"
  :group 'org)

(defcustom org-weights-hidden-lines-cookie-left-delimiter "["
  "Left delimiter of cookie that shows number of hidden lines."
  :group 'org-weights
  :type 'string)

(defcustom org-weights-hidden-lines-cookie-right-delimiter "]"
  "Left delimiter of cookie that shows number of hidden lines."
  :group 'org-weights
  :type 'string)

(defcustom org-weights-hidden-lines-cookie-left-signal-char "#"
  "Left signal character of cookie that shows number of hidden lines."
  :group 'org-weights
  :type 'string)

(defcustom org-weights-hidden-lines-cookie-right-signal-char ""
  "Right signal character of cookie that shows number of hidden lines."
  :group 'org-weights
  :type 'string)

;; original value was 75
(defcustom org-weights-overlay-column 65
  ;; Should be high enough for tags to stay visible.
  "Column where the weight overlay should be displayed."
  :group 'org-weights
  :type 'integer)

(defface org-weights-face
  '((((class color) (background light))
     (:foreground "chocolate2" :weight bold))
    (((class color) (background dark))
     (:foreground "chocolate1" :weight bold)))
  "Face for weights information higlights.")


(defvar org-weights-overlays nil
  "Running list of currently displayed overlays.")
(make-variable-buffer-local 'org-weights-overlays)

(defvar org-weights-saved-start nil
  "Header start position if, before command, point was within a header line.")

(define-minor-mode org-weights-mode
  "Show header weights in the entire buffer."
  nil nil nil
  (mapc 'delete-overlay org-weights-overlays)
  (setq org-weights-overlays nil)
  (remove-hook 'after-change-functions
               'org-weights-after-change 'local)
  (remove-hook 'pre-command-hook
               'org-weights-pre-command 'local)
  (remove-hook 'post-command-hook
               'org-weights-post-command 'local)
  (when org-weights-mode
    (org-weights-calc-heading-regexp)
    (save-excursion
      (goto-char (point-min))
      (outline-next-visible-heading 1)
      (while (not (eobp))
        (save-excursion
          (org-weights-set-overlay
           (if (eq major-mode 'org-mode)
               (org-weights-at-point-for-org-mode)
             (org-weights-at-point-for-outline))
           (funcall outline-level)
           (org-weights-body-lines)))
        (outline-next-visible-heading 1))
      (add-hook 'after-change-functions 'org-weights-after-change
                nil 'local)
      (add-hook 'pre-command-hook 'org-weights-pre-command
                nil 'local)
      (add-hook 'post-command-hook 'org-weights-post-command
                nil 'local))))

;;;; Hooks.

(defun org-weights-after-change (begin end replaced)
  "Recompute overlays for all headers between BEGIN and END, and up for each."
  (save-match-data
    (save-excursion
      (let ((bol (point-at-bol))
            (force t))
        (goto-char end)
        (condition-case nil
            (while (and (outline-back-to-heading)
                        (or force (>= (point) begin)))
              (unless (= (point) bol)
                (org-weights-set-overlay 
                 (if (eq major-mode 'org-mode)
                     (org-weights-at-point-for-org-mode)
                   (org-weights-at-point-for-outline))
                 (funcall outline-level)
                 (org-weights-body-lines)))
              (save-excursion
                (while (outline-up-heading 1)
                  (org-weights-set-overlay
                   (if (eq major-mode 'org-mode)
                       (org-weights-at-point-for-org-mode)
                     (org-weights-at-point-for-outline))
                   (funcall outline-level)
                   (org-weights-body-lines))))
              (setq force nil))
          (error nil))))))

(defun org-weights-pre-command ()
  "Save point if it stands within a header line."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (setq org-weights-saved-start
            (and (outline-on-heading-p)
                 (point-marker))))))

(defun org-weights-post-command ()
  "If point went on another line, remove the overlay when a header line.
Also, if the line changed, recompute the overlay for saved point."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (unless (and org-weights-saved-start
                   (= (point) org-weights-saved-start))
        (when (outline-on-heading-p)
          (org-weights-unset-overlay))
        (when org-weights-saved-start
          (let ((buffer (marker-buffer org-weights-saved-start)))
            (when buffer
              (set-buffer buffer)
              (goto-char org-weights-saved-start)
              (org-weights-set-overlay
               (if (eq major-mode 'org-mode)
                   (org-weights-at-point-for-org-mode)
                 (org-weights-at-point-for-outline))
               (funcall outline-level)
               (org-weights-body-lines)))))))))

;;;; Routines

(defun org-weights-set-overlay (weights level body-lines)
  "Put an overlays on the current line, displaying WEIGHTS.
Prefix weights with LEVEL number of stars."
  (let ((level-string
         (make-string (if level (org-get-valid-level level 0) 0) ?*))
        (headers (car weights))
        (paragraphs (cdr weights))
        filler overlay)
    (org-weights-goto-overlay-start-position)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (let ((overlays org-weights-overlays))
      (while overlays
        (let ((maybe (pop overlays)))
          (if (and (>= (point) (overlay-start maybe))
                   (<= (point) (overlay-end maybe)))
              (setq overlay maybe
                    overlays nil)))))
    (unless overlay
      (setq overlay (make-overlay (1- (point)) (point-at-eol))))
    (if org-weights-display-weights-p
        (setq filler (max 0 (- org-weights-overlay-column
                           (current-column) 2)))
      (setq filler 1))
    (let* ((strg
            (org-weights-calc-overlay-string
             level-string paragraphs headers body-lines))
           (lst (list 'face 'org-weights-face))
           (text (concat
                  (buffer-substring (1- (point)) (point))
                  (make-string (+ 2 filler) ? )
                  (if (eq major-mode 'org-mode)
                      (org-add-props strg lst)
                    (add-text-properties 0 (length strg) lst strg)
                    strg))))
      (if (not (featurep 'xemacs))
          (overlay-put overlay 'display text)
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'end-glyph (make-glyph text)))
      (push overlay org-weights-overlays))))

(defun org-weights-unset-overlay ()
  "Remove an overlay from the current line, so it gets edited more easily."
  (let (overlay)
    (org-weights-goto-overlay-start-position)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (let ((overlays org-weights-overlays))
      (while (and overlays (not overlay))
        (let ((maybe (pop overlays)))
          (if (and (>= (point) (overlay-start maybe))
                   (<= (point) (overlay-end maybe)))
              (setq overlay maybe)))))
    (when overlay
      (setq org-weights-overlays (delq overlay org-weights-overlays))
      (delete-overlay overlay))))

;; Compliment of Nicolas Goaziou <n.goaziou@gmail.com>, 2012-02-26
(defun org-weights-at-point-for-org-mode()
  "Return cons of number of subtrees and paragraphs in the subtree at point.
Paragraphs (also encompasses equivalent structures)."
  (org-with-wide-buffer
   (org-weights-narrow-to-subtree)
   (let ((tree (org-element-parse-buffer 'element)) (num-hl 0) (num-el 0))
     (org-element-map tree 'headline (lambda (hl) (incf num-hl)))
     (org-element-map
      tree '(paragraph table verse-block quote-block src-block example-block)
      (lambda (el) (incf num-el)))
     (cons (1- num-hl) num-el))))

(defun org-weights-at-point-for-outline ()
  "Return cons of number of subtrees and paragraphs in the subtree at point.
Paragraphs (also encompasses equivalent structures)."
  (save-excursion
    (save-restriction
      (widen)
      (org-weights-narrow-to-subtree)
      (let ((subtrees 0)
            (paragraphs 0)
            (last-line-empty nil))
        (goto-char (point-min))
        (forward-line)
        (while (not (eobp))
          (forward-paragraph)
          (setq paragraphs (1+ paragraphs)))
        (while (not (bobp))
          (outline-previous-heading)
          (setq subtrees (1+ subtrees)))
        ;; FIXME: 1 paragraph too much shown for last subtree in hierarchy?
        ;; FIXME: consecutive subtrees without body are seen as one
        ;; singe paragraph and thus mess up paragraph calculation
        (cons (1- subtrees) (1+ (- paragraphs subtrees)))))))

(defun org-weights-or-cookies ()
  "Toggles between displaying subtree weights or hidden-lines-cookies."
  (interactive)
  (if org-weights-display-weights-p
      (progn
        (setq org-weights-display-weights-p nil)
        (message "Displaying hidden-lines-cookies"))
    (setq org-weights-display-weights-p t)
    (message "Displaying subtree weights"))
  (and org-weights-mode (org-weights-mode) (org-weights-mode)))

(defun org-weights-body-lines ()
  "Return number of lines till next visible headline."
  (let* ((line-num-current-header (line-number-at-pos))
         (line-num-next-visible-header
          (save-excursion
            (outline-next-visible-heading 1)
            (line-number-at-pos))))
    (1- (- line-num-next-visible-header line-num-current-header))))

(defun org-weights-narrow-to-subtree ()
  "Dispatcher calling narrowing functions conditional on major-mode."
  (if (eq major-mode 'org-mode)
      (org-narrow-to-subtree)
    (progn
      (outline-mark-subtree)
      (and
       (use-region-p)
       (narrow-to-region (region-beginning) (region-end)))
      (deactivate-mark))))

(defun org-weights-calc-overlay-string
  (level-string paragraphs headers body-lines)
  "Calculate string to be displayed in overlay."
  (if org-weights-display-weights-p
      (format "%s %3s%s" level-string paragraphs
              (if (zerop headers) ""
                (format " + %s" headers)))
    (format
     " %s%s%s%s%s"
     org-weights-hidden-lines-cookie-left-delimiter
     org-weights-hidden-lines-cookie-left-signal-char
     body-lines
     org-weights-hidden-lines-cookie-right-signal-char
     org-weights-hidden-lines-cookie-right-delimiter)))


(defun org-weights-calc-heading-regexp ()
  "Calculate `org-weights-heading-regexp'."
  (setq org-weights-heading-regexp
        (concat
         ;; 1st group: outline-regexp
         "^\\("
         (if (eq major-mode 'org-mode)
             "\\*+ "
           outline-regexp)
         "\\)"

         ;; ;; FIXME does not match
         ;; ;; 2nd group: trimmed body
         ;; "\\(?: +\\(.*?\\)\\)?[ 	]*$"

         ;; ;; FIXME 
         ;; ;; 2nd group: headline text
         ;; "\\([^\s\t]+\\)"

         ;; 2nd group: headline text
         "\\([])(\\[.,;:\"'[:word:]+[:digit:]-]+[ \\t]?\\)+"

         ;; ;; 3rd group: rest till eol
         ;; "\\(.*$\\)"
         )))

(defun org-weights-goto-overlay-start-position ()
  "Move to postion where overlay should start."
    (cond
     ((and org-weights-display-weights-p
           (eq major-mode 'org-mode))
        (org-move-to-column org-weights-overlay-column))
     ((and org-weights-display-weights-p
           (not (eq major-mode 'org-mode)))
      (move-to-column org-weights-overlay-column))
     ((not org-weights-display-weights-p)
      (re-search-forward
       org-weights-heading-regexp
       (line-end-position)
       'NOERROR)
      (goto-char (match-end 2)))))
