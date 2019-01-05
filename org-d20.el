;;; org-d20.el --- minor mode for tabletop roleplaying games that use a d20

;; Copyright (C) 2017-2018  Sean Whitton

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; URL:
;; Version: 0.3pre
;; Package-Version: 0.3pre
;; Package-Requires: (s seq dash)
;; Keywords: roleplaying

;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; A minor mode intended for use in an Org-mode file in which you are
;;; keeping your GM notes for a tabletop roleplaying game that uses a
;;; d20.

;;; Example file footer:
;;;
;;;     # Local Variables:
;;;     # eval: (org-d20-mode 1)
;;;     # org-d20-party: (("Zahrat" . 2) ("Ennon" . 4) ("Artemis" . 5))
;;;     # End:
;;;
;;; Alternatively, example first line of file:
;;;
;;;     # -*- mode: org; mode: org-d20; org-d20-party: (("Zahrat" . 0) ("Anca" . 1)) -*-

;;; Code:

(require 's)
(require 'seq)
(require 'dash)

(defcustom org-d20-party nil
  "Party initiative modifiers.  A list of cons cells, where the
  car of each cell is a character's name, and the cdr of each
  cell is that character's initiative modifier as an integer.")

(defcustom org-d20-dice-sound nil
  "Path to a sound file that `play-sound-file' can play.")

(defcustom org-d20-letter-monsters nil
  "Individuate up to 26 monsters/NPCs with letters, rather than
  with digits.")

(defcustom org-d20-continue-monster-numbering nil
  "Continue the numbering/lettering of monsters between types of
  monsters, rather than starting again for each type.")

(defvar org-d20-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f9>") 'org-d20-initiative-dwim)
    (define-key map (kbd "S-<f9>") 'org-d20-initiative-add)
    (define-key map (kbd "<f10>") 'org-d20-damage)
    (define-key map (kbd "S-<f10>") 'org-d20-roll-at-point)
    (define-key map (kbd "<f11>") 'org-d20-roll)
    (define-key map (kbd "S-<f11>") 'org-d20-roll-last)
    (define-key map (kbd "<f12>") 'org-d20-d20)
    (define-key map (kbd "S-<f12>") 'org-d20-d%)
    map)
  "Keymap for `org-d20-mode'.")

(defun org-d20--roll (exp)
  "Evaluate dice roll expression EXP.

Returns a cons cell, whose car is a string expressing the results
on each dice rolled, and whose cdr is the final result.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (let ((exps (seq-map (lambda (s) (s-chop-prefix "+" s))
                       (s-slice-at "[+-]" exp))))
    (seq-reduce #'org-d20--roll-inner exps '("" . 0))))

(defun org-d20--roll-inner (accum exp)
  (-let* (((rolls . total) accum)
          (sign (if (s-prefix-p "-" exp) -1 1))
          (ours (let ((chopped (s-chop-prefix "-" exp)))
                  (if (string= (substring chopped 0 1) "d")
                      (concat "1" chopped) chopped)))
          (split (seq-map 'string-to-int (s-split "[dk]" ours)))
          (times (seq-elt split 0))
          (sides (ignore-errors (seq-elt split 1)))
          (keep (ignore-errors (seq-elt split 2)))
          (new-rolls))
    (if (not sides)
        (let ((rolls*
               (org-d20--rolls-concat sign rolls (int-to-string times))))
          (cons rolls* (+ total (* sign times))))
      (while (> times 0)
        (let ((new-roll (1+ (random (- sides 1)))))
          (push new-roll new-rolls))
        (setq times (- times 1)))
      (when keep
        ;; TODO this should drop the items without reordering the list
        (setq new-rolls (seq-drop (sort new-rolls '<) (- times keep))))
      (dolist (new-roll new-rolls)
        (setq rolls (org-d20--rolls-concat sign rolls
                                           (org-d20--rolls-bracket sides new-roll)))
        (setq total (+ total (* sign new-roll))))
      (cons rolls total))))

(defun org-d20--rolls-concat (sign a b)
  (if (>= sign 0)
      (if (s-blank? a)
          b
        (concat a " + " b))
    (if (s-blank? a)
        (concat "- " b)
      (concat a " - " b))))

(defun org-d20--rolls-bracket (sides roll)
  (let ((roll* (int-to-string roll)))
    (cond ((= sides 4)
           (concat "‹" roll* "›"))
          ((= sides 6)
           (concat "|" roll* "|"))
          ((= sides 8)
           (concat "/" roll* "/"))
          ((= sides 10)
           (concat "{" roll* "}"))
          ((= sides 12)
           (concat "⟨" roll* "⟩"))
          ((= sides 20)
           (concat "(" roll* ")"))
          ((= sides 100)
           (concat "«" roll* "»"))
          (t
           (concat "[" roll* "]")))))

(defun org-d20-initiative ()
  "Generates an Org-mode table with initiative order and monster/NPC HP."
  (interactive "*")
  (let ((rows))
    (let (name-input init-input hd-input num-input (monster 1))
      (loop
       do (setq name-input (read-string "Monster/NPC name (blank when done): "))
       (when (> (length name-input) 0)
         (setq init-input (read-string (concat name-input "'s init modifier: "))
               hd-input (read-string (concat name-input "'s hit points: "))
               num-input (string-to-int
                          (read-string (concat "How many " name-input "? "))))
         ;; in 5e, all monsters of the same kind have the same
         ;; initiative
         (let ((init (int-to-string
                      (org-d20--roll (concat
                                      "1d20"
                                      (org-d20--num-to-term init-input)))))
               (monsters-left num-input))
           (while (>= monsters-left 1)
             (let ((hp (int-to-string (org-d20--roll hd-input))))
               (push (list
                      "" (concat name-input
                                 " "
                                 (org-d20--monster-number monster))
                      (org-d20--num-to-term init-input) init hp "0")
                     rows))
             (setq monsters-left (1- monsters-left)
                   monster (1+ monster)))))
       (unless org-d20-continue-monster-numbering (setq monster 1))
       while (-all? (lambda (x) (> (length x) 0))
                    (list name-input init-input hd-input))))
    (dolist (pc org-d20-party)
      (let ((init (read-string (concat (car pc) "'s initiative roll: "))))
        (push (list "" (car pc) (org-d20--num-to-term (cdr pc)) init "-" "-")
              rows)))
    ;; we prepended each new item to the list, so reverse before
    ;; printing.  This ensures that the numbering/lettering of
    ;; monsters on the same initiative count is ascending
    (setq rows (seq-reverse rows))
    (insert
     "Round of combat: 1\n|Turn|Creature|Mod|Init|HP|Damage|Status|\n|-\n")
    (dolist (row rows)
      (dolist (cell row)
        (insert "|" cell))
      (insert "|\n"))
    (delete-char -1)
    (org-table-goto-column 4)
    (org-table-sort-lines nil ?N)
    (org-table-goto-line 2)
    (org-table-goto-column 1)
    (insert ">>>>")                     ; four chars in 'Turn'
    (org-table-align)))

(defun org-d20-initiative-advance ()
  "Advance the turn tracker in an initiative table."
  (interactive "*")
  (when (org-at-table-p)
    (loop
     do (let* ((back (search-backward ">>>>" (org-table-begin) t))
               (forward (search-forward ">>>>" (org-table-end) t))
               (cur (if back back forward)))
          (goto-char cur)
          (skip-chars-backward ">")
          (delete-char 4)
          (if (save-excursion (org-table-goto-line (1+ (org-table-current-line))))
              (progn
                (forward-line 1)
                (org-table-next-field)
                (insert ">>>>"))
            (save-excursion
              (search-backward "Round of combat:")
              (search-forward-regexp "[0-9]+")
              (skip-chars-backward "0-9")
              (replace-match
               (int-to-string (1+ (string-to-int (match-string 0))))))
            (org-table-goto-line 2)
            (insert ">>>>")))
     while (save-excursion
             (org-table-goto-column 2)
             (looking-at "~"))))
  (org-table-align))

(defun org-d20-damage (dmg)
  "Apply damage to the monster/NPC in the initiative table row at point."
  (interactive "*nDamage dealt: ")
  (when (org-at-table-p)
    (org-table-goto-column 6)
    (skip-chars-forward " ")
    (when (looking-at "[0-9]+")
      (let ((total-damage (+ dmg (string-to-int (match-string 0)))))
        (replace-match (int-to-string total-damage))
        (save-excursion
          (org-table-goto-column 5)
          (skip-chars-forward " ")
          (when (looking-at "[0-9]+")
            (let ((max-hp (string-to-int (match-string 0))))
              (if (>= total-damage max-hp)
                  (progn
                    (org-table-goto-column 2)
                    (insert "~")
                    (org-table-end-of-current-cell-content)
                    (insert "~"))
                (when (>= total-damage (/ max-hp 2))
                  (org-table-goto-column 7)
                  (org-table-end-of-current-cell-content)
                  (unless (looking-back "bloodied")
                    (unless (looking-back "|")
                      (insert "; "))
                    (insert "bloodied")))))))))
    (org-table-align)))

(defun org-table-end-of-current-cell-content ()
  (interactive)
  (search-forward "|" (save-excursion (end-of-line) (point)))
  (forward-char -2)
  (skip-chars-backward " "))

(defun org-d20-roll (exp)
  "Prompt, evaluate and display dice roll expression EXP.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (interactive "sRoll: ")
  (setq org-d20-roll--last exp)
  (-let* (((rolls . result) (org-d20--roll exp))
          (result* (int-to-string result)))
    ;; if `rolls' contains no spaces then we just rolled a single
    ;; dice, so don't show the intermediate calculation
    (if (s-contains? " " rolls)
        ;; if the frame is not wide enough to show the full result,
        ;; strip out the spaces in the hope that it will fit
        (let ((rolls-display (if (>
                                  (+ (length exp) 3 (length rolls) 3 (length result*))
                                  (frame-width))
                                 (s-replace " " "" rolls)
                               rolls)))
          (message "%s = %s = %s" exp rolls-display result*))
      (message "%s = %s" exp (int-to-string result))))
  (when org-d20-dice-sound
    (play-sound-file org-d20-dice-sound)))

(defun org-d20-roll-last ()
  (interactive)
  (if (boundp 'org-d20-roll--last)
      (org-d20-roll org-d20-roll--last)
    (call-interactively 'org-d20-roll)))

(defun org-d20-d20 ()
  "Roll two d20, showing result with advantage and disadvantage, and with neither."
  (interactive)
  (let* ((fst (cdr (org-d20--roll "1d20")))
         (snd (cdr (org-d20--roll "1d20")))
         (fst* (int-to-string fst))
         (snd* (int-to-string snd))
         (adv (if (>= fst snd)
                  (concat (propertize fst* 'face 'bold) "  " snd*)
                (concat fst* "  " (propertize snd* 'face 'bold))))
         (disadv (if (<= fst snd)
                     (concat (propertize fst* 'face 'bold) "  " snd*)
                   (concat fst* "  " (propertize snd* 'face 'bold)))))
    (message "No adv./disadv.:  %s\tAdv.:  %s\tDisadv.:  %s"
             fst* adv disadv))
  (when org-d20-dice-sound
    (play-sound-file org-d20-dice-sound)))

(defun org-d20-d% ()
  "Roll a percentile dice."
  (interactive)
  (org-d20-roll "1d100"))

(defun org-d20-initiative-dwim ()
  "Start a new combat or advance the turn tracker, based on point."
  (interactive "*")
  (if (org-at-table-p)
      (org-d20-initiative-advance)
    (org-d20-initiative)))

(defun org-d20-initiative-add ()
  "Add a monster to an existing combat."
  (interactive "*")
  (if (org-at-table-p)
      (let* ((name-input (read-string "Monster/NPC name: "))
             (init-input (read-string (concat name-input "'s init modifier: ")))
             (hd-input (read-string (concat name-input "'s hit points: ")))
             (num-input (string-to-int
                         (read-string (concat "How many " name-input "? "))))
             (monster 1))
        ;; first, if we need to, try to count the number of monsters.
        ;; We can only use a crude heuristic here because we don't
        ;; know what kind of things the user might have added to the
        ;; table
        (when org-d20-continue-monster-numbering
          (save-excursion
            (org-table-goto-line 1)
            (while (org-table-goto-line (1+ (org-table-current-line)))
              (org-table-goto-column 2)
              (when (looking-at "[^|]+ \\([A-Z]\\|[0-9]+\\)~? *|")
                (setq monster (1+ monster))))))
        (save-excursion
          ;; ensure we're not on header row (following won't go past end
          ;; of table)
          (org-table-goto-line (1+ (org-table-current-line)))
          (org-table-goto-line (1+ (org-table-current-line)))
          (let ((init (int-to-string
                       (cdr (org-d20--roll (concat
                                            "1d20"
                                            (org-d20--num-to-term init-input))))))
                (monsters-left num-input))
            (while (>= monsters-left 1)
              ;; open a new row and then immediately move it downwards
              ;; to ensure that the monsters on the same initiative
              ;; count are numbered/lettered in ascending order
              (org-table-insert-row)
              (org-table-move-row)
              (org-table-next-field)
              (insert name-input)
              (insert " ")
              (insert (org-d20--monster-number monster))
              (org-table-next-field)
              (insert (org-d20--num-to-term init-input))
              (org-table-next-field)
              (insert init)
              (org-table-next-field)
              (insert (int-to-string (cdr (org-d20--roll hd-input))))
              (org-table-next-field)
              (insert "0")
              (setq monsters-left (1- monsters-left)
                    monster (1+ monster))))
          (org-table-goto-column 4)
          (org-table-sort-lines nil ?N)
          (org-table-align)))
    (org-d20-initiative)))

(defun org-d20--num-to-term (n)
  (let ((k (if (stringp n) (string-to-int n) n)))
    (if (>= k 0)
        (concat "+" (int-to-string k))
      (int-to-string k))))

(defun org-d20--monster-number (n)
  (if (and org-d20-letter-monsters (>= 26 n))
      (seq-elt
       (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
             "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
       (- n 1))
    (int-to-string n)))

(defun org-d20-roll-at-point ()
  "Roll the dice expression at point and display result in minibuffer."
  (interactive)
  (let ((exp (thing-at-point 'sexp t)))
    (org-d20-roll exp)))

;;;###autoload
(define-minor-mode org-d20-mode
  "Bind convenience functions for running a d20-like game in an
Org-mode document."
  :lighter " d20")

(provide 'org-d20)
;;; org-d20.el ends here
