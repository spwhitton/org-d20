;;; spwd20.el --- minor mode for d20 tabletop roleplaying games

;; Copyright (C) 2017  Sean Whitton

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; URL:
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: (s seq dash)
;; Keywords:

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
;;; keeping your GM notes for a d20 game.

;;; Example file footer:
;;;
;;;     # Local Variables:
;;;     # eval: (spwd20-mode 1)
;;;     # spwd20-party: (("Zahrat" . 2) ("Ennon" . 4) ("Artemis" . 5))
;;;     # End:
;;;
;;; Example first line of file:
;;;
;;;     # -*- mode: org; mode: spwd20; spwd20-party: (("Zahrat" . 0) ("Anca" . 1)) -*-

;;; TODO dice expression roller shows results for each die

;;; TODO rewrite combat to use a data strcuture holding all data from
;;; the table with functions to read, write and update an Org table.
;;; Then e.g. taking damage is done by reading table, updating struct
;;; and writing it back to the buffer

;;; Code:

(require 's)
(require 'seq)
(require 'dash)

(defcustom spwd20-party nil
  "Party initiative modifiers.")

(defcustom spwd20-dice-sound
  "~/lib/annex/doc/sounds/147531__ziembee__diceland.wav"
  "Path to a sound file that `play-sound-file' can play.")

(defvar spwd20-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f9>") 'spwd20-initiative-dwim)
    (define-key map (kbd "S-<f9>") 'spwd20-initiative-add)
    (define-key map (kbd "<f10>") 'spwd20-damage)
    (define-key map (kbd "<f11>") 'spwd20-roll)
    (define-key map (kbd "S-<f11>") 'spwd20-roll-last)
    (define-key map (kbd "<f12>") 'spwd20-d20)
    (define-key map (kbd "S-<f12>") 'spwd20-d%)
    map)
  "Keymap for `spwd20-mode'.")

(defun spwd20--roll (exp)
  "Evaluate dice roll expression EXP.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (let ((exps (seq-map (lambda (s) (s-chop-prefix "+" s))
                       (s-slice-at "[+-]" exp))))
    (-sum (seq-map 'spwd20--roll-inner exps))))

(defun spwd20--roll-inner (exp)
  (let* ((sign (if (s-prefix-p "-" exp) -1 1))
         (ours (let ((chopped (s-chop-prefix "-" exp)))
                 (if (string= (substring chopped 0 1) "d")
                     (concat "1" chopped) chopped)))
         (split (seq-map 'string-to-int (s-split "[dk]" ours)))
         (times (seq-elt split 0))
         (sides (ignore-errors (seq-elt split 1)))
         (keep (ignore-errors (seq-elt split 2)))
         (rolls))
    (* sign
       (if (not sides)
           times
         (while (> times 0)
           (let ((roll (1+ (random (- sides 1)))))
             (push roll rolls))
           (setq times (- times 1)))
         (-sum
          (if keep
              (seq-drop (sort rolls '<) (- times keep))
            rolls))))))

(defun spwd20-initiative ()
  "Generates an Org-mode table with initiative order and monster HP."
  (interactive "*")
  (let ((rows))
    (let (name-input init-input hd-input num-input)
      (loop
       do (setq name-input (read-string "Monster/NPC name (blank when done): "))
       (when (> (length name-input) 0)
         (setq init-input (read-string (concat name-input "'s init modifier: "))
               hd-input (read-string (concat name-input "'s hit points: "))
               num-input (string-to-int
                          (read-string (concat "How many " name-input "? "))))
         ;; in 5e, all monsters of the same kind have the same
         ;; initiative
         ;; TODO defcustom to toggle this for other editions
         (let ((init (int-to-string
                      (spwd20--roll (concat
                                     "1d20"
                                     (spwd20--num-to-term init-input)))))
               (monster num-input))
           (while (>= monster 1)
             (let ((hp (int-to-string (spwd20--roll hd-input))))
               (push (list
                      "" (concat name-input " " (int-to-string monster))
                      (spwd20--num-to-term init-input) init hp "0")
                     rows))
             (setq monster (1- monster)))))
       while (-all? (lambda (x) (> (length x) 0))
                    (list name-input init-input hd-input))))
    (dolist (pc spwd20-party)
      (let ((init (read-string (concat (car pc) "'s initiative roll: "))))
        (push (list "" (car pc) (spwd20--num-to-term (cdr pc)) init "-" "-")
              rows)))
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

(defun spwd20-initiative-advance ()
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

(defun spwd20-damage (dmg)
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

(defun spwd20-roll (exp)
  "Prompt, evaluate and display dice roll expression EXP.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (interactive "sRoll: ")
  (setq spwd20-roll--last exp)
  (message "%s = %s" exp (int-to-string (spwd20--roll exp)))
  (when spwd20-dice-sound
    (play-sound-file spwd20-dice-sound)))

(defun spwd20-roll-last ()
  (interactive)
  (if (boundp 'spwd20-roll--last)
      (spwd20-roll spwd20-roll--last)
    (call-interactively 'spwd20-roll)))

(defun spwd20-d20 ()
  "Roll two d20, showing result with advantage and disadvantage, and with neither."
  (interactive)
  (let* ((fst (spwd20--roll "1d20"))
         (snd (spwd20--roll "1d20"))
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
  (when spwd20-dice-sound
    (play-sound-file spwd20-dice-sound)))

(defun spwd20-d% ()
  "Roll a percentile dice."
  (interactive)
  (spwd20-roll "1d100"))

(defun spwd20-initiative-dwim ()
  "Start a new combat or advance the turn tracker, based on point."
  (interactive "*")
  (if (org-at-table-p)
      (spwd20-initiative-advance)
    (spwd20-initiative)))

(defun spwd20-initiative-add ()
  "Add a monster to an existing combat."
  (interactive "*")
  (if (org-at-table-p)
      (let* ((name-input (read-string "Monster/NPC name: "))
             (init-input (read-string (concat name-input "'s init modifier: ")))
             (hd-input (read-string (concat name-input "'s hit points: ")))
             (num-input (string-to-int
                         (read-string (concat "How many " name-input "? ")))))
        (save-excursion
          ;; ensure we're not on header row (following won't go past end
          ;; of table)
          (org-table-goto-line (1+ (org-table-current-line)))
          (org-table-goto-line (1+ (org-table-current-line)))
          (let ((init (int-to-string
                       (spwd20--roll (concat
                                      "1d20"
                                      (spwd20--num-to-term init-input)))))
                (monster num-input))
            (while (>= monster 1)
              (org-table-insert-row)
              (org-table-next-field)
              (insert name-input)
              (insert " ")
              (insert (int-to-string monster))
              (org-table-next-field)
              (insert (spwd20--num-to-term init-input))
              (org-table-next-field)
              (insert init)
              (org-table-next-field)
              (insert (int-to-string (spwd20--roll hd-input)))
              (org-table-next-field)
              (insert "0")
              (setq monster (1- monster))))
          (org-table-goto-column 4)
          (org-table-sort-lines nil ?N)
          (org-table-align)))
    (spwd20-initiative)))

(defun spwd20--num-to-term (n)
  (let ((k (if (stringp n) (string-to-int n) n)))
    (if (>= k 0)
        (concat "+" (int-to-string k))
      (int-to-string k))))

;;;###autoload
(define-minor-mode spwd20-mode
  "Bind convenience functions for running a d20-like game in an
Org-mode document."
  :lighter " d20")

(provide 'spwd20)
;;; spwd20.el ends here
