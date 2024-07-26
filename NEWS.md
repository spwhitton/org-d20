0.6 (2024-07-26)
----------------

- Add org-mode to org-d20-initiative-add interactive spec.
- Ensure we read numbers for initiative modifiers.
- Some refactoring.

0.5 (2021-02-11)
----------------

- Add missing (require 'subr-x).
- Note apt-get installation option in README.md.

0.4 (2020-12-29)
----------------

- New defcustom, `org-d20-display-rolls-buffer`.
- `org-d20-d20` shows the result of rolling a d4 in case /Bless/ is active.
- Replace some calls to `loop` with calls to `cl-loop`.
- Other code cleanup.

0.3 (2019-04-15)
----------------

- Bug fix: off-by-one error in call to `random`
  - This meant that the maximum value on a dice could never be
    rolled.  Now monsters can critically hit players again.
- Bug fix: make roll20's 'k' notation actually have an effect
- Change default keybindings to match Emacs conventions
  - Avoid binding to <f9>

- `org-d20-roll` shows the results on each dice rolled
- New defcustom, `org-d20-letter-monsters`
- New defcustom, `org-d20-continue-monster-numbering`
- You can now specify a dice expression for the number of a type of
  monster/NPC

- Improve README for new users
- Autoload some more functions users might want to bind
- defcustoms now specify their types, making the customisation
  interface more useful
- Refactoring/cleanup

0.2 (2018-12-31)
----------------

- First public release.
- Remove hardcoded path to my dice rolling wav.
- Add org-d20-roll-at-point.

0.1 (2017)
----------

Version imported from my dotfiles repository.
