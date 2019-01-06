0.3 (unreleased)
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
- Refactoring/cleanup

0.2 (2018-12-31)
----------------

- First public release.
- Remove hardcoded path to my dice rolling wav.
- Add org-d20-roll-at-point.

0.1 (2017)
----------

Version imported from my dotfiles repository.
