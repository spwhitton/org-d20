# org-d20 #

## Description ##

org-d20 is a minor mode for Emacs' [Org-mode](https://orgmode.org/)
for GMs running games whose rules centre around rolling d20s.  It
should be useful for *Dungeons and Dragons* 3rd, 4th and 5th editions,
Paizo's *Pathfinder*, and
[d20 System](https://en.wikipedia.org/wiki/D20_System) games like *d20
Modern*.

The idea is that you're already keeping your campaign notes in an
Org-mode file.  This minor mode does useful things for you while you
are visiting that buffer.  For example,

- rolling dice, with 5e's advantage and disadvantage displayed in case
  those are needed;

- tracking combat turns and rounds right next to your existing
  description of the monsters and the terrain.
  
The minor mode's defaults suit the way that I run D&D 5e.  Patches to
add `defcustoms` to make org-d20 more suitable for other games, and
other sets of house rules, are welcome.

## Installation ##

Presently manual.  I hope to make org-d20 available from a popular
online repository soon.

## Usage ##

It's useful to activate the mode automatically when you open the
Org-mode file in which you are keeping your campaign notes.  You can
also specify the names of your party members, and their initiative
modifiers, if you want to use org-d20's combat tracker.

You can either end the Org-file with a footer like this:

     # Local Variables:
     # mode: org
     # mode: org-d20
     # org-d20-party: (("Zahrat" . 2) ("Ennon" . 4) ("Artemis" . 5))
     # End:

or start it with a first line like this:

    # -*- mode: org; mode: org-d20; org-d20-party: (("Zahrat" . 0) ("Anca" . 1)) -*-

Then close and reopen the file.  Emacs will probably ask you to
confirm that the value for org-d20-party that you've supplied is safe.

The following bindings should then be available:

- <kbd>&gt;f9></kbd> -- start a combat, or advance the turn/round counter
  for an existing combat, depending on point
- <kbd>S-&gt;f9></kbd> -- add new monsters to an existing combat
- <kbd>&gt;f10></kbd> -- apply damage to the monster at point
- <kbd>S-&gt;f10></kbd> -- evaluate the dice expression (e.g. `4d10`) at
  point
- <kbd>&gt;f11></kbd> -- prompt for dice expression and evaluate it
- <kbd>S-&gt;f11></kbd> -- roll the last dice expression again
- <kbd>&gt;f12></kbd> -- roll d20, with advantage and disadvantage shown
- <kbd>S-&gt;f12></kbd> -- roll percentile dice

## Bugs/patches ##

Please report bugs and submit patches by e-mail to
`<spwhitton@spwhitton.name>`.

## License ##

Copyright (C) 2017-2018  Sean Whitton

org-d20 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

org-d20 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with org-d20.  If not, see
[<http://www.gnu.org/licenses/>](http://www.gnu.org/licenses/).
