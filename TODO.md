Soon
----

- Dice expression roller shows results for each die
- Screenshot from my live notes
- Page on my website
- [Submit to MELPA](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org#making-your-package-ready-for-inclusion)

Features
--------

- defcustom to roll each monster's initiative separately (as in 3e),
  rather than having type of monster act at the same time (as in 5e)
- For number of monsters, accept a dice expession (e.g. 1d4 Gas
  Spores)

Possible refactoring
--------------------

Rewrite combat to use a data strcuture holding all data from the table
with functions to read, write and update an Org table.  Then
e.g. taking damage is done by reading table, updating struct and
writing it back to the buffer

At the very least, org-d20-initiative should call
org-d20-initiative-add or similar code reuse.
