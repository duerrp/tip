tip
===

Tip is a very simple terminal line note pad. It can help you take and retrieve
notes, e.g., about some common command line options for a particular tool -
right from the terminal. Compared to other similar tools, tip has two main
advantages: 1. It uses gpg to encrypt your notes (which means you can put them
in a version control system without being worried about the contents of your
notes) and 2. it uses pygments to syntax highlight your notes.

If you find any problem, feel free to start a discussion on the `issues
<https://github.com/duerrp/tip/issues>`__ page.

Usage
-----

To create a new note (or edit an existing one), use ``tip -e NAME``. Tip will
open the note in your EDITOR (note that your EDITOR should be able to handle gpg
encrypted files).

To read a note, run ``tip NAME``.

Tip will put your notes in the folder specified by the TIP_DIRECTORY environment
variable (or in ~/.tip/ by default).

Installation
------------

Tip can be compiled and installed using stack by issuing ``stack setup`` and
``stack install``. Make sure you have gpg as well as pygments installed on your
system.

License
-------

The tip package is licensed under a BSD3 licence (see the
`LICENSE <https://github.com/duerrp/tip/blob/master/LICENSE>`__).
