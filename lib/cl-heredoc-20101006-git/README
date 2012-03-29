cl-heredoc - Common Lisp reader heredoc dispatcher
==================================================
cl-heredoc is an implementation of "here documents" [1] that allow the user to
embed literal strings into code or data without any need for quoting, something
that is missing in both ANSI CL and popular implementations.

cl-heredoc is completely written in Common Lisp and licensed under the
GPLv3+[2]. Please see the file COPYING in the top-level directory of the
distribution tarball or see the link at [2] if you didn't receive a copy along
with this file.


Usage
=====
cl-heredoc primarily exports a single function, READ-HEREDOC, that is meant to
be used with the CL reader by attaching it to a dispatch character, e.g.

CL-USER> (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
CL-USER> #>eof>Write whatever (you) "want",
  no matter how many lines or what characters until
the magic end sequence has been reached!eof

Will read everything starting after "#>eof>" until the given sequence, here
"eof", is found and return the result as a string. The enclosed text is not
subject to any kind of evaluation and needs no quoting.
If used in a non-interactive fashion, i.e. outside the REPL, an error will be
signaled if EOF is reached.

The underlying function READ-UNTIL-MATCH may also be used directly: It takes two
arguments, a stream and a termination string and will read from the stream until
the terminating sequence is found. Everything read so far excluding the
terminator is returned as a string.


Links and References
====================
Homepage: http://www.cliki.net/cl-heredoc
Hacking:  http://github.com/e-user/cl-heredoc

[1] http://en.wikipedia.org/wiki/Here_document
[2] http://www.gnu.org/licenses/gpl-3.0-standalone.html


--------------------------------------------------------------------------------
Copyright (C) 2009, 2010  Alexander Kahl <e-user@fsfe.org>
This file is part of cl-heredoc.
cl-heredoc is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

cl-heredoc is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
