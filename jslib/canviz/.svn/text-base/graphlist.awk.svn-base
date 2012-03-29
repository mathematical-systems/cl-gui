#
# This file is part of Canviz. See http://www.canviz.org/
# $Id$
#

{
	print "/*"
	print " * This file is part of Canviz. See http://www.canviz.org/"
	print " */"
	print ""
	print "var graphs = ["
	for (i = 1; i <= NF; ++i) {
		line = "'" $i ".txt'"
		if (i != NF) {
			line = line ","
		}
		print line
	}
	print "];"
}
