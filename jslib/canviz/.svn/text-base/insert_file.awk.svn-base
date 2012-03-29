#
# This file is part of Canviz. See http://www.canviz.org/
# $Id$
#

BEGIN {
	replacement = ""
}

NR==FNR {
	gsub("\&", "\\\\\&", $0)
	replacement = replacement $0 "\n";
	next;
}

{
	gsub(placeholder, replacement, $0)
	print
}
