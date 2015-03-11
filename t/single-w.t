# -*- perl -*-
use lib 't';
use strict;
use TestRegexp;

TestRegexp(input => [ 's' ],
           nomatch => [ 's1' ],
           type  => 'pcre',
	   word  => 1);

	   