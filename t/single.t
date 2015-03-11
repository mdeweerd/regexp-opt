# -*- perl -*-
use lib 't';
use strict;
use TestRegexp;

TestRegexp(input => [ 's' ],
           match => [ 's1', 'asa' ], 
           nomatch => [ 'a' ],
           type  => 'pcre',
	   word  => 0);
	   