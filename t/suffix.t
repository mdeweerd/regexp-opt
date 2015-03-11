# -*- perl -*-
use lib 't';
use strict;
use TestRegexp;

TestRegexp(input => [ 'afoo', 'foo', 'efoo' ],
           nomatch => [ 'bfoo' ],
	   re => '\b(?:(?:[ae])?foo)\b',
           type  => 'pcre',
	   word  => 1);

