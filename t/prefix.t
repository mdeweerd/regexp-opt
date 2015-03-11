# -*- perl -*-
use lib 't';
use strict;
use TestRegexp;

TestRegexp(input => [ 'afoo', 'afquz', 'afbar' ],
           nomatch => [ 'afbarba' ],
           type  => 'pcre',
	   word  => 1);
