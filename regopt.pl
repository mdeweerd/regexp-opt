use strict;
use Data::Dumper;

# split_prefix(ARRAY)
sub split_prefix {
    my $aref = shift;

    my $n = $#{$aref};
    my $j = 0;
    my $k = -1;
    while ($n > 0 and
	   $j <= $#{$aref->[0]} and
	   $j <= $#{$aref->[1]} and
	   $aref->[1][$j] eq $aref->[0][$j]) {
#	print "Start iteration\n";
	for (my $i = 0; $i < $n; $i++) {
#	    print "($i,$j) $aref->[$i+1][$j] <=> $aref->[$i][$j]\n";
	    if ($j <= $#{$aref->[$i]}) {
#		print "A:".($j <= $#{$aref->[$i+1]})."\n";
#		print "B:".($aref->[$i+1][$j] eq $aref->[$i][$j])."\n";
		unless ($j <= $#{$aref->[$i+1]} and
			$aref->[$i+1][$j] eq $aref->[$i][$j]) {
#		    print "($i,$j): ". Dumper($aref->[$i]) . " stop\n";
		    $n = $i;
		    last;
		}
	    }
	}
	$k = $j++;
#	print "End iteration: $n, $j\n";
    }
#    print "res $n,$k\n";
    $n = 0 if ($k == -1);
    return ($n,$k);
}


##########
my @input = (
    'abab',
    'ac',
    'abba',
    'abbaab',
    'abbaabab',
    'ba',
    'bb',
    'babab',
    );
# my @ainput = ('abcd','e');
print join("\n", sort @input)."\n\n";
# my @t = map { my @x = split //, $_; \@x } sort @input;
# print Dumper(\@t);
# my @x = split_prefix \@t;
# print "@x\n";
# exit;

sub dprint {
    my $res = shift;
    my $p = shift;
    print "$p " if defined $p;
    foreach my $x (@{$res}) {
	print "$x->[0]";
	if (defined($x->[1])) {
	    print " ";
	    print Dumper($x);
	}
	print "\n";
    }
}

#my $level;
sub regexp_opt {
    my @t = @_;
    my @output;
#    ++$level;
#    print "$level IN ".Dumper(\@t)."\n";
    return [] if $#t == -1;
    while (1) {
#	print "$level NEXT $#t ".Dumper(\@t)."\n";
	my @res = split_prefix \@t;
#	print "$level R @res, $#t\n";
	if ($res[0] == 0) {
	    push @output, join('', @{$t[0]}) ;
	} elsif ($res[1] <= 0) {
	    push @output, @t[0..$res[0]];
	} else {
	    my @x = @{$t[0]}[0..$res[1]];
	    push @output, [ join('', @x), 
			    regexp_opt(map { my @r = @{$_};
					     my @a = @r[$#x+1..$#r];
#					     print "A @r -- @a\n";
					     \@a
				       } @t[0..$res[0]]) ];
#	    print "==\n";
	}
#	print "$level T=$#t\n";
	last if $res[0] == $#t;
	@t = @t[($res[0]+1)..$#t];
    }
#    print "$level OUT ".Dumper(\@output)."\n";
#    --$level;
    return \@output;
}

my @t = map { my @x = split //, $_; \@x } sort @input;
my $res = regexp_opt(@t);
#dprint($res,"RESULT");



