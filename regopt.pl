use strict;
use Carp;
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

sub regexp_opt {
    my @t = @_;
    my @output;
    return [] if $#t == -1;
    while (1) {
	my @res = split_prefix \@t;
	if ($res[0] == 0) {
	    push @output, join('', @{$t[0]}) ;
	} elsif ($res[1] <= 0) {
	    push @output, @t[0..$res[0]];
	} else {
	    my @x = @{$t[0]}[0..$res[1]];
	    push @output, [ join('', @x), 
			    regexp_opt(map { my @r = @{$_};
					     my @a = @r[$#x+1..$#r];
					     \@a
				       } @t[0..$res[0]]) ];
	}
	last if $res[0] == $#t;
	@t = @t[($res[0]+1)..$#t];
    }
    return \@output;
}

sub trans_posix_recursive {
    my ($treeref, $s) = @_;
    my @tree = @{$treeref};
    my $delim;
    while ($#tree >= 0) {
	my $node = shift @tree;
	$$s .= $delim if defined($delim);
	if (ref($node) eq 'ARRAY') {
	    $$s .= '(';
	    trans_posix_recursive($node, $s);
	    $$s .= ')';
	} else {
	    ${$s} .= "($node)";
	}
	$delim = '|';
    }
    return $$s;
}

sub trans_posix {
    my ($tree, $opts) = @_;
    return trans_posix_recursive($tree);
}

my %transtab = (
    posix => \&trans_posix
);

sub array_to_regexp {
    my $trans = \&trans_posix;
    my $opts;

    if (ref($_[0]) eq 'HASH') {
	$opts = shift;
    }

    if (defined($opts->{type})) {
	$trans = $transtab{$opts->{type}};
	croak "unsupported type: $opts->{type}"
	    unless defined $trans;
    }
    
    my @t = map { my @x = split //, $_; \@x } sort @_;
    my $res = regexp_opt(@t);
    print Dumper($res) if ($opts->{debug});
    return &{$trans}($res, $opts);
}

my $s = array_to_regexp({ debug => 1 }, @input);
print "$s\n";



