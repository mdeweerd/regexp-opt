package String::Regexp;

use strict;
use Carp;
use Data::Dumper;

require Exporter;
our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(array_to_regexp) ] );
our @EXPORT_OK = ( qw(array_to_regexp) );
our @EXPORT = qw(array_to_regexp);
our $VERSION = "1.00";

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
	for (my $i = 0; $i < $n; $i++) {
	    if ($j <= $#{$aref->[$i]}) {
		unless ($j <= $#{$aref->[$i+1]} and
			$aref->[$i+1][$j] eq $aref->[$i][$j]) {
		    $n = $i;
		    last;
		}
	    }
	}
	$k = $j++;
    }
    $n = 0 if ($k == -1);
    return ($n,$k);
}

use constant T_ALT => 0x0;
use constant T_PFX => 0x1;
use constant T_SFX => 0x2;
use constant T_MASK => 0xf;

use constant T_OPT => 0x10;

sub regexp_opt {
    my @t = @_;
    my @output;
    return [] if $#t == -1;
    push @output;
    while (1) {
	my @res = split_prefix \@t;
	if ($res[1] <= 0) {
	    my @rv = map { [ reverse @{$_} ] } @t;
	    @res = split_prefix \@rv;
	    if ($res[1] > 0) {
		my @x = reverse @{$rv[0]}[0..$res[1]];
		my $sfxlen = $#x;
		my $sfx = join('', @x);
		my $type = T_SFX;
		my $prefixes = regexp_opt(map { my @r = @{$_};
						if ($sfxlen == $#r) {
						    $type |= T_OPT;
						    ();
						} else {
						    [ @r[0..$#r-$sfxlen-1] ];
						}
					  } @t[0..$res[0]]);
		push @output, [ $type, $sfx, $prefixes ];
	    } else {
		push @output, map { join('', @{$_}) } @t[0..$res[0]];
	    }
	} elsif ($res[0] == 0) {
	    push @output, join('', @{$t[0]});
	} else {
	    my @x = @{$t[0]}[0..$res[1]];
	    my $pfxlen = $#x;
	    my $pfx = join('', @x);
	    my $type = T_PFX;
	    my $suffixes = regexp_opt(map { my @r = @{$_};
					    if ($pfxlen == $#r) {
						$type |= T_OPT;
						();
					    } else {
						[ @r[$pfxlen+1..$#r] ];
					    }
				      } @t[0..$res[0]]);
	    push @output, [ $type, $pfx, $suffixes ];
	}
	last if $res[0] == $#t;
	@t = @t[($res[0]+1)..$#t];
    }
    return \@output;
}

sub posix_build_opt {
    my $s = shift;
    my $delim;
    
    $$s .= '(' if ($#_ > 0);
    foreach my $elt (@_) {
	$$s .= $delim if defined $delim;
	if (ref($elt) eq 'ARRAY') {
	    trans_posix_recursive($elt, $s);
	} else {
	    $$s .= "($elt)";
	}
	$delim = '|';
    }
    $$s .= ')' if ($#_ > 0);
}

sub trans_posix_recursive {
    my ($treeref, $s) = @_;
    my @tree = @{$treeref};
    my $delim;

    my $mode = shift @tree;
    my $type = $mode & T_MASK;
    if ($type == T_ALT) {
	posix_build_opt($s, @tree);
	$$s .= '?' if ($mode & T_OPT); # FIXME
    } elsif ($type == T_PFX) {
	$$s .= '('.shift(@tree);
	posix_build_opt($s, @{$tree[0]});
	$$s .= '?' if ($mode & T_OPT);
	$$s .= ')';	
    } elsif ($type == T_SFX) {
	my $sfx = shift(@tree);
	$$s .= '(';
	posix_build_opt($s, @{$tree[0]});
	$$s .= '?' if ($mode & T_OPT);
	$$s .= "$sfx)";	
    } else {
	croak "unrecognized element type";
    }
    return $$s;
}

sub trans_posix {
    my ($tree, $opts) = @_;
    my $s = '';
    trans_posix_recursive($tree, \$s);
    $s = "\\<$s\\>" if $opts->{word};
    return $s;
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
    unshift @{$res}, T_ALT;
    print Dumper($res) if ($opts->{debug});
    return &{$trans}($res, $opts);
}

1;



