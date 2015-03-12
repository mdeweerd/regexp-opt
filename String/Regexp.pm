# -*- perl -*-
# Copyright (C) 2015 Sergey Poznyakoff <gray@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

# Synopsis:
#   my @res = split_prefix(ARRAY)
# Arguments:
#   ARRAY is a sorted array of char array references.
# Description:
#   Find N first elements of ARRAY sharing the longest prefix (of length L).
#   In other words, find N and L such that ARRAY[N][L+1] != ARRAY[N][L+1].
# Return value:
#   (N, L)
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

# Each node of the parse tree is a list.  Its 0th element keeps the type of
# the node.  Its lowest byte is one of the following:

# Rest of the node is a list of alternatives.
use constant T_ALT => 0x0;

# A prefixed list of alternatives.  Element 1 is the prefix string and
# element 2 is a reference to the list.
use constant T_PFX => 0x1;
# A suffixed list of alternatives.  Element 1 is the suffix string and
# element 2 is a reference to the list.
use constant T_SFX => 0x2;

# This mask is used to get the node type:
use constant T_MASK => 0xf;

# If the type is ORed with T_OPT, the element is optional.
use constant T_OPT => 0x10;

# Synopsis:
#   my @list = regexp_opt(ARRAY)
# Arguments:
#   ARRAY is a sorted array of char array references.
# Description:
#   Recursively parse the array of arguments and return a parse tree.
sub regexp_opt {
    my @t = @_;
    my @output;
    return [] if $#t == -1;
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

# ###################################
# Convert parse tree to a regexp
#
# The functions below take hash and string reference as their first two
# arguments.
#
# The first argument is a reference to a configuration hash, which contains
# the following keys:
#
# rxchars: A Perl regular expression matching special characters, which should
# be escaped with a backslash on output:
#   posix [][\\<>.(){}?*^\$]
#   pcre  [][\\.(){}?*^\$]
#
# group:   A reference to a list of two elements containig markers for
# parenthesized groups.  Non-capturing groups are used, if possible.
#   posix [ '(', ')' ]
#   pcre  [ '(?:', ')' ]
#
# The second argument is a reference to a string where the generated
# expression will be stored.
# 
# ###################################

# Synopsis:
#   my $s = escape_re_chars(CONF, STRING)
# Arguments:
#   See above.
# Description:
#   Escape special characters in the STRING
# Return value:
#   Escaped string, safe to use in regular expressions.
sub escape_re_chars {
    my ($conf,$s) = @_;
    $s =~ s/($conf->{rxchars})/\\$1/g;
    return $s;
}

# Synopsis:
#   posix_build_opt(CONF, STRING, LIST...)
# Arguments:
#   CONF and STRING as described above.
#   LIST is a subtree.
# Description:
#   Convert subtree into regular expression.
sub posix_build_opt {
    my $conf = shift;
    my $s = shift;
    my $delim;
    my @cclass;
    
    $$s .= $conf->{group}[0] if ($#_ > 0);
    foreach my $elt (@_) {
	$$s .= $delim if defined $delim;
	if (ref($elt) eq 'ARRAY') {
	    trans_posix_recursive($conf, $s, $elt);
	    $delim = '|';
	} elsif (length($elt) == 1) {
	    push @cclass, $elt;
	} else {
	    $$s .= $conf->{group}[0] . escape_re_chars($conf, $elt) . $conf->{group}[1];
	    $delim = '|';
	}
    }

    if ($#cclass == 0) {
	$$s .= $delim if defined $delim;
	$$s .= $cclass[0];
    } elsif ($#cclass >= 0) {
	$$s .= $delim if defined $delim;
	$$s .= '[';
	@cclass = sort {
	    if ($a eq '[') {
		if ($b eq ']') {
		    return 1;
		} else {
		    return -1;
		}
	    } elsif ($b eq '[') {
		if ($b eq ']') {
		    return -1;
		} else {
		    return 1;
		}
	    } elsif ($a eq ']') {
		return -1;
	    } elsif ($b eq ']') {
		return 1;
	    } elsif ($a eq '-') {
		return 1;
	    } elsif ($b eq '-') {
		return -1;
	    } else {
		$a cmp $b;
	    }
	} @cclass;

	my $start = shift @cclass;
	my $end;
	while (my $c = shift @cclass) {
	    if (defined($end)) {
		if (ord($c) - ord($end) == 1) {
		    $end = $c;
		} else {
		    if (ord($end) - ord($start) > 1) {
			$$s .= "$start-$end";
		    } else {
			$$s .= "$start$end";
		    }
		    $start = $c;
		    $end = undef;
		}
	    } elsif (ord($c) - ord($start) == 1) {
 		$end = $c;
	    } else {
		$$s .= $start;
		$$s .= $end if defined $end;
		$start = $c;
		$end = undef;
	    }
	}

	if (defined($start)) {
	    $$s .= $start;
	    if (defined($end)) {
		if (ord($end) - ord($start) > 1) {
		    $$s .= "-$end";
		} else {
		    $$s .= $end;
		}
	    }
	}
	$$s .= ']';
    }
    
    $$s .= $conf->{group}[1] if ($#_ > 0);
}

# Synopsis:
#   trans_posix_recursive(CONF, STRING, TREE...)
# Arguments:
#   CONF and STRING as described above.
#   TREE is a list of tree nodes.
# Description:
#   Recursively convert tree into a regular expression.
# Return value:
#   Regular expression string.
sub trans_posix_recursive {
    my ($conf, $s, $treeref) = @_;
    my @tree = @{$treeref};
    my $delim;

    my $mode = shift @tree;
    my $type = $mode & T_MASK;
    if ($type == T_ALT) {
	posix_build_opt($conf, $s, @tree);
	$$s .= '?' if ($mode & T_OPT); # FIXME
    } elsif ($type == T_PFX) {
	$$s .= $conf->{group}[0] . escape_re_chars($conf, shift(@tree));
	posix_build_opt($conf, $s, @{$tree[0]});
	$$s .= '?' if ($mode & T_OPT);
	$$s .= $conf->{group}[1];	
    } elsif ($type == T_SFX) {
	my $sfx = shift(@tree);
	$$s .= $conf->{group}[0];
	posix_build_opt($conf, $s, @{$tree[0]});
	$$s .= '?' if ($mode & T_OPT);
	$$s .= escape_re_chars($conf, $sfx). $conf->{group}[1];	
    } else {
	croak "unrecognized element type";
    }
    return $$s;
}

# ########################################################
# Generate POSIX and Perl-compatible regular expressions.
# ########################################################

# Synopsis:
#   my $s = trans_posix(TREE, OPTS)
# Arguments:
#   TREE - a reference to a parse tree obtained from
#          regexp_opt;
#   OPTS - hash reference
# Description:
#   Convert tree into POSIX regular expression.

sub trans_posix {
    my ($tree, $opts) = @_;
    my %conf = (
	rxchars => '[][\\<>.(){}?*^\$]',
	group   => [ '(', ')' ]
    );
    my $s = '';
    trans_posix_recursive(\%conf, \$s, $tree);
    if ($opts->{match} eq 'word') {
	$s = "\\<$s\\>";
    } elsif ($opts->{match} eq 'exact') {
	$s = "^$s\$";
    }
    return $s;
}

# Synopsis:
#   my $s = trans_pcre(TREE, OPTS)
# Arguments:
#   TREE - a reference to a parse tree obtained from
#          regexp_opt;
#   OPTS - hash reference
# Description:
#   Convert tree into Perl-compatible regular expression.

sub trans_pcre {
    my ($tree, $opts) = @_;
    my %conf = (
	rxchars => '[][\\.(){}?*^\$]',
	group   => [ '(?:', ')' ]
    );
    my $s = '';
    trans_posix_recursive(\%conf, \$s, $tree);
    if ($opts->{match} eq 'word') {
	$s = "\\b$s\\b";
    } elsif ($opts->{match} eq 'exact') {
	$s = "^$s\$";
    }
    return $s;
}

my %transtab = (
    posix => \&trans_posix,
    pcre => \&trans_pcre
);

=pod

=head1 NAME

array_to_regexp - Convert list of strings to a regular expression

=head1 SYNOPSIS

use String::Regexp qw(:all);

my $s = array_to_regexp(@strings);

my $s = array_to_regexp(\%opts, @strings);

=head1 DESCRIPTION

Return a regexp to match a string in the list B<@strings>.  First argument
can be a reference to a hash, which controls how the regexp is built.
Valid keys are:

=over 4

=item B<type> => B<posix>|B<pcre>

Controls the flavor of the generated expression: POSIX or Perl-compatible one.
Default is B<pcre>.    

=item B<match> => B<default>|B<exact>|B<word>
    
If B<default>, the expression will match any word from B<@strings> appearing
as a part of another word.

If B<exact>, the expression will match a word from B<@strings> appearing
on a line alone.    
    
If B<word>, the expression will match single words only.    
    
=item B<debug> => B<0>|B<1>

If B<1>, enable debugging output.    
    
=back    
    
=cut     
sub array_to_regexp {
    my $trans = \&trans_pcre;
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



