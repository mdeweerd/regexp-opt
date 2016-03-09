# -*- perl -*-
# Copyright (C) 2015-2016 Sergey Poznyakoff <gray@gnu.org>
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

package List::Regexp;

use strict;
use Carp;
use Data::Dumper;

require Exporter;
our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(regexp_opt) ] );
our @EXPORT_OK = ( qw(regexp_opt) );
our @EXPORT = qw(regexp_opt);
our $VERSION = "1.00";

# Synopsis:
#   my @res = split_prefix(ARRAY)
# Arguments:
#   ARRAY is a sorted array of char array references.
# Description:
#   Find N first elements of ARRAY sharing the longest prefix (of length L).
#   In other words, find N and L such that ARRAY[N][L+1] != ARRAY[N+1][L+1].
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
#   my @list = parse(ARRAY)
# Arguments:
#   ARRAY is a sorted array of char array references.
# Description:
#   Recursively parse the array of arguments and return a parse tree.
sub parse {
    my @t = @_;
    my @output;
    return [] if $#t == -1;
    while (1) {
	my @res = split_prefix \@t;
	if ($res[1] < 0) {
	    my @rv = map { [ reverse @{$_} ] } @t;
	    @res = split_prefix \@rv;
	    if ($res[1] >= 0) {
		my @x = reverse @{$rv[0]}[0..$res[1]];
		my $sfxlen = $#x;
		my $sfx = join('', @x);
		my $type = T_SFX;
		my $prefixes = parse(map { my @r = @{$_};
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
	    my $suffixes = parse(map { my @r = @{$_};
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
# branch:  A delimiter used to separate branches ('|' for both posix and
# pcre)   
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
#   nodelist_to_regexp(CONF, STRING, LIST...)
# Arguments:
#   CONF and STRING as described above.
#   LIST is a subtree.
# Description:
#   Convert subtree into regular expression.
sub nodelist_to_regexp {
    my $conf = shift;
    my $strref = shift;
    my $delim;
    my @cclass;
    my $s;

    foreach my $elt (@_) {
	if (ref($elt) eq 'ARRAY') {
	    $s .= $delim if defined $delim;
	    generic_regexp($conf, \$s, $elt);
	    $delim = $conf->{branch};
	} elsif (length($elt) == 1) {
	    push @cclass, $elt;
	} else {
	    $s .= $delim if defined $delim;
	    $s .= escape_re_chars($conf, $elt);
	    $delim = $conf->{branch};
	}
    }

    if ($#cclass == 0) {
	$s .= $delim if defined $delim;
	$s .= $cclass[0];
    } elsif ($#cclass >= 0) {
	$s .= $delim if defined $delim;
	$s .= '[';
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
			$s .= "$start-$end";
		    } else {
			$s .= "$start$end";
		    }
		    $start = $c;
		    $end = undef;
		}
	    } elsif (ord($c) - ord($start) == 1) {
 		$end = $c;
	    } else {
		$s .= $start;
		$s .= $end if defined $end;
		$start = $c;
		$end = undef;
	    }
	}

	if (defined($start)) {
	    $s .= $start;
	    if (defined($end)) {
		if (ord($end) - ord($start) > 1) {
		    $s .= "-$end";
		} else {
		    $s .= $end;
		}
	    }
	}
	$s .= ']';
    }

    $s = $conf->{group}[0] . $s . $conf->{group}[1]
	if $delim;
    $$strref .= $s;
}

# Synopsis:
#   generic_regexp(CONF, STRING, TREE...)
# Arguments:
#   CONF and STRING as described above.
#   TREE is a list of tree nodes.
# Description:
#   Recursively convert tree into a regular expression.
# Return value:
#   Regular expression string.
sub generic_regexp {
    my ($conf, $s, $treeref) = @_;
    my @tree = @{$treeref};
    my $delim;

    my $mode = shift @tree;
    my $type = $mode & T_MASK;
    if ($type == T_ALT) {
	nodelist_to_regexp($conf, $s, @tree);
	$$s .= '?' if ($mode & T_OPT); # FIXME
    } elsif ($type == T_PFX) {
	$$s .= escape_re_chars($conf, shift(@tree));
	nodelist_to_regexp($conf, $s, @{$tree[0]});
	$$s .= '?' if ($mode & T_OPT);
    } elsif ($type == T_SFX) {
	my $sfx = shift(@tree);
	$$s .= $conf->{group}[0];
	nodelist_to_regexp($conf, $s, @{$tree[0]});
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
#          parse;
#   OPTS - hash reference
# Description:
#   Convert tree into POSIX extended regular expression.

sub trans_posix {
    my ($tree, $opts) = @_;
    my %conf = (
	rxchars => '[][\\<>.|(){}?*^\$]',
	group   => [ '(', ')' ],
	branch  => '|'
    );
    my $s = '';
    generic_regexp(\%conf, \$s, $tree);
    if ($opts->{match} eq 'word') {
	$s = "\\<$s\\>";
    } elsif ($opts->{match} eq 'exact') {
	$s = "^$s\$";
    } elsif (defined($opts->{match}) and $opts->{match} ne 'default') {
	croak "invalid match value: $opts->{match}";
    }
    return $s;
}

# Synopsis:
#   my $s = trans_pcre(TREE, OPTS)
# Arguments:
#   TREE - a reference to a parse tree obtained from
#          parse;
#   OPTS - hash reference
# Description:
#   Convert tree into Perl-compatible regular expression.

sub trans_pcre {
    my ($tree, $opts) = @_;
    my %conf = (
	rxchars => '[][\\|.(){}?*^\$]',
	group   => [ '(?:', ')' ],
	branch  => '|'
    );
    my $s = '';
    generic_regexp(\%conf, \$s, $tree);
    if ($opts->{match} eq 'word') {
	$s = "\\b$s\\b";
    } elsif ($opts->{match} eq 'exact') {
	$s = "^$s\$";
    } elsif (defined($opts->{match}) and $opts->{match} ne 'default') {
	croak "invalid match value: $opts->{match}";
    }
    return $s;
}

sub trans_emacs {
    my ($tree, $opts) = @_;
    my %conf = (
	rxchars => '[][.?*^\$]',
	group   => [ '\\\\(?:', '\\\\)' ],
	branch  => '\\\\|'
    );

    my $s = '';
    generic_regexp(\%conf, \$s, $tree);
    if ($opts->{match} eq 'word') {
	$s = '\\\\<'.$s.'\\\\>';
    } elsif ($opts->{match} eq 'exact') {
	$s = "^$s\$";
    } elsif (defined($opts->{match}) and $opts->{match} ne 'default') {
	croak "invalid match value: $opts->{match}";
    }
    return $s;
}

my %transtab = (
    pcre => \&trans_pcre,
    posix => \&trans_posix,
    emacs => \&trans_emacs
);

=pod

=head1 NAME

regexp_opt - Convert list of strings to a regular expression

=head1 SYNOPSIS

use List::Regexp qw(:all);

my $s = regexp_opt(@strings);

my $s = regexp_opt(\%opts, @strings);

=head1 DESCRIPTION

Return a regexp to match a string in the list B<@strings>.  First argument
can be a reference to a hash, which controls how the regexp is built.
Valid keys are:

=over 4

=item B<type> => B<pcre>|B<posix>|B<emacs>

Controls the flavor of the generated expression: Perl-compatible (the
default), POSIX extended, or Emacs.

=item B<match> => B<default>|B<exact>|B<word>
    
If B<default>, the expression will match any word from B<@strings> appearing
as a part of another word.

If B<exact>, the expression will match a word from B<@strings> appearing
on a line alone.    
    
If B<word>, the expression will match single words only.    
    
=item B<debug> => B<0>|B<1>

If B<1>, enable debugging output.    
    
=back    

=head1 AUTHORS

Sergey Poznyakoff <gray@gnu.org>    
    
=cut     
sub regexp_opt {
    my $trans = \&trans_pcre;
    my $opts;

    $opts = shift if (ref($_[0]) eq 'HASH');

    if (defined($opts->{type})) {
	croak "unsupported type: $opts->{type}"
	    unless exists $transtab{$opts->{type}};
	$trans = $transtab{$opts->{type}};
    }
    
    my @t = map { my @x = split //, $_; \@x } sort @_;
    my $tree = parse(@t);
    unshift @{$tree}, T_ALT;
    print Data::Dumper->Dump([$tree], [qw(tree)]) if ($opts->{debug});
    return &{$trans}($tree, $opts);
}

1;



