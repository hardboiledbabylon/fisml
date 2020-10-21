#!/usr/bin/env perl

# Copyright © 2017 by D. F. Hall <authorfunction@hardboiledbabylon.com>

# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.

# THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
# OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.


# {FOOTNOTE||ID||MARK}text
# {FN@||[I|O|Q|X]||ID}

# Multi-line footnotes aren't supported.  (However, they can be
# simulated with the appropriate tags.)

# usage: fisml_footnoter.pl [file | stdin] [-i | -o | -q | -x]
#
# Defaults to position 'i'.
#
# When set for position 'i', the MARK and following footnote are placed
# at the 'i' tag and the 'o' tag is removed.
#
# When set for position 'o', 'i' is replaced with the MARK and
# 'o' is replaced with the footnote text.
#
# When set for position 'q', everything is the same as for position
# 'o', except that 'o' anchors are deleted and what would have
# replaced the 'o' tag will instead go where the 'q' anchor is.
#
# When set for 'x', anchors 'o' and 'q' are removed, and the footnote
# MARK is placed between {SUP>}{<SUP} tag pairs and immediately
# following that the footnote in placed. Option 'x' is mostly for
# controlling the footnote MARK for output in latex.  Though it could
# have other uses.

# -H will generate a unique id for use with fisml_link.pl
# and enclose the MARK in the appropriate tags
# {LINK>}src||text{<LINK}
# {ANCHOR>}id{<ANCHOR}

# -S will override the MARK specified in footnote and replace
# it with a number representing the order and position that
# footnote was found sequentially in the file.

# -B will enclose the footnote MARK in [] brackets.

# -N will enclose the MARK within {FOOT>}{<FOOT} in {SUP} tags
# when -x or -o is used and the MARK is numerical.

# -A add extra space after MARK in footnote body when in -x or
# -o mode

# NOTE
# hyperlink support is broken if not useless at the moment

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use Getopt::Std; 

my %options=();

if( ! getopts("ioxqHSBNA", \%options) ){
    die;
}

use constant POS_INLINE => 1;
use constant POS_OUTLINE => 2;
use constant POS_XLINE => 3;
use constant POS_QLINE => 4;
use constant OFF => 1==0;
use constant ON => not OFF;
use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my %opts;

$opts{USE_FOOTNOTE_POS} = POS_INLINE;
$opts{USE_HTML_LINK} = OFF;
$opts{USE_SEQ_MARK} = OFF;
$opts{USE_BRACKETS} = OFF;
$opts{USE_FOOT_SUPER} = OFF;
$opts{USE_PADDING} = OFF;

if(defined($options{"i"})) {
    $opts{USE_FOOTNOTE_POS} = POS_INLINE;
}
if(defined($options{"o"})) {
    $opts{USE_FOOTNOTE_POS} = POS_OUTLINE;
}
if(defined($options{"x"})) {
    $opts{USE_FOOTNOTE_POS} = POS_XLINE;
}
if(defined($options{"q"})) {
    $opts{USE_FOOTNOTE_POS} = POS_QLINE;
}
if(defined($options{"S"})) {
    $opts{USE_SEQ_MARK} = ON;
}
if(defined($options{"H"})) {
    $opts{USE_HYPER_LINK} = ON;
}
if(defined($options{"B"})) {
    $opts{USE_BRACKETS} = ON;
}
if(defined($options{"N"})) {
    $opts{USE_FOOT_SUPER} = ON;
}
if(defined($options{"A"})) {
    $opts{USE_PADDING} = ON;
}

if( ( $opts{USE_HYPER_LINK} and
      $opts{USE_FOOTNOTE_POS} == POS_INLINE ) or
    ( $opts{USE_HYPER_LINK} and
      $opts{USE_FOOTNOTE_POS} == POS_XLINE ) or
    ( $opts{USE_HYPER_LINK} and
      $opts{USE_FOOTNOTE_POS} == POS_QLINE ) ){
    die "Options -i (or -x or -q) and -H are mutually exclusive";
}

my @input=();

if( @ARGV > 1 ){
    die "Too many file argument, can only handle one at a time.";
}
elsif( @ARGV == 1 ){
    open(my $input_file, '<', $ARGV[0]) or die "Couldn't open input: $ARGV[0]";
    chomp( @input = <$input_file> );
    close( $input_file );
}
else{
    chomp( @input = <STDIN> );
}

my %footnotes;

my $NUM_FOOTER_ENTRIES = 0;

my $IN_FOOT = FALSE;
my $CUR_FOOT = "";
foreach my $line (@input){

    # extra braces are allowable in the text section
    # so allow greedy match there
    # make sure fisml_unwrap_foot.pl is run 1st
    if( $line =~ m/^{FOOTNOTE\|\|(.+?)\|\|(.+?)}(.*)/ ){

        $IN_FOOT = TRUE;
        $CUR_FOOT = $1;

	$footnotes{$1}{id} = $1;
	$footnotes{$1}{mark} = $2;
	$footnotes{$1}{text} = $3;

	if( $opts{USE_SEQ_MARK} ){
	    $footnotes{$1}{mark} = $NUM_FOOTER_ENTRIES;
	}

	if( $opts{USE_BRACKETS} ){
	    $footnotes{$1}{mark} = "[" . $footnotes{$1}{mark} . "]";
	}	
	
	$NUM_FOOTER_ENTRIES++;

	# remove footnote
	$line = "";
    }
    elsif( $line eq "" and $IN_FOOT == TRUE ){
        $IN_FOOT = FALSE;
        $footnotes{$CUR_FOOT}{text} =~ s/^ +//;
    }
    elsif( $IN_FOOT ){
        $footnotes{$CUR_FOOT}{text} .= " " . $line;
        $line = "";
    }
}

my @output;

# need for -i to allow for custom mark from latex
# one that places the SUP but also places the
# footnote inline with the SUP within it
# I|O|X
# if I specified but POS == Q then del I and place O

foreach my $line (@input){

    if( $line =~ m/\{FN@\|\|[I|O|Q]\|\|.+?\}/ ){
	
	my @fragged = split(/(\{FN@\|\|[I|O|Q]\|\|.+?\})/, $line);

	foreach my $frag (@fragged){

	    if( $frag !~ m/\{FN@\|\|([I|O|Q])\|\|(.+?)\}/ ){
		next;
	    }

	    my $POS = $1;
	    my $ID = $2;

	    if( exists $footnotes{$ID} ){

		my $alt_footmark = $footnotes{$ID}{mark};
		my $footmark_pad = "";

		if( $opts{USE_PADDING} == ON ){
		    $footmark_pad = " ";
		}
		
		if( $opts{USE_FOOT_SUPER} == ON ){
		    if( $footnotes{$ID}{mark} =~ m/^[0-9]+$/ ){
			$alt_footmark = "{SUP>}" . $footnotes{$ID}{mark} . "{<SUP}";
		    }
		}

		my $foot_rep_i = quotemeta("{FN@\|\|I\|\|" . $ID . "}");
		my $foot_rep_o = quotemeta("{FN@\|\|O\|\|" . $ID . "}");
		my $foot_rep_q = quotemeta("{FN@\|\|Q\|\|" . $ID . "}");
		my $foot_act_i = "{FOOT>}" . $footnotes{$ID}{text} . "{<FOOT}";
		my $foot_act_x = "{SUP>}" . $footnotes{$ID}{mark} . "{<SUP}{FOOT>}" . $alt_footmark . $footmark_pad . $footnotes{$ID}{text} . "{<FOOT}";
		my $foot_act_o = "{FOOT>}" . $alt_footmark . $footmark_pad . $footnotes{$ID}{text} . "{<FOOT}";

		my $foot_mark = "{SUP>}" . $footnotes{$ID}{mark} . "{<SUP}";
		
		# mark becomes footer id in hyperlinks
		# or else things could get confused
		# when reading the src
		if( $opts{USE_HYPER_LINK} ){
		    $foot_act_o = "{FOOT>}{SUP>}{ANCHOR>}". $footnotes{$ID}{id} . "_footer" . "{<ANCHOR}{LINK>}#" . $footnotes{$ID}{id}. "_origin" . "||" . $footnotes{$ID}{id} . "{<LINK}{<SUP}" . $footnotes{$ID}{text} . "{<FOOT}";
		    $foot_mark = "{SUP>}{ANCHOR>}" . $footnotes{$ID}{id} . "_origin" . "{<ANCHOR}{LINK>}#" . $footnotes{$ID}{id} . "_footer" . "||" . $footnotes{$ID}{mark} . "{<LINK}{<SUP}";
		}
		
		if( $opts{USE_FOOTNOTE_POS} == POS_INLINE ){
		    if( $POS eq "I" ){
			$frag =~ s/$foot_rep_i/$foot_act_i/;
		    }
		    if( $POS eq "O" ){
			$frag =~ s/$foot_rep_o//;
		    }
		    if( $POS eq "Q" ){
			$frag =~ s/$foot_rep_q//;
		    }
		}
		if( $opts{USE_FOOTNOTE_POS} == POS_OUTLINE ){
		    if( $POS eq "I" ){
			$frag =~ s/$foot_rep_i/$foot_mark/;
		    }
		    if( $POS eq "O" ){
			$frag =~ s/$foot_rep_o/$foot_act_o/;
		    }
		    if( $POS eq "Q" ){
			$frag =~ s/$foot_rep_q//;
		    }
		}
		if( $opts{USE_FOOTNOTE_POS} == POS_QLINE ){
		    if( $POS eq "I" ){
			$frag =~ s/$foot_rep_i/$foot_mark/;
		    }
		    if( $POS eq "O" ){
			$frag =~ s/$foot_rep_o//;
		    }
		    if( $POS eq "Q" ){
			$frag =~ s/$foot_rep_q/$foot_act_o/;
		    }
		}
		if( $opts{USE_FOOTNOTE_POS} == POS_XLINE ){
		    if( $POS eq "I" ){
			$frag =~ s/$foot_rep_i/$foot_act_x/;
		    }
		    if( $POS eq "O" ){
			$frag =~ s/$foot_rep_o//;
		    }
		    if( $POS eq "Q" ){
			$frag =~ s/$foot_rep_q//;
		    }
		}   
	    }
	}
	$line = join("", @fragged);
    }
}

my @out;
my $BLANK = FALSE;

foreach my $line (@input){

    if( $line eq "" and $BLANK == TRUE){
        next;
    }
    elsif( $line eq "" ){
        $BLANK = TRUE;
    }
    else{
        $BLANK = FALSE;
    }
    
    print $line . "\n";
}

