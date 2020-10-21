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
#

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use Getopt::Std;

my %options=();

if( ! getopts("tc", \%options) ){
    die;
}

use constant OFF => 1==0;
use constant ON => not OFF;
use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my %opts;

$opts{PRINT_TOTAL} = OFF;
$opts{INPUT_STDIN} = OFF;
$opts{IGNORE_FOOTNOTES} = OFF;
$opts{EXPAND_CONTRACTIONS} = OFF;

my %replacements;

if(defined($options{"t"})) {
    $opts{PRINT_TOTAL} = ON;
}
if(defined($options{"c"})) {
    $opts{EXPAND_CONTRACTIONS} = ON;
}

if( @ARGV == 0){
    $opts{INPUT_STDIN} = ON;
}

if( $opts{INPUT_STDIN} == ON and $opts{PRINT_TOTAL} == ON ){
    die "makes no sense to have total and input from stdin at same time\n";
}

sub count_words {

    my $in = $_[0];

    if( $in =~ m/^$/ ){
	return 0;
    }

    if( $in =~ m/^\{\!\!\}/ ){
        return 0;
    }

    if( $in =~ m/\{.*?\}/ ){

	my @fragged = split(/(\{.*?\})/, $in);

        # this could be handled by specifying
        # something is fisml.conf
        # WC_INCLUDE|(r|a)|PATTERN||
	foreach my $frag (@fragged) {
            $frag =~ s/^\{.*\}$/ /;
        }

	$in = join("", @fragged);
    }

    # The problem with this is it's impossible (or extremely
    # expensive) to tell the difference between the contraction in
    # "Donna's going to work." and the possesive in "Donna's toy
    # chest."
    
    if( $opts{EXPAND_CONTRACTIONS} == ON ){
	# can't is usually counted as 1 word, a shortening
	# of 'cannot'
	if( $in =~ m/([\']*)([Cc])an\'t([-[:space:]…—“”‘’"\',.?\(\)\[\]\{\}])/ ){
	    $in =~ s/([\']*)([Cc])an\'t([-[:space:]…—:;“”‘’"\',.?\(\)\[\]{}])/$1$2annot$3/g;	   
	}
	$in =~ s/\'/ /g;
    }
    
    #$in =~ s/\x{002d}//g;     # dash; collapse to single word
    
    # fix prefixes first
    # most common in english according to en_us.aff
    # handle all capitalization cases
    $in =~ s/(^|[^[:alpha:]])([Rr][Ee])\x{002d}([[:alpha:]])/$1$2$3/g;
    $in =~ s/(^|[^[:alpha:]])([Ii][Nn])\x{002d}([[:alpha:]])/$1$2$3/g;
    $in =~ s/(^|[^[:alpha:]])([Uu][Nn])\x{002d}([[:alpha:]])/$1$2$3/g;
    $in =~ s/(^|[^[:alpha:]])([Dd][Ee])\x{002d}([[:alpha:]])/$1$2$3/g;
    $in =~ s/(^|[^[:alpha:]])([Cc][Oo][Nn])\x{002d}([[:alpha:]])/$1$2$3/g;
    $in =~ s/(^|[^[:alpha:]])([Pp][Rr][Oo])\x{002d}([[:alpha:]])/$1$2$3/g;

    # other dashes categorized as linking separate words
    $in =~ s/\x{002d}/ /g;
    
    $in =~ s/\x{0027}//g;     # apostrophe / single quote ascii

    $in =~ s/\n/ /g;

    # overkill ?
    $in =~ s/[^[:alpha:]]/ /g;

    $in =~ s/\s+/ /g;

    $in =~ s/^\s+//;
    $in =~ s/\s+$//;

    if( $in =~ m/^\s+$/ or $in =~ m/^$/ ){
	return 0;
    }

    my @words = split( /\s+/, $in );

    return scalar @words;
}
    
if( $opts{INPUT_STDIN} == ON ){

    my $total = 0;

    while( my $line = <STDIN> ){
	$total += count_words( $line );
    }

    print "$total\n";
}
else{

    my $all_total = 0;
    
    foreach my $file (@ARGV){
	
	open(my $ifile, '<', $file) or die "Couldn't file: $file";

	my $total = 0;

	while( my $line = <$ifile> ){
	    $total += count_words( $line );
	}

	$all_total += $total;
	
	print "$file: $total\n";
    }

    if( $opts{PRINT_TOTAL} == ON){
	print "total: $all_total\n";
    }
}
