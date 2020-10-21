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

use Getopt::Long;

use Symbol;

use constant OFF => 1==0;
use constant ON => not OFF;
use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my $TUNE_F = 2;
# old default for TUNE_FB was 3

GetOptions( "tune:i" => \$TUNE_F );

if( $TUNE_F < 2 ){
    die "Tune factor < minumum of 2";
}

my $TUNE_FB = $TUNE_F + 1;

my %hyph;

my @INPUTS;

if( @ARGV > 0 ){
    foreach my $arg (@ARGV){
	my $INPUT_FILE = gensym;
	open($INPUT_FILE, '<', $arg) or die "Couldn't open input: $arg";
	push( @INPUTS, $INPUT_FILE );
    }
}
else{
    push( @INPUTS, *STDIN );
}

foreach my $input (@INPUTS){

    while( my $line = <$input> ){
	
	if( $line =~ m/^$/ or $line =~ m/^\{\!\!\}/ ){
	    next;
	}


	if( $line =~ m/\{.*?\}/ ){

	    my @fragged = split(/(\{.*?\})/, $line);
	    
	    foreach my $frag (@fragged) {
		$frag =~ s/^\{.*\}$/ /;
	    }

	    $line = join("", @fragged);
	}

	$line =~ s/\n+$//;

	$line =~ s/\p{P}/ /g;
	$line =~ s/[…—“”‘’;:\(\)\[\].?!\/~`+=@#$%^&*|<>,_]/ /g;

	$line =~ s/\s+/ /g;

	$line =~ s/^\s+//;
	$line =~ s/\s+$//;

	if( $line =~ m/^\s+$/ or $line =~ m/^$/ ){
	    next;
	}

	foreach my $frag ( split(/ /, $line) ) {
	    if( $frag =~ m/([[:alpha:]])\1{$TUNE_FB,}/ ){
		while( $frag =~ m/(([[:alpha:]])\2{$TUNE_F,})/ ){
		    my $x = join("-", (split(//,"$1")));
		    $frag =~ s/$1/$x/;
		}
		if( ! exists $hyph{$frag} ){
		    $hyph{$frag} = $frag;
		}
	    }
	}
    }
}

foreach my $key (keys %hyph){
    print "\\hyphenation{".$key."}\n";
}