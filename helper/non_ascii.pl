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

use Symbol;

my %hold;

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
	
	  foreach my $c ( split( //, $line) ){
	    if( ord($c) > 128 ){
		  if( ! exists $hold{$c} ){
		    $hold{$c} = $c;
		  }
	    }
	  }
    }
}

foreach my $c (keys %hold){
    print "$c\n";
}
