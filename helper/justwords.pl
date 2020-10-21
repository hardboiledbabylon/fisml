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

use utf8;

use strict;
use warnings;
use open qw(:std :utf8);

my $output;

my $input_file;

if( @ARGV > 1 ){
    die "Too many file argument, can only handle one at a time.";
}
elsif( @ARGV == 1 ){
    open($input_file, '<', $ARGV[0]) or die "Couldn't open input: $ARGV[0]";
}
else{
    $input_file = *STDIN;
}

while(my $line = <$input_file>){

    if( $line =~ m/^$/ ){
	next;
    }

    $line =~ s/\x{002d}//g;     # dash; collapse to single word
    $line =~ s/\x{0027}//g;     # apostrophe / single quote ascii
    $line =~ s/\x{00A7}//g;     # section
    $line =~ s/\x{2019}//g;     # single quote right
                                # can be apostrophe

    $line =~ s/\p{P}/ /g;
#    $line =~ s/\n/ /g;
    $line =~ s/[<>=+|\\^&*()!@#$%\/~`{}_:;]/ /g;

    $line =~ s/ +/ /g;

    $output .= $line;
}

$output =~ s/\s+/ /g; 
    
print $output;