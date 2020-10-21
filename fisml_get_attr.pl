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

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

my %attributes;

if( @ARGV < 2 ){
    die "need file and argument";
}
if( @ARGV > 2 ){
    die "too many arguments";
}

open( my $input, '<', $ARGV[0]) or die "Couldn't open file: $ARGV[0]";

while(my $line = <$input>){

    chomp( $line );

    if($line =~ m/^\{\!\!\}\{\@\}\{(.+?)\}(.*)/){
	my @working;
	$working[0] = $1;
	$working[1] = $2;
#	$working[0] =~ s/\s*$//;
	$working[1] =~ s/^\s*//;
        $working[1] =~ s/\s*$//;
	$attributes{$working[0]} = $working[1];
    }
}

if( exists $attributes{$ARGV[1]} ){
    print $attributes{$ARGV[1]} . "\n";
}