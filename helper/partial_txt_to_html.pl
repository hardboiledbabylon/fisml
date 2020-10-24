#!/usr/bin/perl

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

use constant FALSE => 1==0;
use constant TRUE => not FALSE;

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

my $P_START = FALSE;
my $P_PUT = FALSE;

while (my $line = <$input_file>) {
    if( $line !~ m/^$/ ){
	if( $line =~ m/^\s*\t*(<\/*div|<\/*p|<\/*blockquote|<\/*h2|<\/*h3|<\/*h4|<\/*h1|<\/*tt)/){
	    print $line;
	    while ( $line = <$input_file>) {
		if( $line =~ m/^$/ ){
		    print "\n";
		    last;
		}
		print $line;
	    }
	}
	else{
	    print "<p>\n";
	    print $line;
	    while (1) {
                $line = <$input_file>;
                if( !defined $line){
                    print "</p>\n";
                    last;
                }
		if( $line =~ m/^$/ ){
		    print "</p>\n\n";
		    last;
		}
		print $line;
	    }
	}
    }
}

