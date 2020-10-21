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

# output a subpart of an input that is between START and END

use utf8;

use strict;
use warnings;
use open qw(:std :utf8);

use Getopt::Std;

use constant FALSE => 1==0;
use constant TRUE => not FALSE;
use constant ON => 1;
use constant OFF => 0;

# .config/fsnip.conf ?
my $DIVIDER_STR = "{!!}{SNIP}{!!}";

my %options;
my %opts;

$opts{INVERTED} = OFF;
$opts{OUTPUT} = OFF;
$opts{DIVIDER} = OFF;

# if -d '', use internal ?
# -o overrides -d
if( ! getopts('ioHd:', \%options) ){
    die;
}

if(defined($options{"i"})) {
    $opts{INVERTED} = ON;
}
if(defined($options{"o"})) {
    $opts{OUTPUT} = ON;
}
if(defined($options{"d"})) {
    $opts{DIVIDER} = ON;
    if( $options{"d"} ne '' ){
        $DIVIDER_STR = $options{"d"};
    }
}

if(defined($options{"H"})) {
    print "fsnip.pl [-i -o -d -H] START END [FILE or STDIN]\n";
    print "   -i invert; snip things _not_ between START & END\n";
    print "   -d print a divider string where each snip is removed\n";
    print "   -o ouput each snip to a separate file (overrides -d)\n";
    print "   -H print this message\n";
    exit(0);
}

if( $opts{OUTPUT} == ON and $opts{INVERTED} != ON ){
    die "-o doesn't really make any sense without -i"
}

if( @ARGV != 2 ){
    die "bad arguments; require starting and ending pattern";
}

my $startmark = qr/$ARGV[0]/;
my $endmark = qr/$ARGV[1]/;

my $started = FALSE;

my $output;

if( $opts{OUTPUT} != ON ){
    $output = *STDOUT;
}

my $lnum = 0;
my $n_snips = 0;

while(my $line = <STDIN>){

    $lnum++;

    if( $line =~ /$startmark/ ){
	if( $started == TRUE ){
	    die "detected start mark without closing mark on line: $lnum"
	}

        if( $opts{DIVIDER} == ON and $opts{OUTPUT} == OFF ){
            print "$DIVIDER_STR\n";
        }

	if( $opts{OUTPUT} == ON ){
	    my $OF = sprintf("snip_%04d", $n_snips);
	    $n_snips++;
	    open($output, '>', $OF) or die "Couldn't open input: $OF";
	}
	
	$started = TRUE;
	next;
    }
    
    if( $line =~ /$endmark/ ){
	if( $started != TRUE ){
	    die "detected end mark before starting mark on line: $lnum"
	}
	
	if( $opts{OUTPUT} == ON ){
	    close( $output );
	}
	
	$started = FALSE;
	next;
    }

    if( $started == TRUE ){
	if( $opts{INVERTED} == ON ){
	    print $output $line;
	    next;
	}
	else{
	    next;
	}
    }
    else{
	if( $opts{INVERTED} == ON){
	    next;
	}
	print $output $line;
    }

}

