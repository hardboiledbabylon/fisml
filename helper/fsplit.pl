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

# split a file into sections at a match

use utf8;

use strict;
use warnings;
use open qw(:std :utf8);
use Getopt::Std;

use constant OFF => 1==0;
use constant ON => not OFF;

my $i = 0;
my $fh;

my %options=();

if( ! getopts("d0H", \%options) ){
    die;
}

my %opts;
$opts{DEL_PATTERN} = OFF;
$opts{SECT_ZERO} = OFF;

if(defined($options{"d"})) {
    $opts{DEL_PATTERN} = ON;
}
if(defined($options{"0"})) {
    $opts{SECT_ZERO} = ON;
}
if(defined($options{"H"})) {
    print "fsplit.pl [-d -0 -H] SPLIT_REGEX FILE_BASNAME ";
    print "SECTION_NAME [FILE or STDIN]\n";
    print "   -d remove SPLIT_REGEX from output\n";
    print "   -0 don't output what is at the head of a file, ";
    print "before the first SPLIT_REGEX\n";
    print "   -H print this message\n";
    exit(0);
}

if( @ARGV != 3 ){
    die "bad arguments";
}

my $splitmark = qr/$ARGV[0]/;
my $fbase = $ARGV[1];
my $sectname = $ARGV[2];

if( 0 ){
    print "pr: $ARGV[0]\n";
    print "s: $splitmark\n";
    print "f: $fbase\n";
    print "n: $sectname\n";
    exit;
}

if( $opts{SECT_ZERO} == ON ){
    my $output_name = $fbase . "_0000";
    open($fh, '>', $output_name) or die "couldn't open output file";
    
    if( $sectname ne "" ){
        print $fh "{!!}{@}{split_name}" . $sectname . " 0" . "\n";
    }
}

while(my $line = <STDIN>){

    if( $line =~ /$splitmark/ ){
	$i++;
	my $output_name = sprintf("$fbase"."_%04d", $i);
	
	if( defined $fh ){
	    close( $fh );
	}
	
	open($fh, '>', $output_name) or die "couldn't open output file";
	if( $sectname ne "" ){
	    print $fh "{!!}{@}{split_name}" . $sectname . " " . $i . "\n";
	}
        if( $opts{DEL_PATTERN} == ON ){
            next;
        }
    }
    if( $i == 0 and $opts{SECT_ZERO} == OFF ){
        next;
    }
    print $fh $line;
    
}

if( defined $fh ){
    close( $fh );
}
