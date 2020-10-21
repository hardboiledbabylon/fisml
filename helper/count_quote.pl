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

use constant ON => 1;
use constant OFF => 0;

my %options=();

if( ! getopts("nc", \%options) ){
    die;
}

my %opts;
$opts{PRINT_CONT} = OFF;
$opts{CONT} = OFF;

if(defined($options{"c"})) {
    $opts{PRINT_CONT} = ON;
}
if(defined($options{"n"})) {
    $opts{NCONT} = ON;
}

# quotes sometimes are not balanced
# across multiple paragraphs
# but there is no way to catch a quote that
# should be at the previous end of the line but
# isn't, except by going through with -n

my $QS=0;
my $LN=1;

my $PB=1; #1st para always implied

while( my $line = <STDIN>){

    chomp($line);

    my @pieces = split( //, $line);

    if( $opts{CONT} == ON ){
        #if [0] = “ & prev_blank & opening quote prior
        if( $PB == 1 and $pieces[0] eq "“" and $QS == 1){
            shift(@pieces);
            if( $opts{PRINT_CONT} == ON ){
                print "continuation on line $LN\n"
            }
        }
    }

    foreach my $piece (@pieces){
        if( $piece eq "“" ){
            if( $QS == 1 ){
                print "already opened, but open on line: $LN\n";
                exit;
            }
            else{
                $QS = 1;
            }
        }
        elsif( $piece eq "”"){
            $QS = 0;
        }
    }
    
    $LN++;
    
    if( length($line) > 0 ){
        $PB=0;
    }
    else{
        $PB=1;
    }
}
