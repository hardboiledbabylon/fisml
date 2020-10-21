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
#

# if the first line of a paragraph matches a regex
# print BEGIN before that paragraph and END after
# that paragraph

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my $comment = FALSE;
my $in_para = FALSE;
my $has_pattern = FALSE;

if( @ARGV != 3 ){
    die "paba.pl <pattern> <begin> <end>";
}

my $pattern = qr/$ARGV[0]/;
my $nbeg = $ARGV[1];
my $nend = $ARGV[2];

while( my $line = <STDIN> ){

    #1st line of new para
    if( $line !~ m/^$/ and $in_para == FALSE ){
        if( $line =~ m/$pattern/ ){
            print $nbeg . "\n";
            $has_pattern = TRUE;
        }
        print $line;
        $in_para = TRUE;
    }
    elsif( $line !~ m/^$/ and $in_para == TRUE ){
        print $line;
    }
    # extra newlines between paras
    elsif( $line =~ m/^$/ and $in_para == FALSE){
        print "\n";
    }
    #end of para
    elsif( $line =~ m/^$/ and $in_para == TRUE){
        if( $has_pattern == TRUE ){
            print $nend . "\n";
            $has_pattern = FALSE;
        }
        print "\n";
        $in_para = FALSE;
    }

}