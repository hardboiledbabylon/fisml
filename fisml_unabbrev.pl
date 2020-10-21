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

# if a paragraph starts with a tag that ends with a hash/pound
# (aka {.+#}) treat this as an abbreviated tag and place
# the paired version ({.+>}{<.+}) at start and end of
# the paragraph

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my $comment = FALSE;
my $in_para = FALSE;
my $has_pattern = FALSE;
my $pattern_hold = "";


while( my $line = <STDIN> ){

    if( $line =~ m/^\{\!\!\}/ ){
        print $line;
        next;
    }

    #1st line of new para
    if( $line !~ m/^$/ and $in_para == FALSE ){
        if( $line =~ m/^\{(.+)\#\}/ ){
            $pattern_hold = $1;
            #also remove any spaces that might be after
            $line =~ s/^\{.+\#\} *//;
            print "{" . $pattern_hold . ">}" . "\n";
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
            print "{<" . $pattern_hold . "}" . "\n";
            $has_pattern = FALSE;
        }
        print "\n";
        $in_para = FALSE;
    }

}

# error on last line, not detecting end of para
if( $has_pattern == TRUE and $in_para == TRUE ){
    print "{<" . $pattern_hold . "}" . "\n";
}
