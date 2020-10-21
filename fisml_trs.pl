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

use Getopt::Long; 

my %opts;

GetOptions( "f=s" => \my @FILES,
	    "e=s" => \my @SPECS_CL) or die "Error in command line";

if( ! @FILES and ! @SPECS_CL ){
    die "Must have either file or provided line";
}

use constant OFF => 1==0;
use constant ON => not OFF;
use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my %replacements;

my @specs;

sub process_escape {

    my $line = shift;
    my $ESC_P = FALSE;

    my @pieces = split( //, $line );

    my @give;
    
    foreach my $c (@pieces){

	if( $c eq "^" and not $ESC_P ){
	    $ESC_P = TRUE;
	    next;
	}
	else{
	    if( $ESC_P and $c eq "M" ){
		push( @give, "\n" );
	    }
	    else{
		push( @give, $c );
	    }
	    $ESC_P = FALSE;
	}
    }
    return join( "", @give );
}

foreach my $file ( @FILES ) {

    if( $file eq "" ){
        next;
    }

    open(my $spec_file, '<', $file) or die "Couldn't open spec file: $file";

    my @tmp;

    chomp( @tmp = <$spec_file> );
    push( @specs, @tmp );
    close $spec_file;
}

# command line specs don't have terminating ||
if( @SPECS_CL ){
    foreach my $spc (@SPECS_CL){
        if( $spc ne "" ){
            push( @specs, $spc . "||");
        }
    }
}

foreach my $spec (@specs){
	
	if( $spec !~ m/^(.)\|(e*)\|(.*)\|\|/ ){
	    next;
	}

	if( $2 eq "e" ){
	    $replacements{$1} = process_escape( $3 );
	}
	else{
	    $replacements{$1} = $3;
	}
}
         
sub trs_process {

    my $line = shift;

    my @give;

    my @fragged = split(/(\{.*?\})/, $line);

    foreach my $frag (@fragged) {
	if( $frag !~ m/\{.*\}/ ){

	    foreach my $c ( split(//, $frag) ){
		
		if( exists $replacements{$c} ){
		    push( @give, $replacements{$c});
		}
		else{
		    push( @give, $c );
		}
	    }
	}
	else{
	    push( @give, $frag );
	}
    }

    return join( "", @give );
}

my $input_file;

if( @ARGV > 1 ){
    die "Too many file arguments, can only handle one at a time.";
}
elsif( @ARGV == 1 ){
    open($input_file, '<', $ARGV[0]) or die "Couldn't open input: $ARGV[0]";
}
else{
    $input_file = *STDIN;
}

while(my $line = <$input_file>){
    
    if( $line =~ m/^\{\!\!\}/ ){
	print $line;
	next;
    };

    print trs_process( $line );

}
