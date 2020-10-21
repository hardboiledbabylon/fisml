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

# This does not operate recursively.  This is so a spec file can be
# used to replace one fisml tage with another for secondary
# processing.

# When multiply spec files are specified, entries in subsequent files
# override entries in prior files

# Trailing whitespace is not removed from entries in the spec file.

# The footnoter should be run prior to running this, or the foonotes
# will get broken.

# USAGE
#
# fisml_do.pl [-C -B] -f file.fisml ... -e "{xxx}||yyy"
#
# -C leave comments lines in output.
# -B replace all sections in the input patterns with blanks.
#    This is mostly useful for testing if some tag has been missed
#    as the only {xx} patterns that should be left are tags that
#    have been missed.
# Entries from later files overwrite earlier matching entries.
# Entries on the command line also override each other in this way.
# But they also override all file patterns that came before.


use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use Getopt::Long;

use constant OFF => 0;
use constant ON => 1;
use constant FALSE => 1==0;
use constant TRUE => not FALSE;

my %opts;

$opts{LEAVE_COMMENT_LINE} = OFF;
$opts{BLANK_OUTPUT} = OFF;

GetOptions( "f=s" => \my @FILES,
	    "e=s" => \my @SPECS_CL,
	    "C" => \$opts{LEAVE_COMMENT_LINE},
	    "B" => \$opts{BLANK_OUTPUT} ) or die "Error in command line";

if( ! @FILES and ! @SPECS_CL ){
    die "Must have either file or provided line";
}

my %replacements;

my @specs;

sub process_escape{

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
            push( @specs, $spc . "||" );
        }
    }
}

foreach my $spec (@specs){
	
	if( $spec !~ m/^(\{.*?\})\|(e*)\|(.*)\|\|/ ){
	    next;
	}

	if( $opts{BLANK_OUTPUT} == ON ){
	    $replacements{$1} = " ";
	}
	else{
	    if( $2 eq "e" ){
		$replacements{$1} = process_escape( $3 );
	    }
	    else{
		$replacements{$1} = $3;
	    }
	}
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
    
    if( $line =~ m/^\{\!\!\}/ and $opts{LEAVE_COMMENT_LINE} == OFF ){
	next;
    };

    my @fragged = split(/(\{.*?\})/, $line);

    foreach my $frag (@fragged) {
	if( exists $replacements{$frag} ){
	    $frag = $replacements{$frag}
	}
    }
    
    print join("", @fragged);

}

