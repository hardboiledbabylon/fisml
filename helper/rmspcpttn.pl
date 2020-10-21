#!/usr/bin/env perl

# Copyright © 2019 by D. F. Hall <authorfunction@hardboiledbabylon.com>

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

# remove and add space around string matches

# C||STRING|| -> string is centered between spaces
# L||STRING|| -> spaces to right of string are removed
# R||STRING|| -> spaces to left of string are removed
# Z||STRING|| -> zero-width joiner
# D||STRING|| -> string and spaces around string are replaced with a dash
# S||STRING|| -> string specifies where two paragraphs will be merged, and
#                the string is removed from the output.
# Q||STRING|| -> string specifies where two paragraphs will be merged,
#                but string remains in the output and spaces to either side
#                are removed.
use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use constant OFF => 0;
use constant ON => 1;

use Getopt::Long;

my @TOKENS_Q;
my @TOKENS_S;
my @TOKENS_Z;
my @TOKENS_L;
my @TOKENS_R;
my @TOKENS_C;
my @TOKENS_D;

my %option;

$option{ADD_SPACE} = OFF;

GetOptions( "f=s" => \my @FILES,
            "p" => \$option{ADD_SPACE},
            "Q=s" => \my @Q_CL,
            "S=s" => \my @S_CL,
            "Z=s" => \my @Z_CL,
            "L=s" => \my @L_CL,
            "R=s" => \my @R_CL,
            "C=s" => \my @C_CL,
            "D=s" => \my @D_CL ) or die "Error in command line";

if( ! @FILES and !@Q_CL and ! @S_CL and ! @Z_CL
    and ! @L_CL and ! @R_CL and ! @C_CL and ! @D_CL ){
    die "Must have either file or provided line";
}

my @ents;

foreach my $file ( @FILES ) {

    if( $file eq "" ){
        next;
    }

    open(my $spec_file, '<', $file) or die "Couldn't open file: $file";

    my @tmp;

    chomp( @tmp = <$spec_file> );
    push( @ents, @tmp );
    close $spec_file;
}

if( @Q_CL ){
    foreach my $ent (@Q_CL){
        if( $ent ne "" ){
            $ent = "Q||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}
if( @S_CL ){
    foreach my $ent (@S_CL){
        if( $ent ne "" ){
            $ent = "S||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}
if( @Z_CL ){
    foreach my $ent (@Z_CL){
        if( $ent ne "" ){
            $ent = "Z||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}
if( @L_CL ){
    foreach my $ent (@L_CL){
        if( $ent ne "" ){
            $ent = "L||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}
if( @R_CL ){
    foreach my $ent (@R_CL){
        if( $ent ne "" ){
            $ent = "R||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}
if( @C_CL ){
    foreach my $ent (@C_CL){
        if( $ent ne "" ){
            $ent = "C||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}
if( @D_CL ){
    foreach my $ent (@D_CL){
        if( $ent ne "" ){
            $ent = "D||" . $ent . "||";
            push( @ents, $ent );
        }
    }
}

foreach my $ent (@ents){
    if( $ent !~ m/([QSZLRCD])\|\|(.+)\|\|/ ){
        next;
    }
    if( $1 eq "Q" ){
        push( @TOKENS_Q, $2 );
    }
    if( $1 eq "S" ){
        push( @TOKENS_S, $2 );
    }
    if( $1 eq "Z" ){
        push( @TOKENS_Z, $2 );
    }
    if( $1 eq "L" ){
        push( @TOKENS_L, $2 );
    }
    if( $1 eq "R" ){
        push( @TOKENS_R, $2 );
    }
    if( $1 eq "C" ){
        push( @TOKENS_C, $2 );
    }
    if( $1 eq "D" ){
        push( @TOKENS_D, $2 );
    }
}

my $slurped; {local $/; $slurped = <STDIN>; }

my @broken = split( /(\n*\{\!\!\}.*\n)/, $slurped);

foreach my $chunk (@broken){
    
    if( $chunk !~ /^\n*\{\!\!\}/ ){

        foreach my $TOKEN (@TOKENS_Q){
            my $QTOKEN = quotemeta( $TOKEN );
            $chunk =~ s/[ \n]*$QTOKEN[ \n]*/$TOKEN/g;
        }
        
        foreach my $TOKEN (@TOKENS_S){
            my $QTOKEN = quotemeta( $TOKEN );
            $chunk =~ s/[ \n]*$QTOKEN[ \n]*/\n/g;
        }

        foreach my $TOKEN (@TOKENS_Z){
            my $QTOKEN = quotemeta( $TOKEN );
            $chunk =~ s/[ \n]*$QTOKEN[ \n]*//g;
        }

        foreach my $TOKEN (@TOKENS_L){
            my $QTOKEN = quotemeta( $TOKEN );
            if( $option{ADD_SPACE} ){
                $chunk =~ s/[ \n]*$QTOKEN/$TOKEN /g;
            }
            else{
                $chunk =~ s/[ \n]*$QTOKEN/$TOKEN/g;
            }

        }

        foreach my $TOKEN (@TOKENS_R){
            my $QTOKEN = quotemeta( $TOKEN );
            if( $option{ADD_SPACE} ){
                $chunk =~ s/$QTOKEN[ \n]*/ $TOKEN/g;
            }
            else{
                $chunk =~ s/$QTOKEN[ \n]*/$TOKEN/g;
            }
        }

        foreach my $TOKEN (@TOKENS_C){
            my $QTOKEN = quotemeta( $TOKEN );
            $chunk =~ s/[ \n]*$QTOKEN[ \n]*/$TOKEN/g;
        }

        foreach my $TOKEN (@TOKENS_D){
            my $QTOKEN = quotemeta( $TOKEN );
            $chunk =~ s/[ \n]*$QTOKEN[ \n]*/-/g;
        }

    }

    print $chunk;
}
