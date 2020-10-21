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

#
# fisml_decomment.pl should usually be run before this
# however, if it is, tags in the header information won't
# be detected
#
# to get all the tags for a final document run everything through the
# pipeline ( cat xx.txt | fisml_decomment.pl | fisml_footnoter.pl |
# fisml_tags.pl >xx.fisml )
#
# usage:
#
# fisml_tags [-g [-x xfile.fisml [-a]] [-m mfile.fisml]] file.txt
#
# -x will compare the already-extant tags in mfile.fisml
#    and will output those already extant tags instead of
#    blank ones.
# -g will send to standard output a blank .fisml file.
#    This is required for the -x or -m options.
# -a will output all tags reads with the -x option.
# -m will compare the already-extant tags in mfile.fisml
#    and will output those already extants tags instead
#    of blank ones and add any that don't exist in the
#    input to the output.
#
# Values read from -x override those read from -m

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use Symbol;

use Getopt::Long;

use constant OFF => 0;
use constant ON => 1;

my %opts;

$opts{XFILE} = "";
$opts{MFILE} = "";
$opts{GEN} = OFF;
$opts{ALL} = OFF;
$opts{UNABBREV} = OFF;

my %tags;
my %XTAGS;
my %MTAGS;

GetOptions( "x=s" => \$opts{XFILE},
	    "m=s" => \$opts{MFILE},
	    "g" => \$opts{GEN},
            "U" => \$opts{UNABBREV},
	    "a" => \$opts{ALL}) or die "Error in command line";

if( $opts{XFILE} ne "" and $opts{GEN} == OFF ){
    die "can't use -x without -g";
}
if( $opts{MFILE} ne "" and $opts{GEN} == OFF ){
    die "can't use -m without -g";
}
if( $opts{GEN} == OFF and $opts{ALL} != OFF ){
    die "-a used without -g and -x";
}
if( $opts{XFILE} eq "" and $opts{ALL} != OFF ){
    die "-a makes no sense without -x";
}

sub fill_hash {

    my $INFILE = shift;
    my %TAGS;
    my $XIN;
    
    open($XIN, '<', $INFILE) or die "Couldn't open input: $INFILE";

    while( my $spec = <$XIN> ){
	
	if( $spec !~ m/^(\{.*?\})\|e*\|.*\|\|/ ){
	    next;
	}

	chomp( $spec );

	$TAGS{$1} = $spec;
	
    }

    close( $XIN );

    return( %TAGS );

}


if( $opts{XFILE} ne "" and $opts{GEN} == ON){
    %XTAGS = fill_hash( $opts{XFILE} );
}
if( $opts{MFILE} ne "" and $opts{GEN} == ON){
    %MTAGS = fill_hash( $opts{MFILE} );
}

#use Unicode::Collate;

# we should only need to cmp byte-by-byte to
# get appropriate matches within a document
# so long as opening and closings are grouped,
# that's all that really matters

sub cmp_tag {

    my @ap = ($a =~ m/\{(<?)(.*?)(>?)\}/);
    my @bp = ($b =~ m/\{(<?)(.*?)(>?)\}/);

    my $cmp_state = $ap[1] cmp $bp[1];

    if( $cmp_state == 0 ){
    	if( $ap[2] eq ">" and $bp[0] eq "<"){
    	    return -1;
    	}
    	else{
    	    return 1;
    	}
    }
    # put placeholder tags at bottom
    elsif( $ap[1] =~ m/^__.*__$/ and $bp[1] !~ m/^__.*__$/ ){
	return 1;
    }
    elsif( $bp[1] =~ m/^__.*__$/ and $ap[1] !~ m/^__.*__$/ ){
	return -1;
    }
    else{
    	return $cmp_state;
    }
}

my @INPUTS;

if( @ARGV > 0 ){
    foreach my $arg (@ARGV){
	my $INPUT_FILE = gensym;
	open($INPUT_FILE, '<', $arg) or die "Couldn't open input: $arg";
	push( @INPUTS, $INPUT_FILE );
    }
}
else{
    push( @INPUTS, *STDIN );
}

foreach my $input (@INPUTS){
    while(my $line = <$input> ){

	if( $line =~ m/^\{\!\!\}/ ){
	    next;
	}

	my @fragged = split(/(\{.*?\})/, $line);

        #should this try to enforce that abbrev tags
        #only come at start of para?
	foreach my $frag (@fragged) {
	    if( $frag =~ m/^\{.*\}$/ ){
                if( $frag =~ m/^\{.+\#\}$/ and $opts{UNABBREV} != OFF ){
                    my @tagP;
                    $tagP[0] = $frag;
                    $tagP[1] = $frag;
                    $tagP[0] =~ s/\#}$/>\}/;
                    $tagP[1] =~ s/\#}$/\}/;
                    $tagP[1] =~ s/^\{/\{</;
                    foreach my $t (@tagP){
                        if( ! exists $tags{$t} ){
                            $tags{$t} = $t . "||||";
                        }
                    }
                }
                else{         
                    if( ! exists $tags{$frag} ){
                        $tags{$frag} = $frag . "||||";
                    }
                }
            }
        }
    }
}

foreach my $input (@INPUTS){
    close( $input );
}

if( $opts{MFILE} ne "" and $opts{GEN} == ON ){
    foreach my $key (keys %MTAGS){
	$tags{$key} = $MTAGS{$key};
    }
}
    
if( $opts{ALL} == ON and $opts{XFILE} ne "" and $opts{GEN} == ON ){
    foreach my $key (keys %XTAGS){
	$tags{$key} = $XTAGS{$key};
    }
}
elsif( $opts{ALL} == OFF and $opts{XFILE} ne "" and $opts{GEN} == ON ){
    foreach my $key (keys %tags){
	if( exists $XTAGS{$key} ){
	    $tags{$key} = $XTAGS{$key};
	}
    }
}
	   
my @key_array;

foreach my $key (keys %tags){
    push(@key_array, $key);
}

my @output_array = sort cmp_tag @key_array;

my $output_str="";

if( $opts{GEN} ){
    foreach my $k (@output_array){
        $output_str .= $tags{$k} . "\n"
    }
}
else{
    foreach my $k (@output_array){
        $output_str .= $k . "\n"
    }
}

print $output_str;
