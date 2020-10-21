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


# detect possible abbrev not at start of para
# -L <file> look for tags in file but not in .fisml file

# if {'}{''} seen should recommend {'''}, etc
# or {'}''
# or report '''+ or ```+ on line x
# but only check chunks between tags
# and detect '{''} -> warning, may not be an error

# .config/fisml/lint.conf
# !$$||{FOO}|| something that should not occure at the end of
#              a para
# !^^||{FOO}|| something that should not occure at the beg of
#              a para
# !{FOO}{BAR}  two tags that should never appear together
#              or with spaces between
# can't be used with stdin
#
# check first part at line end
# set flag
# check rest at start of lie
# if and flag -> problem? (difficult if more than two tags)

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use Getopt::Long;

use constant OFF => 0;
use constant ON => 1;

use constant FALSE => 1==0;
use constant TRUE => not FALSE;
use constant OTHER_TRUTH => 3;

my $FISML_LINE_WIDTH=63; #+1 for newline

my %opts;
$opts{CHECK_AGAINST} = OFF;
$opts{UNABBREV} = OFF;
$opts{SKIP_PAIR_CHECK} = OFF;

# alt config file option
# pass to load_fisml_conf()
# load_fisml_conf searches ~/.fisml.conf, ~/.config/fisml/fisml.conf

my %CHECKS;

GetOptions( "L=s" => \my @FILES,
            "skip-pair-check" => \$opts{SKIP_PAIR_CHECK},
            "U" => \$opts{UNABBREV} ) or die "Error in command line";

if( @FILES ){
    
    $opts{CHECK_AGAINST} = ON;

    #comment implicit
    $CHECKS{"{!!}"} = "{!!}";
    
    foreach my $file ( @FILES ) {

        if( $file eq "" ){
            next;
        }

        open(my $spec_file, '<', $file) or die "Couldn't open spec file: $file";

        my @tmp;

        chomp( @tmp = <$spec_file> );
        
        foreach my $t (@tmp){
            if( $t !~ m/^(\{.*?\})\|(e*)\|(.*)\|\|/ ){
                next;
            }
            $CHECKS{$1} = $1;
        }

        close $spec_file;
    }
}

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

# if one tag is out of order, everything past that can't be
# trusted, so ignore
# both these have to be global to account for crossing
# multiple lines
my $PAIR_IGNORE_FURTHER = FALSE;
my @PAIR_TAG_PAIRS = ();

my $linen = 0;

my $in_para = FALSE;
my $comment = FALSE;

sub PROCESS_END_TAG {
    my $CTAG = shift;
    my $FRAG = shift;
    my $LN = shift;
    
    if( @PAIR_TAG_PAIRS == 0 ){
        print "line: $LN, tag: $FRAG closes what isn't open.\n";
        print "===FURTHER CHECKING OF THIS DISABLED===\n";
        $PAIR_IGNORE_FURTHER = TRUE;
    }
    elsif( $CTAG ne $PAIR_TAG_PAIRS[-1] ){
        print "line: $LN, tag: $FRAG closes out of order.\n";
        print "===FURTHER CHECKING OF THIS DISABLED===\n";
        $PAIR_IGNORE_FURTHER = TRUE;
    }
    else{
        pop(@PAIR_TAG_PAIRS);
    }
}

while( my $line = <$input_file> ){

    $linen++;

    # comment lines are allowed to cluster
    # so one after the other doesn't need
    # any special attention
    # and blank lines after are only required
    # if a non-comment line follows
    if( $line =~ m/^\{\!\!\}/ ){
        $comment = TRUE;
        if( (length($line) - 1) > $FISML_LINE_WIDTH ){
        print "line: $linen, (comment) > $FISML_LINE_WIDTH columns\n";
    }
        next;
    }
    elsif( $line !~ m/^$/ and $comment ){
        print "line: " . $linen . ", line following comment isn't blank.\n";
        $comment = FALSE;
    }
    # ignore valid blank line after comment
    # so it's not counted as an extra blank line
    elsif( $line =~ m/^$/ and $comment ){
        $comment = FALSE;
        next;
    }
    else{
        $comment = FALSE;
    }

    if( $line =~ m/\{\!\!\}/ ){
        print "line: " . $linen. ", comment not at beginning of line.\n";
    }

    #1st line of new para
    if( $line !~ m/^$/ and !$in_para ){
        $in_para = TRUE;
    }
    #somewhere in para but not 1st line
    elsif( $line !~ m/^$/ and $in_para ){
        $in_para = OTHER_TRUTH;
    }
    # extra newlines between paras
    elsif( $line =~ m/^$/ and !$in_para ){
        print "line: $linen, extra blank line between paragraphs.\n";
        # gets confused when comments are removed
    }
    #end of para
    elsif( $line =~ m/^$/ and $in_para ){
        $in_para = FALSE;
    }

    if( $in_para ){
        my $t = $line;
        if( $in_para != OTHER_TRUTH ){
            $t =~ s/^\{.+?#\}//;
        }
        if( $t =~ m/\{.+?#\}/ ){
            print "line: " . $linen . ", apparent abbreviated tag not at beginning of paragraph". "\n";
        }
    }

    # if FISML_SENTENCE_START
    if( index($line, "“{}") > -1 or
        index($line, "{``}{}") > -1 or
        index($line, "‘{}") > -1 or
        index($line, "{`}{}") > -1 ){
        print "line: $linen, open quote precedes start of sentence.\n";
    }
    elsif( $line =~ m/[^ \n]\{\}/ ){
        print "line: $linen, non-space precedes start of sentence.\n";
    }
    if( $line =~ m/\{\}[ \t\n]/ ){
        print "line: $linen, start of sentence followed by blank space.\n";
    }
    if( $line =~ m/\{\}\{\}/ ){
        print "line: $linen, start of sentence followed by start of sentence.\n";
    }

    if( $line =~ m/\{\}[a-z]/ ){
        print "line: $linen, start of sentence followed by lowercase char.\n";
    }

    if( $line =~ m/\.\{---\"\}/ ){
        print "line: $linen, {---\"} preceded by '.' char.\n";
    }

    if( (length($line) - 1) > $FISML_LINE_WIDTH ){
        print "line: $linen, > $FISML_LINE_WIDTH columns\n";
    }

    my $expect = "{";
    my $coff = 0;
    foreach my $c ( split(//, $line)){
	if( $c eq "{" or $c eq "}" ){
	    if( $c eq $expect ){
		if( $c eq "{" ){
		    $expect = "}";
		}
		else{
		    $expect = "{";
		}
	    }
	    else{
		if( $c eq "{" ){
                    print "line: $linen, offset: $coff, expected: '}', got: '{'\n"
		}
		else{
                    print "line: $linen, offset: $coff, expected: '{', got: '}'\n"
		}
	    }
	}
	
	$coff++;
	
    }

    # in 1st paragraph
    if( $in_para and $in_para != OTHER_TRUTH
        and $opts{UNABBREV} and $opts{CHECK_AGAINST} ){
        if( $line =~ m/^(\{.+?#\})/ ){
            my $a = $1;
            $line =~ s/^\{.+?#\}//;
            my @tagP;
            $tagP[0] = $a;
            $tagP[1] = $a;
            $tagP[0] =~ s/#\}$/>}/;
            $tagP[1] =~ s/#\}$/}/;
            $tagP[1] =~ s/^\{/\{</;
            foreach my $t (@tagP){
                if( ! exists $CHECKS{$t} ){
                    print "line: $linen, tag: $t not found in inputs with -L.\n";
                }
            }
        }
    }

    my @fragged = split(/(\{.*?\})/, $line);

    # .fisml
    # possible TAG_PAIR_EXCEPTIONS: {<<} & {>>}
    unless( $opts{SKIP_PAIR_CHECK} ){
        foreach my $frag (@fragged) {
            if( $frag =~ m/^\{.*?\}$/ ){
                if( $frag =~ m/[\n\t ]+/ ){
                    print "line: $linen, tag: $frag has blank space in tag\n";
                }
                if( $PAIR_IGNORE_FURTHER != TRUE ){
                    # {<VIDEO>} style tages
                    if( $frag =~ m/^\{\<(.+?)\>\}$/ ){
                        PROCESS_END_TAG($1, $frag, $linen);
                        push(@PAIR_TAG_PAIRS, $1);
                    }
                    elsif( $frag =~ m/^\{(.+?)\>\}$/ ){
                        push(@PAIR_TAG_PAIRS, $1);
                    }
                    elsif( $frag =~ m/^\{\<(.+?)\}$/ ){
                        PROCESS_END_TAG($1, $frag, $linen);
                    }
                }
                if( $opts{CHECK_AGAINST} ){
                    if( ! exists $CHECKS{$frag} ){
                        print "line: $linen, tag: $frag not found in inputs with -L.\n";
                    }
                }
            }
        }
    }    
}
