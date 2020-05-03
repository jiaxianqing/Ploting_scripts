#!/usr/bin/perl -w

#   block2draw.pl -- transform blocks 2 R draw
#
#   Author: Xianqing Jia
#   Created: 2018-08-22
#   Version: 1.0.18.08.22
#   Updated: 1.0.18.08.22 initial version
#            1.0.19.01.15 add centromere and gene option

use strict;
use Getopt::Long;

my $CMDLINE = "perl $0 @ARGV";
my $VERSION = '1.0.18.08.22';
my $HEADER  = "##$CMDLINE\n##Version: $VERSION\n";

my %options = ();
my %bed = ();
my ($block, $sort, $distance, $genefile, $centfile, $thickness, $output, $pos, $show_help);
GetOptions(
            "b|block=s"            => \$block,
            "s|sort=s"             => \$sort,
            
            "g|gene=s"             => \$genefile,
            "c|centromere=s"       => \$centfile,
            
            "d|distance=i"         => \$distance,
            "t|thickness=i"        => \$thickness,
            
            "p|position=s"         => \$pos,
            "o|output=s"           => \$output,
            
            "help|?"               => \$show_help,
           );

unless( !$show_help && $block ) {
    print <<EOF;
$0  transform vcf file to r/qtl format.

Version: $VERSION

Usage:   perl $0 --vcf <vcf filename> --phenotype <phenotype filename> --genome <genome filename>

Note:   all the range is :[a,b)

Options:
    -b, --block <filename>
        required
    -s, --sort <filename>
        samples' order, required. e.g.
        #Sample	Days
        DE55	65
            or
        #Sample
        DE55
    
    -g, --gene <filename>
        gene list and position. e.g.
        chr01   1   2000    sd1
    -c, --centromere <filename>
        centromere position. e.g.
        chr01	15977962	17876674
    
    -d, --distance <int>
        distance between two sample. [5]
    -t, --thickness <int>
        block thickness. [5]
    
    -p, --position <filename>
        output position file of samples in the ordinate.
    -o, --output   <filename>
        output filename, default to STDOUT
    
    -?, --help
        show this help message

EOF

    exit(1);
}

$|++;


print STDERR "\n# $0 v$VERSION\n# " . (scalar localtime()) . "\n\n";

if ($output) {
    open (STDOUT, "> $output") || die $!;
}
print STDERR ">> Start to process: $block\n\n";
####################################################### MAIN ###########################################################
$thickness = 5 if (!$thickness);
$distance = 5 if (!$distance);

print STDOUT "Sample\tTag\tChrom\tType\tXmin\tXmax\tYmin\tYmax\n";
# samples order
my @sample = ();

if($genefile){
    push @sample, "Gene";
}
if($centfile){
    push @sample, "Centromere";
}
my %type = ();
open SORT, "<$sort";
while (<SORT>){
    chomp;
    next if (/^#/);
    my @line = split /\t/, $_;
    push @sample, $line[0];
    if($line[1]){
        $type{$line[0]} = $line[1];
    }
}
close SORT;
# y position

open POS, ">$pos";
my %ylab = ();
my $ymin = 0;
for(my $i = $#sample; $i >=0; $i --){
    my $ymax = $ymin + $thickness;
    $ylab{$sample[$i]} = "$ymin\t$ymax";
    my $mean = ($ymin + $ymax) / 2;
    print POS "$sample[$i]\t$mean\n";
    $ymin = $ymax + $distance;
}
if($genefile){
    open IN, "<$genefile";
    while(<IN>){
        chomp;
        next if (/^#/);
        my @line = split /\t/, $_;
        my $type = "Gene";
        print STDOUT "$type\t$line[3]\t$line[0]\t$type\t$line[1]\t$line[2]\t$ylab{$type}\n";
    }
}
if($centfile){
    open IN, "<$centfile";
    while(<IN>){
        chomp;
        next if (/^#/);
        my @line = split /\t/, $_;
        my $type = "Centromere";
        print STDOUT "$type\t$type\t$line[0]\t$type\t$line[1]\t$line[2]\t$ylab{$type}\n";
    }
}

# read blocks
open BLOCK, "<$block";
while (<BLOCK>){
    chomp;
    next if (/^#/);
    my @line = split /\t/, $_;
    if($ylab{$line[0]}){
        if($type{$line[0]}){
            print STDOUT "$line[0]\t$type{$line[0]}\t$line[1]\t$line[2]\t$line[3]\t$line[4]\t$ylab{$line[0]}\n";
        }else{
            print STDOUT "$line[0]\tSample\t$line[1]\t$line[2]\t$line[3]\t$line[4]\t$ylab{$line[0]}\n";
        }
    }
}
close BLOCK;


#Sample	Chrom	Type	Xmin	Xmax	Ymin	Ymax
#DE10	chr01	D/N	1	2406951	600	605
#DE10	chr01	N/N	2406952	2422395	600	605
#DE10	chr01	D/N	2422402	4177942	600	605






####################################################### SUB ###########################################################
