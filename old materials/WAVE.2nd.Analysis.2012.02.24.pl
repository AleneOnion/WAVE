#!/usr/bin/perl
use Time::Local;
use IO::Handle;
use strict;
use warnings;
STDOUT->autoflush(1);
STDERR->autoflush(1);

# data to analyze will be in subset2_station.csv. Data will be printed in overlap_station.csv
open (SUBSET, "<subset_samples.csv") or die "Can't open good file subset2_samples.csv";
open (OVERLAP, ">overlap_samples.csv") or die "Can't create OVERLAP file";

my ($subset_rec);

# first read and skip past any initial comment lines
# this assumes that there are no comment lines mixed in with good records
#I had to remove this code because it doesn't record values below 0
while ($subset_rec = <SUBSET>) {
	last if substr($subset_rec,0,1) ne '#';
}
my ($sampleid, $genspecies, $wqa,$bap,$prevsampleid, $catid, $prevcatid, $junk, $parameter_diff, $i, $j);
my $insert_counter = 0;
$prevsampleid = 0;
$prevcatid = 0;
$i=0;
$j=0;

chomp $subset_rec; # it's always OK to chomp the same rec more than once; hurts nothing
($sampleid, $genspecies,$junk,$wqa,$bap,$catid) = split(/,/, $subset_rec);

print OVERLAP "'SampleId','GenSpecies','WQA','BAP','Catid','number'\n";

while (1) {
	# if no more good recs, end
	if (!defined $subset_rec) {
		print "End of good file, inserted $insert_counter records\n";
                print "\n I hope you sorted this by site/category or this didn't work\n";
		close OVERLAP;
		close SUBSET;
		exit;
	}

	chomp $subset_rec; # it's always OK to chomp the same rec more than once; hurts nothing
	($sampleid, $genspecies,$junk,$wqa, $bap, $catid) = split(/,/, $subset_rec);

	#need to make sure $i is the correct value to cue up the correct averages, sds, and range values.
	#to do this, I compare the parameter in the file to the internal parameter defined by the hash.
	if ($sampleid == $prevsampleid){
            if ($prevcatid == $catid) {
                print OVERLAP "$sampleid,$genspecies,$wqa,$bap,$catid,$i\n";
            }
            else {
                $i=$i+1;
                $prevcatid = $catid;
                print OVERLAP "$sampleid,$genspecies,$wqa,$bap,$catid,$i\n";
            }
        }
        else {
            if ($catid > 0){
		$i=1;
                $prevsampleid = $sampleid;
                $prevcatid = $catid;
                print OVERLAP "$sampleid,$genspecies,$wqa,$bap,$catid,$i\n";
	    }
	    elsif ($catid == 0){
		$i=0;
                $prevsampleid = $sampleid;
                $prevcatid = $catid;
                print OVERLAP "$sampleid,$genspecies,$wqa,$bap,$catid,$i\n";
	    }
		
        }
        $subset_rec = <SUBSET>; #moves to the next record
        $insert_counter++;
    }
