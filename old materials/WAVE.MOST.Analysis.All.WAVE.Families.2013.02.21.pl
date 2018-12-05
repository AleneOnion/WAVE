#!/usr/bin/perl
use Time::Local;
use IO::Handle;
use strict;
use warnings;
STDOUT->autoflush(1);
STDERR->autoflush(1);

# data to analyze will be in all_station.csv. Data will be printed in subset2_station.csv
open (ALL, "<all_samples.csv") or die "Can't open good file all_samples.csv";
open (SUBSET2, ">subset_samples.csv") or die "Can't create SUBSET2 file";

my ($all_rec);

# first read and skip past any initial comment lines
# this assumes that there are no comment lines mixed in with good records
#I had to remove this code because it doesn't record values below 0
while ($all_rec = <ALL>) {
	last if substr($all_rec,0,1) ne '#';
}
my ($samplespe, $sampleid, $species_id, $wqa, $bap, $CatID,$genspecies, $indiv, $prevsampleid, $junk, $parameter_diff);
my $insert_counter = 0;
$prevsampleid = 2;

chomp $all_rec; # it's always OK to chomp the same rec more than once; hurts nothing
($sampleid,$junk,$genspecies, $indiv, $wqa, $bap, $junk) = split(/,/, $all_rec);

print SUBSET2 "#SampleID,genspecies,indiv,WQA,BAP,CatID\n";

while (1) {
	# if no more good recs, end
	if (!defined $all_rec) {
		print "End of good file, inserted $insert_counter records\n";
                print "\n Make certain to remove duplicates (>genus/family)\n";
		close SUBSET2;
		close ALL;
		exit;
	}
#This section will pull out all records from families that have been found by WAVE volunteers
	chomp $all_rec; # it's always OK to chomp the same rec more than once; hurts nothing
	($sampleid,$junk,$genspecies, $indiv, $wqa, $bap, $junk) = split(/,/, $all_rec);

            #Aeshnidae
            if ($genspecies =~ m/(Anax|Basiaeschna|Boyeria|Aeshnidae)/i) {
                print SUBSET2 "$sampleid,Aeshnidae,$indiv,$wqa,$bap,0\n";
            }
            #Amphipoda
            if ($genspecies =~ m/(Gammarus|Gammaridae|Crangonyx|Crangonyctidae)/i) {
                print SUBSET2 "$sampleid,Amphipoda,$indiv,$wqa,$bap,0\n";
            }
            #Ancylidae
            if ($genspecies =~ m/(Ferrissia|Ancylidae)/i) {
                print SUBSET2 "$sampleid,Ancylidae,$indiv,$wqa,$bap,0\n";
            }
            #Asellidae
            if ($genspecies =~ m/(Caecidotea|Lirceus|Asellidae)/i) {
                print SUBSET2 "$sampleid,Asellidae,$indiv,$wqa,$bap,0\n";
            }
            #Athericidae
            if ($genspecies =~ m/(Atherix|Athericidae)/i) {
                print SUBSET2 "$sampleid,Athericidae,$indiv,$wqa,$bap,1\n";
            }
            #Baetidae
            if ($genspecies =~ m/(Acentrella|Acerpenna|Baetis|Callibaetis|Centroptilum|Cloeon|Diphetor|Heterocloeon|Iswaeon|Plauditus|Procloeon|Pseudocloeon|Baetidae)/i) {
                print SUBSET2 "$sampleid,Baetidae,$indiv,$wqa,$bap,0\n";
            }
            #Bithyniidae
            if ($genspecies =~ m/(Bithynia|Bithyniidae)/i) {
                print SUBSET2 "$sampleid,Bithyniidae,$indiv,$wqa,$bap,0\n";
            }
            #Brachycentridae
            if ($genspecies =~ m/(Adicrophleps|Brachycentrus|Micrasema|Brachycentridae)/i) {
                print SUBSET2 "$sampleid,Brachycentridae,$indiv,$wqa,$bap,2\n";
            }
            #Caenidae
            if ($genspecies =~ m/(Brachycercus|Caenis|Caenidae)/i) {
                print SUBSET2 "$sampleid,Caenidae,$indiv,$wqa,$bap,3\n";
            }
            #Calopterygidae
            if ($genspecies =~ m/(Calopteryx|Hetaerina|Calopterygidae)/i) {
                print SUBSET2 "$sampleid,Calopterygidae,$indiv,$wqa,$bap,0\n";
            }
            #Cambaridae
            if ($genspecies =~ m/(Cambarus|Orconectes|Cambaridae)/i) {
                print SUBSET2 "$sampleid,Cambaridae,$indiv,$wqa,$bap,0\n";
            }
            #Chironomidae
            if ($genspecies =~ m/(Ablabesmyia|Acricotopus|Alotanypus|Apsectrotanypus|Axarus|Brillia|Brundiniella|Camptocladius|Cladopelma|Cladotanytarsus|Clinotanypus|Coelotanypus|Conchapelopia|Constempellina|Corynoneura|Cricotopus|Cryptochironomus|Cryptotendipes|Demicryptochironomus|Diamesa|Dicrotendipes|Diplocladius|Einfeldia|Endochironomus|Epoicocladius|Eukiefferiella|Glyptotendipes|Goeldichironomus|Guttipelopia|Gymnometriocnemus|Harnischia|Hayesomyia|Heleniella|Helopelopia|Heterotrissocladius|Hudsonimyia|Hydrobaenus|Hyporhygma|Krenopelopia|Krenosmittia|Labrundinia|Larsia|Lauterborniella|Limnophyes|Lopescladius|Macropelopia|Meropelopia|Microchironomus|Micropsectra|Microtendipes|Monodiamesa|Nanocladius|Natarsia|Neostempellina|Nilotanypus|Nilothauma|Odontomesa|Orthocladiinae|Orthocladius|Pagastia|Pagastiella|Paraboreochlus|Parachaetocladius|Parachironomus|Paracladopelma|Paracricotopus|Parakiefferiella|Paralauterborniella|Paralimnophyes|Paramerina|Parametriocnemus|Paraphaenocladius|Paratanytarsus|Paratendipes|Paratrichocladius|Parorthocladius|Pentaneura|Phaenopsectra|Polypedilum|Potthastia|Procladius|Prodiamesa|Psectrocladius|Psectrotanypus|Pseudochironomus|Pseudokiefferiella|Pseudorthocladius|Psilometriocnemus|Rheocricotopus|Rheopelopia|Rheotanytarsus|Rheotanytarsus|Robackia|Saetheria|Sergentia|Smittia|Stelechomyia|Stempellina|Stempellinella|Stenochironomus|Stictochironomus|Stilocladius|Sublettea|Symbiocladius|Sympotthastia|Synorthocladius|Tanypus|Tanytarsus|Telopelopia|Thienemanniella|Thienemannimyia|Tribelos|Trissocladius|Trissopelopia|Tvetenia|Chironomidae|Chironomini|Diamesinae|Orthocladiinae|Prodiamesinae|Tanypodinae|Tanytarsini|Unniella|Xenochironomus|Xylotopus|Zalutschia|Zavrelia|Zavreliella|Zavrelimyia)/i) {
                print SUBSET2 "$sampleid,Chironomidae,$indiv,$wqa,$bap,0\n";
            }
            #Chironomus
            if ($genspecies =~ m/(Chironomus)/i) {
                print SUBSET2 "$sampleid,Chironomus,$indiv,$wqa,$bap,0\n";
            }
            #Chloroperlidae
            if ($genspecies =~ m/(Alloperla|Haploperla|Rasvena|Suwallia|Sweltsa|Chloroperlidae)/i) {
                print SUBSET2 "$sampleid,Chloroperlidae,$indiv,$wqa,$bap,4\n";
            }
            #Coenagrionidae
            if ($genspecies =~ m/(Argia|Chromagrion|Enallagma|Ischnura|Coenagrionidae)/i) {
                print SUBSET2 "$sampleid,Coenagrionidae,$indiv,$wqa,$bap,0\n";
            }
            #Cordulegastridae
            if ($genspecies =~ m/(Cordulegaster|Cordulegastridae)/i) {
                print SUBSET2 "$sampleid,Cordulegastridae,$indiv,$wqa,$bap,0\n";
            }
            #Corduliidae
            if ($genspecies =~ m/(Neurocordulia|Corduliidae)/i) {
                print SUBSET2 "$sampleid,Corduliidae,$indiv,$wqa,$bap,0\n";
            }
            #Corixidae
            if ($genspecies =~ m/(Hesperocorixa|Corixidae)/i) {
                print SUBSET2 "$sampleid,Corixidae,$indiv,$wqa,$bap,0\n";
            }
            #Corydalidae
            if ($genspecies =~ m/(Chauliodes|Corydalus|Nigronia|Corydalidae)/i) {
                print SUBSET2 "$sampleid,Corydalidae,$indiv,$wqa,$bap,5\n";
            }
            #Curculionidae
            if ($genspecies =~ m/(Curculionidae)/i) {
                print SUBSET2 "$sampleid,Curculionidae,$indiv,$wqa,$bap,0\n";
            }
            #Dixidae
            if ($genspecies =~ m/(Dixa|Dixidae)/i) {
                print SUBSET2 "$sampleid,Dixidae,$indiv,$wqa,$bap,0\n";
            }
            #Dryopidae
            if ($genspecies =~ m/(Dryopidae|Helichus)/i) {
                print SUBSET2 "$sampleid,Dryopidae,$indiv,$wqa,$bap,0\n";
            }
            #Dytiscidae
            if ($genspecies =~ m/(Agabetes|Agabus|Celina|Hydroporous|Laccophilus|Dytiscidae)/i) {
                print SUBSET2 "$sampleid,Dytiscidae,$indiv,$wqa,$bap,0\n";
            }
            #Elmidae
            if ($genspecies =~ m/(Ancyronyx|Dubiraphia|Macronychus|Microcylloepus|Optioservus|Oulimnius|Promoresia|Stenelmis|Elmidae)/i) {
                print SUBSET2 "$sampleid,Elmidae,$indiv,$wqa,$bap,0\n";
            }
            #Empididae
            if ($genspecies =~ m/(Chelifera|Clinocera|Hemerodromia|Neoplasta|Oreogeton|Empididae|Wiedemannia)/i) {
                print SUBSET2 "$sampleid,Empididae,$indiv,$wqa,$bap,0\n";
            }
            #Ephemerellidae
            if ($genspecies =~ m/(Attenella|Dannella|Ephemerella|Eurylophella|Serratella|Ephemerellidae)/i) {
                print SUBSET2 "$sampleid,Ephemerellidae,$indiv,$wqa,$bap,6\n";
            }
            #Ephemeridae
            if ($genspecies =~ m/(Ephemera|Hexagenia|Litobrancha|Ephemeridae)/i) {
                print SUBSET2 "$sampleid,Ephemeridae,$indiv,$wqa,$bap,7\n";
            }
            #Glossosomatidae
            if ($genspecies =~ m/(Agapetus|Culoptila|Glossosoma|Protoptila|Glossosomatidae)/i) {
                print SUBSET2 "$sampleid,Glossosomatidae,$indiv,$wqa,$bap,8\n";
            }
            #Goeridae
            if ($genspecies =~ m/(Goera|Goeridae)/i) {
                print SUBSET2 "$sampleid,Goeridae,$indiv,$wqa,$bap,0\n";
            }
            #Gomphidae
            if ($genspecies =~ m/(Gomphus|Hagenius|Lanthus|Ophiogomphus|Stylogomphus|Stylurus|Gomphidae)/i) {
                print SUBSET2 "$sampleid,Gomphidae,$indiv,$wqa,$bap,9\n";
            }
            #Gyrinidae
            if ($genspecies =~ m/(Dineutus|Gyrinus|Gyrinidae)/i) {
                print SUBSET2 "$sampleid,Gyrinidae,$indiv,$wqa,$bap,0\n";
            }
            #Haliplidae
            if ($genspecies =~ m/(Haliplus|Peltodytes|Haliplidae)/i) {
                print SUBSET2 "$sampleid,Haliplidae,$indiv,$wqa,$bap,0\n";
            }
            #Helicopsychidae
            if ($genspecies =~ m/(Helicopsyche|Helicopsyche|Helicopsychidae)/i) {
                print SUBSET2 "$sampleid,Helicopsychidae,$indiv,$wqa,$bap,10\n";
            }
            #Heptageniidae
            if ($genspecies =~ m/(Cinygmula|Epeorus|Heptagenia|Leucrocuta|Nixe|Rhithrogena|Stenacron|Stenonema|Heptageniidae)/i) {
                print SUBSET2 "$sampleid,Heptageniidae,$indiv,$wqa,$bap,11\n";
            }
            #HIRUDINEA
            if ($genspecies =~ m/(Batracobdella|Erpobdella|Helobdella|Placobdella|Erpobdellidae|Glossiphoniidae|Hirudinea)/i) {
                print SUBSET2 "$sampleid,HIRUDINEA,$indiv,$wqa,$bap,0\n";
            }
            #Hydraenidae
            if ($genspecies =~ m/(Hydraena|Hydraenidae)/i) {
                print SUBSET2 "$sampleid,Hydraenidae,$indiv,$wqa,$bap,0\n";
            }
            #Hydrophilidae
            if ($genspecies =~ m/(Anacaena|Berosus|Crenitis|Helochares|Helophorus|Hydrobius|Hydrochara|Hydrochus|Laccobius|Hydrophilidae)/i) {
                print SUBSET2 "$sampleid,Hydrophilidae,$indiv,$wqa,$bap,0\n";
            }
            #Hydropsychidae
            if ($genspecies =~ m/(Arctopsyche|Cheumatopsyche|Diplectrona|Homoplectra|Hydropsyche|Macrostemum|Parapsyche|Potamyia|Hydropsychidae)/i) {
                print SUBSET2 "$sampleid,Hydropsychidae,$indiv,$wqa,$bap,0\n";
            }
            #Hydroptilidae
            if ($genspecies =~ m/(Agraylea|Alisotrichia|Hydroptila|Ithytrichia|Leucotrichia|Mayatrichia|Neotrichia|Orthotrichia|Oxyethira|Palaeagapetus|Hydroptilidae)/i) {
                print SUBSET2 "$sampleid,Hydroptilidae,$indiv,$wqa,$bap,12\n";
            }
            #Isonychiidae
            if ($genspecies =~ m/(Isonychia|Isonychiidae)/i) {
                print SUBSET2 "$sampleid,Isonychiidae,$indiv,$wqa,$bap,13\n";
            }
            #Lepidostomatidae
            if ($genspecies =~ m/(Lepidostoma|Lepidostomatidae)/i) {
                print SUBSET2 "$sampleid,Lepidostomatidae,$indiv,$wqa,$bap,14\n";
            }
            #Leptoceridae
            if ($genspecies =~ m/(Ceraclea|Leptocerus|Mystacides|Nectopsyche|Oecetis|Setodes|Triaenodes|Leptoceridae)/i) {
                print SUBSET2 "$sampleid,Leptoceridae,$indiv,$wqa,$bap,0\n";
            }
            #Leptohyphidae
            if ($genspecies =~ m/(Tricorythodes|Leptohyphidae)/i) {
                print SUBSET2 "$sampleid,Leptohyphidae,$indiv,$wqa,$bap,15\n";
            }
            #Leptophlebiidae
            if ($genspecies =~ m/(Choroterpes|Habrophlebia|Habrophlebiodes|Leptophlebia|Paraleptophlebia|Leptophlebiidae)/i) {
                print SUBSET2 "$sampleid,Leptophlebiidae,$indiv,$wqa,$bap,16\n";
            }
            #Lestidae
            if ($genspecies =~ m/(Lestes|Lestidae)/i) {
                print SUBSET2 "$sampleid,Lestidae,$indiv,$wqa,$bap,0\n";
            }
            #Leuctridae
            if ($genspecies =~ m/(Leuctra|Leuctridae|Zealeuctra)/i) {
                print SUBSET2 "$sampleid,Leuctridae,$indiv,$wqa,$bap,17\n";
            }
            #Limnephilidae
            if ($genspecies =~ m/(Hesperophylax|Hydatophylax|Limnephilus|Nemotaulius|Platycentropus|Pseudostenophylax|Psychoglypha|Pycnopsyche|Limnephilidae)/i) {
                print SUBSET2 "$sampleid,Limnephilidae,$indiv,$wqa,$bap,0\n";
            }
            #Lymnaeidae
            if ($genspecies =~ m/(Fossaria|Lymnaea|Pseudosuccinea|Radix|Stagnicola|Lymnaeidae)/i) {
                print SUBSET2 "$sampleid,Lymnaeidae,$indiv,$wqa,$bap,0\n";
            }
            #Macromiidae
            if ($genspecies =~ m/(Macromia|Macromiidae)/i) {
                print SUBSET2 "$sampleid,Macromiidae,$indiv,$wqa,$bap,0\n";
            }
            #Odontoceridae
            if ($genspecies =~ m/(Psilotreta|Odontoceridae)/i) {
                print SUBSET2 "$sampleid,Odontoceridae,$indiv,$wqa,$bap,18\n";
            }
            #OLIGOCHAETA
            if ($genspecies =~ m/(Amphichaeta|Arcteonais|Aulodrilus|Bothrioneurum|Branchiura|Chaetogaster|Dero|Eclipidrilus|Haemonais|Ilyodrilus|Isochaetides|Limnodrilus|Nais|Ophidonais|Paranais|Piguetiella|Potamothrix|Pristina|Pristinella|Quistadrilus|Rhyacodrilus|Ripistes|Slavina|Specaria|Spirosperma|Stylaria|Stylodrilus|Tubifex|Undet. Tubificidae|Enchytraeidae|Enchytraeidae|Haplotaxidae|Lumbricina|Lumbriculidae|Naididae|Vejdovskyella)/i) {
                print SUBSET2 "$sampleid,OLIGOCHAETA,$indiv,$wqa,$bap,0\n";
            }
            #OSTRACODA
            if ($genspecies =~ m/(Crustacean|Ostracoda)/i) {
                print SUBSET2 "$sampleid,OSTRACODA,$indiv,$wqa,$bap,0\n";
            }
            #PELECYPODA
            if ($genspecies =~ m/(Anodonta|Corbicula|Dreissena|Elliptio|Lampsilis|Musculium|Pisidium|Pyganodon|Sphaerium|Corbiculidae|Dreisseniidae|Sphaeriidae|Unionidae)/i) {
                print SUBSET2 "$sampleid,PELECYPODA,$indiv,$wqa,$bap,0\n";
            }
            #Peltoperlidae
            if ($genspecies =~ m/(Tallaperla|Peltoperlidae)/i) {
                print SUBSET2 "$sampleid,Peltoperlidae,$indiv,$wqa,$bap,19\n";
            }
            #Perlidae
            if ($genspecies =~ m/(Acroneuria|Agnetina|Claassenia|Eccoptura|Neoperla|Paragnetina|Perlesta|Perlidae)/i) {
                print SUBSET2 "$sampleid,Perlidae,$indiv,$wqa,$bap,20\n";
            }
            #Perlodidae
            if ($genspecies =~ m/(Cultus|Cultus|Diura|Helopicus|Isogenoides|Isoperla|Malirekus|Remenus|Perlodidae)/i) {
                print SUBSET2 "$sampleid,Perlodidae,$indiv,$wqa,$bap,21\n";
            }
            #Philopotamidae
            if ($genspecies =~ m/(Chimarra|Dolophilodes|Philopotamidae|Wormaldia)/i) {
                print SUBSET2 "$sampleid,Philopotamidae,$indiv,$wqa,$bap,22\n";
            }
            #Phryganeidae
            if ($genspecies =~ m/(Oligostomis|Phryganea|Ptilostomis|Phryganeidae)/i) {
                print SUBSET2 "$sampleid,Phryganeidae,$indiv,$wqa,$bap,0\n";
            }
            #Physidae
            if ($genspecies =~ m/(Physella|Physidae)/i) {
                print SUBSET2 "$sampleid,Physidae,$indiv,$wqa,$bap,0\n";
            }
            #Turbellaria
            if ($genspecies =~ m/(Dugesia|Dugesia|Nematoda|Turbellaria)/i) {
                print SUBSET2 "$sampleid,Turbellaria,$indiv,$wqa,$bap,0\n";
            }
            #Polycentropodidae
            if ($genspecies =~ m/(Cyrnellus|Cyrnellus|Neureclipsis|Nyctiophylax|Polycentropus|Polycentropodidae)/i) {
                print SUBSET2 "$sampleid,Polycentropodidae,$indiv,$wqa,$bap,23\n";
            }
            #Polymitarcyidae
            if ($genspecies =~ m/(Ephoron|Ephoron|Polymitarcyidae)/i) {
                print SUBSET2 "$sampleid,Polymitarcyidae,$indiv,$wqa,$bap,24\n";
            }
            #Potamanthidae
            if ($genspecies =~ m/(Anthopotamus|Potamanthidae)/i) {
                print SUBSET2 "$sampleid,Potamanthidae,$indiv,$wqa,$bap,25\n";
            }
            #Psephenidae
            if ($genspecies =~ m/(Ectopria|Ectopria|Psephenus|Psephenidae)/i) {
                print SUBSET2 "$sampleid,Psephenidae,$indiv,$wqa,$bap,26\n";
            }
            #Psychomyiidae
            if ($genspecies =~ m/(Lype|Psychomyia|Psychomyiidae)/i) {
                print SUBSET2 "$sampleid,Psychomyiidae,$indiv,$wqa,$bap,0\n";
            }
            #Pteronarcidae
            if ($genspecies =~ m/(Pteronarcys|Plecoptera|Pteronarcidae)/i) {
                print SUBSET2 "$sampleid,Pteronarcidae,$indiv,$wqa,$bap,27\n";
            }
            #Ptilodactylidae
            if ($genspecies =~ m/(Anchytarsus|Ptilodactylidae)/i) {
                print SUBSET2 "$sampleid,Ptilodactylidae,$indiv,$wqa,$bap,0\n";
            }
            #Rhyacophilidae
            if ($genspecies =~ m/(Rhyacophila|Rhyacophilidae)/i) {
                print SUBSET2 "$sampleid,Rhyacophilidae,$indiv,$wqa,$bap,28\n";
            }
            #Sialidae
            if ($genspecies =~ m/(Sialis|Sialidae)/i) {
                print SUBSET2 "$sampleid,Sialidae,$indiv,$wqa,$bap,0\n";
            }
            #Simuliidae
            if ($genspecies =~ m/(Cnephia|Prosimulium|Simulium|Stegopterna|Simuliidae)/i) {
                print SUBSET2 "$sampleid,Simuliidae,$indiv,$wqa,$bap,0\n";
            }
            #Tabanidae
            if ($genspecies =~ m/(Chrysops|Hybomitra|Tabanus|Tabanidae)/i) {
                print SUBSET2 "$sampleid,Tabanidae,$indiv,$wqa,$bap,0\n";
            }
            #Talitridae
            if ($genspecies =~ m/(Hyalella|Hyalella|Talitridae)/i) {
                print SUBSET2 "$sampleid,Talitridae,$indiv,$wqa,$bap,0\n";
            }
            #Tipulidae
            if ($genspecies =~ m/(Antocha|Dicranota|Helius|Hesperoconopa|Hexatoma|Limnophila|Limonia|Molophilus|Ormosia|Pedicia|Pilaria|Pseudolimnophila|Tipula|Ulomorpha|Tipulidae)/i) {
                print SUBSET2 "$sampleid,Tipulidae,$indiv,$wqa,$bap,0\n";
            }
            #Uenoidae
            if ($genspecies =~ m/(Neophylax|Uenoidae)/i) {
                print SUBSET2 "$sampleid,Uenoidae,$indiv,$wqa,$bap,29\n";
            }
            #Valvatidae
            if ($genspecies =~ m/(Valvatidae|Valvata)/i) {
                print SUBSET2 "$sampleid,Valvatidae,$indiv,$wqa,$bap,0\n";
            }
            else {};
        $all_rec = <ALL>; #moves to the next record
        $insert_counter++;
}
