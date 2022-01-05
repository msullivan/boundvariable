#!/usr/bin/perl -w

use strict;

# read in nouns.in


my @nouns;
my @record;

# read
open N, "nouns.in" or die "Can't open nouns.in";
while (<N>) {
    chomp;
    next if /^#/;

    if (/^---$/) {
	@record = ();
	next;
    }

    push @record, $_;
    if (@record == 2) {
	push @record, scalar @nouns;
	push @nouns, [@record];
    }
}
close N;

# write
open IN, "nouns.uh.in" or die "Can't open nouns.uh.in";
open OUT, ">nouns.uh" or die "Can't open nouns.uh for writing";

while (<IN>) {
    chomp;
    my $l = $_;
    
    if ($l eq "(* noun-dt *)") {
	print OUT "datatype noun = ";
	print OUT join ("\n              | ", map { $_->[0] } @nouns);
	print OUT "\n";
    } elsif ($l eq "(* noun-tostring *)") {
	print OUT "fun ";
	print OUT join ("\n  | ", 
			map { "noun-tostring $_->[0] = \"$_->[1]\"" }
			@nouns);
	print OUT "\n";
#    } elsif ($l eq "(* noun-equals *)") {
#	print OUT "fun ";
#	print OUT join ("\n  | ", 
#			map { "noun-equals ($_->[0], $_->[0]) = true" }
#			@nouns);
#	print OUT "\n  | noun-equals (_, _) = false\n";
    } elsif ($l eq "(* noun-toidx *)") {
	print OUT "fun ";
	print OUT join ("\n  | ", 
			map { "noun-toidx $_->[0] = $_->[2]" }
			@nouns);
	print OUT "\n";
    } elsif ($l eq "(* nouns *)") {
	print OUT "val nouns = {| ";
	print OUT join (", ", map { "$_->[0]" } @nouns);
	print OUT " |}\n";
    } elsif ($l eq "(* num_nouns *)") {
	print OUT "val num_nouns = ", scalar @nouns, "\n";
    } elsif ($l eq "(* add nouns *)") {
	print OUT "fun nouns-initialize () = \n  let in\n    ";
	
	print OUT join (";\n    ",
			map {
#			    "update (nouns, noun-toidx $_->[0], $_->[0]);\n".
                            "hash-put (noun_map, \"\L$_->[1]\E\", $_->[0])" }
			@nouns);

	print OUT "\n  end\n";
    } else {
	print OUT "$l\n";
    }
}

close IN;
close OUT;



# ADJECTIVES

# read in adjectives.in


my @adjectives;

# read
open N, "adjectives.in" or die "Can't open adjectives.in";
while (<N>) {
    chomp;
    next if /^#/;

    if (/^---$/) {
	@record = ();
	next;
    }

    push @record, $_;
    if (@record == 2) {
	push @record, scalar @adjectives;
	push @adjectives, [@record];
    }
}
close N;

# write
open IN, "adjectives.uh.in" or die "Can't open adjectives.uh.in";
open OUT, ">adjectives.uh" or die "Can't open adjectives.uh for writing";

while (<IN>) {
    chomp;
    my $l = $_;
    
    if ($l eq "(* adjective-dt *)") {
	print OUT "datatype adjective = ";
	print OUT join ("\n              | ", map { $_->[0] } @adjectives);
	print OUT "\n";
    } elsif ($l eq "(* adjective-tostring *)") {
	print OUT "fun ";
	print OUT join ("\n  | ", 
			map { "adjective-tostring $_->[0] = \"$_->[1]\"" }
			@adjectives);
	print OUT "\n";
    } elsif ($l eq "(* adjective-equals *)") {
#	print OUT "fun ";
#	print OUT join ("\n  | ", 
#			map { "adjective-equals ($_->[0], $_->[0]) = true" }
#			@adjectives);
#	print OUT "\n  | adjective-equals (_, _) = false\n";
	print OUT "fun adjective-equals (a, b) = adjective-toidx a = adjective-toidx b\n"
    } elsif ($l eq "(* adjective-toidx *)") {
	print OUT "fun ";
	print OUT join ("\n  | ", 
			map { "adjective-toidx $_->[0] = $_->[2]" }
			@adjectives);
	print OUT "\n";
    } elsif ($l eq "(* adjectives *)") {
	print OUT "val adjectives = {| ";
	print OUT join (", ",
			map { $_->[0] } @adjectives);
	print OUT " |}\n"
    } elsif ($l eq "(* num_adjectives *)") {
	print OUT "val num_adjectives = ", scalar @adjectives, "\n";
    } elsif ($l eq "(* add adjectives *)") {
	print OUT "fun adjectives-initialize () = \n  let in\n    ";
	
	print OUT join (";\n    ",
			map {
#			    "update (adjectives, adjective-toidx $_->[0], $_->[0]);\n" .
# PERF should compute the lcase ourselves, probably
                            "hash-put (adjective_map, \"\L$_->[1]\E\", $_->[0])" }
			@adjectives);

	print OUT "\n  end\n";
    } else {
	print OUT "$l\n";
    }
}

close IN;
close OUT;
