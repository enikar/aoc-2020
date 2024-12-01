#! /usr/bin/perl

use List::Util qw ( sum product );
use List::MoreUtils qw ( bsearch );

sub combinations ($$) {
    local ($kelems, $datas) = @_;
    local $index_max = scalar(@$datas) - 1;
    $kelems -= 1;
    do_combinations(0, []);
}

sub do_combinations {
    my ($start, $comb) = @_;
    if (scalar(@$comb) == $kelems) {
        [map { [@$comb, $datas->[$_]] } ($start..$index_max)]
    } else {
        my @concatened;
        my $r;
        for $i ($start..$index_max) {
            $r = do_combinations($i + 1, [@$comb, $datas->[$i]]);
            @concatened = (@concatened, @$r);
        }
        return [@concatened];
    }
}


sub part1 ($) {
    my $combs = shift;
    my @sols = grep { sum(@$_) == 2020 }  @$combs;
    my $ans = shift(@sols);
    @$ans;
}

sub part2 ($$) {
    my ($datas, $combs) = @_;
    my $n;
    for $comb (@$combs) {
        $n = 2020 - sum(@$comb);
        if (bsearch { $_ <=> $n } @$datas) {
            return (@$comb, $n);
        }
    }
}

sub print_solution ($$) {
    my ($part, $sol) = @_ ;
    my $p = product(@$sol);
    print "$part: numbers: (@$sol).  Product: $p\n";
}

sub read_datas () {
    open FIK, "<day1.txt";
    my @nums = map { int } (<FIK>);
    close FIK;
    # On trie les nombres pour la part2 où j'utilise bsearch
    # Ça ne change pas grand chose par rapport à un grep dans
    # les données non triées, mais bon.
    sort { $a <=> $b } @nums;
}

sub main () {
    my @datas = read_datas();
    my $combs = combinations(2, \@datas);
    my @sol = part1($combs);
    print_solution("Part1",\@sol);
    @sol = part2(\@datas, $combs);
    print_solution("Part2", \@sol);
}

main();
