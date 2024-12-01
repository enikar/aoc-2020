#! /usr/bin/raku

use v6;
# $kelems est un entier.
# $datas est une liste.
# Retourne une Seq (List ne fonctionne pas ici…).
sub combinations(Int $kelems, List $datas --> Seq) {
    my $index_max = $datas.elems - 1;
    my $k = $kelems - 1;
    sub build($start, @comb) {
        if @comb.elems == $k {
           [ ( ($start..$index_max).map: { @comb.clone.push($datas[$_]) } ) ]
        } else {
            ($start..$index_max).flatmap: {
                build(1+$_, @comb.clone.push($datas[$_]))
            }
        }
    }
    build(0, [])
}

sub print-solution(Str $part, List $sol) {
    my $p = $sol.reduce: &infix:<*>;
    say "$part: numbers: ($sol).  Product: $p";
}

sub part1(List @combs --> List) {
    @combs.first: sub (List $comb) { $comb.sum == 2020 }
}

# Pour la partie 2, la meilleure option est d'utiliser un Set
# C'est plus rapide que de trier la liste et faire un binary-search au
# contraire d'haskell…
# En fait il n'y a pas de binary search, car il n'y en a pas besoin.
sub part2(List $numbers, List @combs --> List) {
    my $n;
    my $nums = $numbers.Set; # Crée un Set à partir de la List $numbers
    for @combs.list -> @comb {
        $n = 2020 - @comb.sum;
        if $nums{$n} {   # test si $n est dans le Set $nums
            @comb.push($n);
            return @comb;
        }
    }
}

sub read-datas(--> List) {
    # IO.lines va lire le fichier de façon fainéante.
    # Ça va donc créer un stream. C'est bien. Mais ça
    # pose des soucis. Raku refuse de lire un stream plusieurs
    # fois quand il n'est pas complètement consommé !

    # do for 'input.txt'.IO.lines { $_.Int };

    # remplacé par un slurp
    # slurp('input.txt').split("\n", :skip-empty).map({ .Int }).list

    # Finalement c'est encore mieux avec IO.words et en utilisant .map
    'day1.txt'.IO.words.map({ .Int }).list
}

# Pas facile de savoir quand une liste est lazy ou pas.
# .eager permet de consommer une liste lazy.
# Utiliser des Array avec @var force des copies mais ce n'est pas
# aussi efficace.
# Le problème vient des Seq. C'est impossible de les utiliser
# deux fois quand ils ne sont pas entièrrement consommés.
# Bref, raku est compliqué au possible, c'est bien le successeur de perl !
# C'est encore très mystérieux ! Toujours est-il que je n'arrive pas
# à descendre en dessous des 2s (meilleur temps ~2.30s ). C'est lent.
sub main() {
    # On fait un binding avec := plutôt qu'une copie avec = car
    # c'est plus efficace. Ça évite d'avoir à copier toute la liste.
    # On utilise eager pour forcer la lecture de toutes les données
    # du fichier. Je ne sais pas pourquoi, je ne peux pas utiliser
    # @numbers de partout… c'est vraiment compliqué.
    my List $numbers := read-datas();
    my List @combs = combinations(2, $numbers);
    print-solution('Part1', part1(@combs));
    print-solution('Part2', part2($numbers, @combs));
}

main();
