#! /usr/bin/python3

from operator import mul
from functools import reduce
from bisect import bisect


def read_datas():
    """Lit le fichier input.txt et retourne la liste des entiers qu'il
contient. """
    numbers = []
    with open("day1.txt") as datas:
        for line in datas:
            numbers.append(int(line))
    return sorted(numbers)


def combinations(kelems, datas):
    """Retourne la liste de toutes les combinaisons de kelems éléments
pris dans datas"""
    nelems = len(datas)
    combs = []
    comb = [0 for i in range(kelems)]

    def build_combs(start, depth):
        nonlocal combs, comb, nelems
        if depth == kelems:
            combs += [comb[:]]
        else:
            for i in range(start, nelems):
                comb[depth] = datas[i]
                build_combs(i + 1, depth+1)

    build_combs(0, 0)
    return combs


def part1(combs):
    """Extrait la solution de la liste de combinaisons"""
    # s = list(filter(lambda a: sum(a) == 2020, combs))
    s = [a for a in combs if sum(a) == 2020]
    return s[0]


def bsearch(elt, datas):
    i = bisect(datas, elt)
    if i:
        return datas[i-1] == elt
    else:
        return False


# Comme pour raku, la recherche dichotomique n'apporte pas
# grand chose. C'est le tri qui fait que la solution est plus
# vite trouvée.
def part2(numbers, combs):
    for comb in combs:
        n = 2020 - sum(comb)
        if bsearch(n, numbers):
            return comb+[n]


def print_solution(part, sol):
    print(f"{part}: numbers: {sol}. Product: {reduce(mul, sol, 1)}")


def main():
    datas = read_datas()
    combs = combinations(2, datas)
    print_solution('Part1', part1(combs))
    print_solution('Part2', part2(datas, combs))


if __name__ == "__main__":
    main()
