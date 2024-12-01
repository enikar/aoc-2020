-- Cette fois-ci j'ai essayé de rendre la procedure Combinations plus
-- générique. Elle est désormais capapble de générer d'autres combinaisons
-- que des Pairs.  Cela complique l'accès aux Pairs dans les fonctions part1
-- et part2, car j'ai utilisé un tableau à 2 dimensions pour stocker les
-- combinaisons calculéees.
-- Pour compiler : gnatmake -Wall -O2 -fdata-sections -ffunction-sections -march=athlon64 aday1_5.adb
-- ou : gnatmake -Wall -O2 -march=athlon64 aday1.adb
with Ada.Text_IO;
with Ada.Containers.Generic_Sort;


procedure ADay1_5 is
  -- pragma Suppress(All_Checks);
  subtype Indices_T is Positive range 1..200;
  subtype Entier is Positive;
  type Tableau is array(Positive range <>) of Entier;

  subtype Pair is Tableau(Positive range 1..2);
  subtype Triple is Tableau(Positive range 1..3);
  type Tab_Combs is array(Positive range <>, Positive range <>) of Entier;


  procedure Read_Datas(T : out Tableau) is
    use Ada.Text_IO;
    f : File_Type;
    buf : String(1..20);
    cnt : Natural;
    I : Positive;
  begin
    I := Indices_T'First;
    Open(f, In_File, "day1.txt");
    while not End_Of_File(f) loop
      Get_Line(f, buf, cnt);
      T(Indices_T(I)) := Entier'Value(buf(1..cnt));
      I := Positive'Succ(I);
    end loop;
    Close(f);
  end Read_Datas;

  package Entier_IO is new Ada.Text_IO.Integer_IO(Entier);

  procedure Affiche_Solution(Part : String; Comb : Tableau) is
    use Ada.Text_IO;
    P : Entier := 1;
  begin
    Put(Part & ": numbers: [");
    for I in Comb'Range loop
      P := P * Comb(I);
      Entier_IO.Put(Comb(I), Width => 0);
      if I /= Comb'Last then
        Put(", ");
      end if;
    end loop;
    Put_Line("].  Product: " & Entier'Image(P));
  end Affiche_Solution;

  function nCk(N : Positive; K : Positive) return Positive is
    K1 : Constant Positive := Positive'Min(K, N-K);
    result : Positive := 1;
  begin
    for I in 1 .. K1 loop
      result := (result * (N-I+1)) / I;
    end loop;
    return result;
  end nCk;


  procedure Combinations(T : Tableau; Combs : out Tab_Combs) is
    N : Constant Positive := T'Last;
    K : Constant Positive := Combs'Last(2);
    J : Positive := Positive'First;
    Comb : Tableau(1..K);

    procedure Build(Depth : Positive; Start : Positive) is
    begin
      if Depth > K then
        for M in 1..K loop
          Combs(J, M) := Comb(M);
        end loop;
        J := Positive'Succ(J);
      else
        for I in Start .. N loop
          Comb(depth) := T(I);
          build(depth+1, I+1);
        end loop;
      end if;
    end Build;
  begin
    Build(1, T'First);
  end Combinations;

  function Pair_Sum(Comb : Pair) return Entier is
    Sum : Natural := 0;
  begin
    for E of Comb loop
      Sum := Sum + Natural(E);
    end loop;
    return Entier(Sum);
  end Pair_Sum;

  Numbers : Tableau(Indices_T);
  type Tab_Pairs is new Tab_Combs(1..nCk(Numbers'Length, 2), 1..2);

  function Get_Pair(Combs : Tab_Pairs; Index : Positive) return Pair is
    Comb : Pair;
  begin
    for I in Combs'Range(2) loop
      Comb(I) := Combs(Index, I);
    end loop;
    return Comb;
  end Get_Pair;

  pragma Inline(Pair_Sum, Get_Pair);

  function Part1(Combs : Tab_Pairs) return Pair is
    Comb : Pair;
  begin
    for I in Combs'Range(1) loop
      Comb := Get_Pair(Combs, I);
      if Pair_Sum(Comb) = 2020 then
        return Comb;
      end if;
    end loop;
    return (1, 1); -- Not reach, makes compiler happy
  end Part1;

  function bsearch(T : Tableau; N : Entier) return Boolean is
    inf, sup, mid : Indices_T;
  begin
    if T(T'Last) < N then
      return False;
    end if;

    sup := T'Last;
    inf := T'First;

    while inf < sup loop
      mid := (inf+sup) / 2;
      if T(mid) < N then
        inf := mid+1;
      else
        sup := mid;
      end if;
    end loop;

    return (T(inf) = N);
  end bsearch;

  function Part2(Combs: Tab_Pairs; Numbers : Tableau) return Triple is
    N : Integer;
    T : Triple;
    Comb : Pair;
  begin
    for I in Combs'Range loop
      Comb := Get_Pair(Combs, I);
      N := 2020 - Pair_Sum(Comb);
      if N > 0 and then bsearch(Numbers, Entier(N)) then
        T(1) := Comb(1);
        T(2) := Comb(2);
        T(3) := Entier(N);
        return T;
      end if;
   end loop;
   return (1, 1, 1); -- Not reach, makes compiler happy
  end Part2;

  function Before(L, R : Indices_T) return Boolean is
    (Numbers(L) < Numbers(R));

  procedure Swap(L, R : Indices_T) is
    x : Entier;
  begin
    x := Numbers(L);
    Numbers(L) := Numbers(R);
    Numbers(R) := x;
  end Swap;

  pragma Inline(Swap, Before);

  procedure Tableau_Sort is
    new Ada.Containers.Generic_Sort(Indices_T, Before, Swap);

  Combs : Tab_Pairs;
  Sol1 : Pair;
  Sol2 : Triple;
begin
  Read_Datas(Numbers);
  Tableau_Sort(Numbers'First, Numbers'Last);
  Combinations(Numbers, Tab_Combs(Combs));
  Sol1 := Part1(Combs);
  Affiche_Solution("Part1", Tableau(Sol1));
  Sol2 := Part2(Combs, Numbers);
  Affiche_Solution("Part2", Tableau(Sol2));
end ADay1_5;

-- vim: ts=2 sts=2 sw=2
