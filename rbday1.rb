#! /usr/bin/ruby
# frozen_string_literal: true

# Combinations of kelems from the array arr
# Usage: Combinations.new(n, arr).combinations
class Combinations
  def initialize(kelems, arr)
    @kelems = kelems
    @index_max = arr.size - 1
    @arr = arr
    @combs = []
    @comb = []
  end

  def combinations_int(start, depth)
    if @kelems == depth
      @combs.push(@comb.clone)
    else
      (start..@index_max).to_a.each do |i|
        @comb[depth] = @arr[i]
        combinations_int(i + 1, depth + 1)
      end
    end
  end

  def combinations
    combinations_int(0, 0)
    @combs
  end

  private :combinations_int
end

def part1(combs)
  i = combs.find_index { |arr| arr.sum == 2020 }
  combs[i]
end

def part2(numbers, combs)
  combs.each do |comb|
    n = 2020 - comb.sum
    return (comb + [n]) if numbers.bsearch { |x| n <=> x }
  end
end

def print_solution(part, sol)
  puts "#{part}: numbers #{sol}.  Product: #{sol.reduce(1, :*)}"
end

def main
  datas = IO.readlines('day1.txt', chomp: true).map(&:to_i).sort
  combs = Combinations.new(2, datas).combinations
  print_solution('Part1', part1(combs))
  print_solution('Part2', part2(datas, combs))
end

main if $PROGRAM_NAME == __FILE__
