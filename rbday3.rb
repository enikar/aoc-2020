#! /usr/bin/ruby
# frozen_string_literal: true

# Aoc 2020 Day 3
def part1(datas)
  pos = 0
  tree_counts = 0
  datas.each do |l|
    tree_counts += 1 if l[pos] == '#'
    pos = (pos + 3) % 31
  end
  tree_counts
end

def part2_tree_counts(step, datas)
  right, down = step
  tree_counts = 0
  pos = 0
  pass = false
  datas.each do |line|
    if pass
      pass = false
      next
    end
    pass = down != 1
    tree_counts += 1 if line[pos] == '#'
    pos = (pos + right) % 31
  end
  tree_counts
end

def part2(datas)
  steps = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
  counts = steps.collect do |step|
    part2_tree_counts(step, datas)
  end
  counts.inject(1) { |acc, i| acc * i }
end

def show_solutions(part, sol)
  puts "#{part}: answer: #{sol}"
end

def main
  datas = IO.readlines('day3.txt', chomp: true)
  show_solutions 'Part1', part1(datas)
  show_solutions 'Part2', part2(datas)
end

main
