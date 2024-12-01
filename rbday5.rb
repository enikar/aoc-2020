#! /usr/bin/ruby
# frozen_string_literal: true

# AoC 2020 Day5
def str2number(str)
  head = str[0, 7]
  tail = str[7, 3]
  h = (head.tr 'FB', '01').to_i(2)
  t = (tail.tr 'LR', '01').to_i(2)
  h * 8 + t
end

def part1(values)
  values.minmax
end

def part2(values)
  values.sort!
  v = 1 + values[0]
  id = 0
  values[1..].each do |n|
    if n != v
      id = v
      break
    else
      v = n + 1
    end
  end
  id
end

def main
  values = IO.readlines('day5.txt', chomp: true).collect do |line|
    str2number(line)
  end
  min, max = part1(values)
  puts "Part1: min: #{min}, max: #{max}"
  puts "Part2: place id: #{part2(values)}"
end

main
