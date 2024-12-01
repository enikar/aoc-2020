#! /usr/bin/ruby
# frozen_string_literal: true

# AoC 2020 Day6

def part1(datas)
  datas.collect { |grp| [].union(*grp).size }.sum
end

def part2(datas)
  datas.collect { |grp| grp[0].intersection(*grp[1..]).size }.sum
end

def main
  datas = IO.readlines('day6.txt', "\n\n", chomp: true).collect do |group|
    group.split(/\s/).collect { |s| s.split('') }
  end
  puts "Part1: sum: #{part1(datas)}"
  puts "Part2: sum: #{part2(datas)}"
end

main
