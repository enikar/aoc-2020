#! /usr/bin/ruby
# frozen_string_literal: true

# AoC 2020 Day2
class MyData
  def initialize(str)
    @first, @second = str.sub(/^(\d+-\d+) .*$/, '\1').split('-').collect(&:to_i)
    @letter = str.sub(/^\S+ ([a-z]):.*$/, '\1')
    @password = str.sub(/^[^:]+: (.*)$/, '\1')
  end

  def part1?
    @password.count(@letter).between?(@first, @second)
  end

  def part2?
    @first <= @password.size &&
      @second <= @password.size &&
      (@password[@first - 1] == @letter &&
       @password[@second - 1] != @letter) ||
      (@password[@first - 1] != @letter &&
       @password[@second - 1] == @letter)
  end
end

def count_valid_passwords(part, passwords)
  count = 0
  passwords.each do |p|
    count += 1 if p.send(part)
  end
  count
end

def show_solutions(part, sol)
  puts "#{part}: valid passwords count: #{sol}"
end

def main
  passwords = IO.readlines('day2.txt', chomp: true).collect do |s|
    MyData.new s
  end
  show_solutions('Part1', count_valid_passwords('part1?', passwords))
  show_solutions('Part2', count_valid_passwords('part2?', passwords))
end

main
