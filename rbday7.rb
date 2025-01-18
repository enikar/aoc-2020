#! /usr/bin/ruby
# frozen_string_literal: true

# AoC 2020, day7

require 'strscan'

# Parsing using StringScanner
# It's quite slow but it's explicit.
def parse_bag(ssc)
  bag = ssc.scan(/^\w+ \w+/)
  ssc.skip(' bags contain ')
  [bag, ssc]
end

def parse_content(ssc)
  content = []
  until ssc.eos?
    b = ssc.scan_until(/[,.]/)
    c = StringScanner.new(b)
    n = c.scan(/^\d+/).to_i
    c.skip(' ')
    color = c.scan(/^\w+ \w+/)
    content.push([color, n])
    ssc.skip(' ')
  end
  content
end

def make_rules
  rules = {}
  IO.foreach('day7.txt', chomp: true) do |line|
    bag, s = parse_bag(StringScanner.new(line))
    content = if s.check('no other bags.')
                []
              else
                parse_content(s)
              end
    rules[bag] = content
  end
  rules
end

# We use a dfs for part1 and part2, but not the same function.
# So this is part1.
def dfs1(rules, bag, visited)
  return visited[bag] if visited.include?(bag)

  contain = 0
  rules[bag].each do |ct|
    b, = ct
    if b == 'shiny gold'
      contain = 1
      break
    end
    if dfs1(rules, b, visited) == 1
      contain = 1
      break
    end
  end
  visited[bag] = contain
  contain
end

def part1(rules)
  acc = 0
  visited = {}
  rules.each_key do |bag|
    next if bag == 'shiny gold'

    acc += dfs1(rules, bag, visited)
  end
  acc
end

# Here starts part2
def dfs2(rules, bag, visited)
  return visited[bag] if visited.include?(bag)

  acc = 1
  rules[bag].each do |ct|
    b, n = ct
    m = dfs2(rules, b, visited)
    acc += m * n
  end
  visited[bag] = acc
  acc
end

def part2(rules)
  dfs2(rules, 'shiny gold', {}) - 1
end

# The main
rules = make_rules
puts "Part1: #{part1 rules}"
puts "Part2: #{part2 rules}"
