#! /usr/bin/ruby
# frozen_string_literal: true

# AoC 2020 Day4

# Fields :
# byr
# iyr
# eyr
# hgt
# hcl
# ecl
# pid
# cid (optionnal)

# class to collect passport fields
class Passport
  def init_fields
    @byr = nil
    @iyr = nil
    @eyr = nil
    @hgt = nil
    @hcl = nil
    @ecl = nil
    @pid = nil
    @cid = nil
    @valid = true
  end

  def parse_fields(fields)
    fields.each do |f|
      case f
      when /^byr:(?<byr>.*)$/
        @byr = Regexp.last_match(:byr)
      when /^iyr:(?<iyr>.*)$/
        @iyr = Regexp.last_match(:iyr)
      when /^eyr:(?<eyr>.*)$/
        @eyr = Regexp.last_match(:eyr)
      when /^hgt:(?<hgt>.*)$/
        @hgt = Regexp.last_match(:hgt)
      when /^hcl:(?<hcl>.*)$/
        @hcl = Regexp.last_match(:hcl)
      when /^ecl:(?<ecl>.*)$/
        @ecl = Regexp.last_match(:ecl)
      when /^pid:(?<pid>.*)$/
        @pid = Regexp.last_match(:pid)
      when /^cid:(?<cid>.*)$/
        @cid = Regexp.last_match(:cid)
      else
        @valid = false
        break
      end
    end
  end

  private :parse_fields, :init_fields

  def initialize(str)
    init_fields
    fields = str.split(/\s/)
    parse_fields fields
  end

  def part1?
    @valid && @byr && @iyr && @eyr && @hgt && @hcl && @ecl && @pid
  end

  def valid_hgt?
    m = /^(?<digits>\d+)(?<unit>cm|in)$/.match(@hgt)
    return false unless m

    if m[:unit] == 'cm'
      m[:digits].to_i.between?(150, 193)
    elsif m[:unit] == 'in'
      m[:digits].to_i.between?(59, 76)
    end
  end
  private :valid_hgt?

  def part2?
    @valid &&
      @byr.to_i.between?(1920, 2002) &&
      @iyr.to_i.between?(2010, 2020) &&
      @eyr.to_i.between?(2020, 2030) &&
      valid_hgt? &&
      (@hcl =~ /^#[0-9a-f]+$/) &&
      %w[amb blu brn gry grn hzl oth].member?(@ecl) &&
      (@pid =~ /^[0-9]{9}$/)
  end
end

def count_valid(passports, part)
  passports.select { |p| p.send(part) }.size
end

def show_solution(part, sol)
  puts "#{part}: valid passports: #{sol}"
end

def main
  datas = IO.readlines('day4.txt', "\n\n").collect do |entry|
    Passport.new(entry)
  end
  show_solution('Part1', count_valid(datas, 'part1?'))
  show_solution('Part2', count_valid(datas, 'part2?'))
end

main
