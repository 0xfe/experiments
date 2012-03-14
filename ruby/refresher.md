# The Ruby Refresher
Copyright &copy; Mohit Muthanna 2011

## Preface

This document aims to refresh one's memory of Ruby syntax, semantics, common
operations, and standard libraries as quickly and efficiently as possible. The
reader is expected to have had some past experience with the Ruby language.

Newcomers to Ruby should read some of the
[beginners documentation](http://www.ruby-lang.org/en/documentation/) before
working through this refresher.

## Instructions

Each line is designed to introduce a new language rule, or reinforce a
previously established rule.

1. Read each line and try to evaluate it in your head.
2. If you can't figure it out, execute it in `irb`.
3. Think (for just a few seconds) about what makes it different from the
   previous lines.
4. If you get confused at any point, look up the relevant documentation at
   [Ruby-Doc.org](http://www.ruby-doc.org/).


## Basics

    a = 1
    b = "boo"
    d = 8

    # This is a comment

    madman = "I have #{a + d} baboons"
    madman2 = "I also have %s baboons" % [a + d]

    puts "This string is printed with a newline"
    print "This string is printed without a newline"

    triple_x = "X" * 3

    print 'This string beings with a single quote.'
    puts 'In Ruby, the string "b = #{b}" evaluates to "b = boo"'


## Arrays

    primes = [2, 3, 5, 7]
    people = ["Bob", "Alice", "Eve"]

    puts "The first prime is " << primes[0]
    puts "The last person is " << people[-1]

    puts "The first person is " << people.first
    puts "The last prime is " << primes.last
    primes.pop
    puts "The last prime is now " << primes.last
    primes.push 7
    puts "The last prime is #{primes.last} again"
    primes << 11
    puts "The last prime is now #{primes.last}"

    puts "There are #{ people.size } people"
    people.shift
    puts "There are now #{ people.length } people"
    people.unshift "Bob"
    puts "#{people.first}'s back!"

    sorted_people = people.sort
    puts "#{people.first} is still Bob"
    puts "#{sorted_people.first} is not Bob"
    people.sort!
    puts "#{people.first} is not Bob"

    puts primes[0..3]     # => [2, 3, 5]
    puts primes[0,3]      # => [2, 3, 5]

    people.each { |person| puts person }
    people.each do |person|
      puts person
    end

    b_people.select { |person| person =~ /^B/ }     # => ["Bob"]
    sum_of_primes = primes.inject { |sum, x| sum + x }

    sentence = "I am your father"
    words = sentence.split(" ")       # => ["I", "am", "your", "father"]
    tweet = "I can't find my father"
    relevant = words.any? { |word| tweet.include?(word) }

    reconstructed_sentence = words.join(" ")

    [1, 4, 8, 3, 54, 34].select { |n| n > 30 }      # returns one array
    [1, 4, 8, 3, 54, 34].partition { |n| n > 30 }   # returns array of arrays
    [1, 3, 5].map { |n| n ** 2 }                    # => [1, 9, 25]
    [[1, 4], [4, 5], [6, 6]].flatten!               # => [1, 4, 4, 5, 6, 6]

## Hashes

    capitals = {
      "India" => "New Delhi",
      "Canada" => "Ottawa",
      "U.S.A" => "Washington D.C"
    }

    person = {
      :first_name, "Albert",
      :last_name, "Einstein",
      :email, "al@wormhole.com"
    }

    puts "The capital of India is #{capitals[India]}"
    puts "Contact me at #{person[:email]}"

    capitals.each do |k, v|
      puts "The capital of #{k} is #{v}
    end

    capitals.has_key?("Canada")

    capitals.keys.sort.each do |k|
      puts "The capital of #{k} is #{capitals[k]}
    end

    more_capitals = {
      "China" => "Beijing",
      "Russia" => "Moscow",
      "Germany" => "Berlin"
    }

    capitals.merge!(more_capitals)
    puts capitals["China"]




