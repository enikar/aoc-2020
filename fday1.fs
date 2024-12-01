\ -*- forth -*-
\ From the shell: $ time gforth fday1.fs -e 'day1 bye'
\ From gforth:
\ usage: include essai.fs
\ or:    s" fday.fs included
\ then: day1
[IFUNDEF] AOC-DAY1 vocabulary AOC-DAY1 [THEN]
Only Forth also AOC-DAY1 also definitions

\ WARNING: NO BOUND CHECK!

\ the file descriptor yield by open-file
0 Value fd-in
\ a buffer of 24 bytes to read line to.
24 Constant max-tampon
Create tampon max-tampon allot
\ numbers will hold the 200 numbers from day1.txt
Create numbers 202 cells allot

\ Get a number from an index. No bounds check
: numbers@ ( index -- n ) \ 0 <= index <= 200
    cells numbers + @ ;

\ store a number at index in numbers
: numbers! ( number index -- )
    cells numbers + ! ;

\ the number of combinations of k elements from a set of n elements
: nCk ( n k -- nCk ) \ n >= k >= 0
    2dup - min ( n k' ) \ k' = min k (n-k)
    1 swap 1+ 1 u+DO ( n acc ) \ LOOP from I=1 To k'
        over I ( n acc n i )
        - 1+   ( n acc n-i+1 )
        *      \ n acc*(n-i+1)
        I /    \ n acc=(acc*(n-i+1) `div` i)
    LOOP
    nip ; \ remove n from the stack

\ an array of combinations of 2 elements from numbers
Create combs 200 2 nCk 2 * 2 + cells allot
\ pos is the current address in combs
combs Value pos
: reset-pos combs to pos ;
: pos+ pos cell+ cell+ to pos ;

\ store num1 and num2 at combs + pos
\ increment pos to the next free address
: combs! ( num1 num2 -- )
    pos 2!
    \ pos 2 cells + to pos ;
    pos+ ;

\ fetch two numbers from combs at pos
\ increment pos to next record
: combs@ ( -- num1 num2 )
    pos 2@
    \ pos 2 cells + to pos ;
    pos+ ;

\ open a file for read, and store the fd to fd-in
: open-input ( addr u -- )
    r/o open-file throw to fd-in ;

: close-input ( -- )
    fd-in close-file throw ;

\ read-line remove the new line character.
: read-line-from-fd-in ( -- n flag )
    tampon max-tampon fd-in read-line throw ;

\ for now, just try to read day1.txt and print each line.
\ : print-input ( -- )
\     s" input.txt" open-input   \ open input.txt and set fd-in
\     BEGIN read-line-from-fd-in \ read a line from input.txt
\     WHILE
\             tampon swap type    \ print the line read
\             cr                  \ emit a new line
\     REPEAT
\     drop \ clean the stack
\     close-input ;   \ close the file


: to-number ( nbytes  -- number )
    tampon swap 0. 2swap >number \ convert nbytes bytes from tampon
    2drop drop ; \ Just keep one number on the stack from the conversion

\ fill the array numbers with the content of input.txt
: read-numbers ( -- )
    s" day1.txt" open-input
    0 >r  \ use return stack to keep an index in the array numbers.
    BEGIN read-line-from-fd-in \ attempt to read a line
    WHILE
            to-number
            r@ numbers! \ get current index, store the number in numbers
            r> 1+ >r    \ calculate the new index and save it
    REPEAT
    drop  \ clean stack, 0 is in the top of stack
    rdrop \ restore the return stack
    close-input ;

\ A slot to keep the current computed pair
Create comb 2 cells allot
\ fetch the two number
: comb@ ( -- num1 num2 )
    comb 2@ ;
\ store num at comb + index  but not two numbers in same time !
: comb! ( num index -- )
    cells comb + ! ;

: build ( start depth -- ) recursive
    dup 2 = IF           ( start depth )
        comb@ combs!     ( start depth ) \ store comb in combs (at pos)
        2drop            ( ) \ clean the stack
    ELSE
        200 rot u+DO              ( depth ) \ loop from start to 199
           I numbers@ over comb!  ( depth ) \ store numbers at I to comb at depth
            dup 1+ I 1+ swap       ( depth I+1 depth+1) \ compute depth+1 and I+1 stack up
                                                        \ the new depth and the new start
           build                  ( depth )             \ call build with these new values
       LOOP
       drop                       (  )                  \ there is yet a needless depth, drop it.
   THEN ;

\ combinations fills combs with combinations of pairs from numbers.
: combinations ( -- )
    reset-pos   \ reset pos
    0 0 build   \ call build
    reset-pos ; \ reset pos

\ Not a secure version. Assume there is a solution. May loop indefinitely
: part1 ( -- num1 num2 )
    reset-pos \ reset pos
    0 0       \ stack up two values to init the loop
    BEGIN
        2drop      \ clean the stack
        combs@     \ fetch a  pair from combs
        2dup +     \ compute the sum
                   \ let the two numbers on the stack
    2020 = UNTIL   \ leave the loop when the sum is equal to 2020
    reset-pos ; \ reset pos

\ TODO: sort the numbers in the array numbers.
\       then write a binary search
\ like part1, this is not secure. May lead to stack underflow.
: part2 ( -- n1 n2 n3 ) \ n1 + n2 + n3 = 2020
    reset-pos \ reset pos
    BEGIN
        2020 combs@ 2dup comb 2! + -  \ store n1 and n2 to comb
                                      \ compute 2020 - n1 - n2
        200 0 DO \ walk through numbers from index = 0 to 199
            dup I numbers@
            = IF drop \ remove 2020 - n1 - n2 from the stack
                 reset-pos         \ reset pos
                 I numbers@        \ fetch the number
                 comb@ unloop exit \ fetch comb and exit
            THEN
        LOOP
        drop  \ remove 2020 - n1 - n2 from the stack
    AGAIN ;


: 3dup ( a b c -- a b c a b c)
    >r 2dup r@ -rot r> ;

: type-solution ( [num3] num2 num1 addr count -- )
    2dup type ." : numbers: "
    2dup s" Part1" compare 0 = IF
        2drop 2dup . . *
    ELSE s" Part2" compare 0 = IF
        3dup . . . * *
    ELSE
        abort" Unknown part"
    THEN THEN
    space ." Product: " . ;


: day1 ( -- )
    read-numbers
    combinations
    cr
    part1 s" Part1" type-solution cr
    part2 s" Part2" type-solution cr ;
