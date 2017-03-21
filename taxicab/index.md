---
layout: default
class: writeup
title: TS - Searching for Ramanujan's taxicab numbers
---

> I remember once going to see him (Ramanujan) when he was lying ill at
> Putney. I had ridden in taxi-cab No. 1729, and remarked that the
> number seemed to be rather a dull one, and that I hoped it was not an
> unfavourable omen. "No", he replied, "it is a very interesting number;
> it is the smallest number expressible as the sum of two \[positive\]
> cubes in two different ways.

(from [Wikipedia](https://en.wikipedia.org/wiki/Taxicab_number))

Taxicab numbers and SICP
------------------------

Taxicab numbers are numbers that can be represented as the sum of two
cubes in at least two different ways. As the above quote indicates, the
smallest such number is 1729, which is the sum of cubes of the pairs (1,
12) and (9, 10).

A bit of Googling reveals a number of approaches to efficiently search
for numbers that satisfy the taxicab property. The one that I've found
particularly exciting is outlined in Chapter 3 of [*Structure and
Interpretation of Computer Programs* by Abelson, Sussman, and
Sussman](https://mitpress.mit.edu/sicp/full-text/book/book.html).
Exercise 3.71 asks the reader to find the first n taxicab numbers, using
a data structure known as a *stream*.

Streams (aka lazy lists)
------------------------

After describing some of the challenges of modeling objects with mutable
state, the authors of SICP introduce streams as objects that represent
an entire history of values, rather than just the state at a particular
time. We decouple program time from real life time.

> In physics, when we observe a moving particle, we say that the
> position (state) of the particle is changing. However, from the
> perspective of the particle's world line in space-time there is no
> change involved.

The magic of streams is possible through lazy evaluation. A stream is
defined as a head (`car`) element and a tail (`cdr`). The tail, however,
is not evaluated until it is needed. And by delaying its evaluation, we
are able to work with streams as though they are lists of values, but
without the overhead of materializing such lists. In fact, we can
recursively define *infinite* sequences as streams, which would be
unthinkable with a regular list.

I decided to implement streams in R, in the package `lazylist` ([on
Github](https://github.com/tarakc02/lazylist)).

    library(lazylist)

    # cons_stream generates a stream
    # notice the recursive definition
    integers_starting_from <- function(k) {
        cons_stream(k, integers_starting_from(k + 1))
    }

    integers <- integers_starting_from(0)
    integers

    ## |   0 
    ## |   1 
    ## |   2 
    ## |   3 
    ## |   4 
    ## |   ...

    integers[1E4]

    ## [1] 9999

    # accessor functions
    stream_car(integers)

    ## [1] 0

    stream_cdr(integers)

    ## |   1 
    ## |   2 
    ## |   3 
    ## |   4 
    ## |   5 
    ## |   ...

Elements of the stream are lazily evaluated as needed, but the evaluated
values are cached so that we can avoid the cost of re-calculating
values.

    fib <- function(a, b) {
        cons_stream(a, fib(b, a + b))
    }

    fibonacci <- fib(0, 1)
    print(fibonacci, n = 10)

    ## |   0 
    ## |   1 
    ## |   1 
    ## |   2 
    ## |   3 
    ## |   5 
    ## |   8 
    ## |   13 
    ## |   21 
    ## |   34 
    ## |   ...

Streams and functionals
-----------------------

So far, streams look like an intriguing curiousity, but may not offer
much value over a memoized function. However, they turn out to be a
powerful abstraction when we think of them as lists, and apply
functionals such as `map` and `filter`.

    squares <- stream_map(integers, function(x) x**2)
    print(squares, n = 10)

    ## |   0 
    ## |   1 
    ## |   4 
    ## |   9 
    ## |   16 
    ## |   25 
    ## |   36 
    ## |   49 
    ## |   64 
    ## |   81 
    ## |   ...

    # find fibonacci numbers that are divisible by 3:
    stream_filter(fibonacci, function(x) !(x %% 3))

    ## |   0 
    ## |   3 
    ## |   21 
    ## |   144 
    ## |   987 
    ## |   ...

Taxicab pairs
-------------

We'll search for taxicab numbers, and the associated pairs, by
generating *infinite streams of pairs*. How exciting!

The function `weighted_pairs` generates a stream consisting of the cross
product of two streams, with the additional property that the pairs are
ordered by a user-supplied weighting function. I'll generate pairs that
are ordered by the sum of their cubes.

    ordered_pairs <- weighted_pairs(integers, integers, 
                                    weight = function(x) sum(x**3))
    ordered_pairs

    ## |   0 0 
    ## |   0 1 
    ## |   1 1 
    ## |   0 2 
    ## |   1 2 
    ## |   ...

Now our job is easy. We look at the stream of sums of cubes of
`ordered_pairs`, and look for duplicates (which is easy since they're
already sorted). The function `shift_left` is just an alias for
`stream_cdr`, noting that returning the tail of a sequence is equivalent
to shifting the entire sequence to the left by one element. Notice that
`stream_map` here is able to take multiple streams and a function with
the appropriate number of arguments.

    library(magrittr)

    # stream of indexes of taxicab numbers
    is_taxicab <- stream_map(list(ordered_pairs,
                                  shift_left(ordered_pairs)),
                             function(x, y) sum(x**3) == sum(y**3))

    taxicab_values <- ordered_pairs %>% 
        stream_at(is_taxicab) %>%
        stream_map(function(x) sum(x**3))

    # the first 5 taxicab numbers
    taxicab_values

    ## |   1729 
    ## |   4104 
    ## |   13832 
    ## |   20683 
    ## |   32832 
    ## |   ...

    # and we can go as far out as we like:
    taxicab_values[100]

    ## [1] 4673088

Finding the associated pairs is also simple:

    format_pair <- function(pair) paste(pair, collapse = ", ")
    print_pairs <- function(pair1, pair2) 
        paste("(", pair1, ") (", pair2, ")", sep = "")

    taxicab_pairs <- 
        stream_map(list(ordered_pairs,
                        shift_left(ordered_pairs)), 
                   function(x, y) print_pairs(format_pair(x), format_pair(y))) %>%
        stream_at(is_taxicab)

    taxicab_pairs

    ## |   (1, 12) (9, 10) 
    ## |   (2, 16) (9, 15) 
    ## |   (2, 24) (18, 20) 
    ## |   (10, 27) (19, 24) 
    ## |   (4, 32) (18, 30) 
    ## |   ...

    taxicab_pairs[100]

    ## [1] "(25, 167) (64, 164)"
