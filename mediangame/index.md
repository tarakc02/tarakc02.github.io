---
layout: default
class: writeup
title: TS - The Median Game
---

Introduction
------------

The median game is described in the blogpost [The Median
Game](https://gilkalai.wordpress.com/2017/01/14/the-median-game/) by Gil
Kalai, which I found via a
[tweet](https://twitter.com/JSEllenberg/status/820339568486576128) by
Jordan Ellenberg.

The
[blogpost](https://gilkalai.wordpress.com/2017/01/14/the-median-game/)
describes the game \[check the comments to the post for variations\]:

> There are three players and they play the game for eight rounds. In
> every round all players simultaneously say a number between 1 and 8. A
> player whose number is (strictly) between the other two get a point.
> At the end of the game the winner is the player whose number of points
> is strictly between those of the others.

A random game
-------------

I created the [`mediangame`
package](https://github.com/tarakc02/mediangame) to experiment with
different strategies for playing the game. For example, to simulate
1,000 games using a random strategy:

    library(mediangame)

    stooges <- list(
        larry = random_player,
        moe = random_player,
        curly = random_player
    )

    set.seed(38370)
    random_results <- replicate(
        1000, 
        simulate_game(stooges), 
        simplify = FALSE
    )
    random_results[[3]]

    ## winner: curly 
    ## final scores:
    ## larry: 1
    ## moe: 3
    ## curly: 2

    library(purrr)
    map_chr(random_results, "game_winner") %>% 
        table

    ## .
    ##     curly     larry       moe NO WINNER 
    ##       157       171       150       522

Here a "player" is just a function with two arguments, the first
argument is the player's own score, and the second argument is a named
vector containing the opponents' scores. The output is not a single
number, but a probability distribution that associates a probability to
each of the integers between 1 and 8.

    random_player(myscore = 2, 
                  others = c(larry = 1, moe = 4)) %>% 
        round(2)

    ## [1] 0.11 0.22 0.03 0.06 0.08 0.17 0.19 0.14

When the game is simulated, this function is called to generate the
probability distribution, and the simulation will then draw from that
distribution in order to generate the play for that round.

A naive strategy
----------------

Consider a strategy of always picking the number 4:

    always_n <- function(n) {
        function(myscore, others) {
            res <- numeric(8)
            res[n] <- 1
            return(res)
        }
    }

    always_4 <- always_n(4)

    players <- list(
        always_4 = always_4,
        random_1 = random_player,
        random_2 = random_player
    )

    set.seed(8360272)
    a4_v_random <- replicate(1000, 
                             simulate_game(players), 
                             simplify = FALSE)
    map_chr(a4_v_random, "game_winner") %>% table

    ## .
    ##  always_4 NO WINNER  random_1  random_2 
    ##        71       483       210       236

As you might expect, playing 4 every time wins too many rounds. The way
to extract final scores from each round is a little quirky -- the game
results object includes the score before each round, and the final score
is kept as the ninth element of the list:

    library(tibble)
    by_game <- map(a4_v_random, ~.$scores[[9]]) %>% 
        map2_df(.x = ., .y = seq_along(.), 
                ~add_column(enframe(.x), game = rep(.y, 3), .before = 1))

Now I can look at the mean scores. As expected, `always_4` wins far too
many rounds:

    library(tidyr)
    library(dplyr)

    spread(by_game, name, value) %>% 
        summarise_each(funs(mean), -game)

    ## # A tibble: 1 × 3
    ##   always_4 random_1 random_2
    ##      <dbl>    <dbl>    <dbl>
    ## 1    3.025    1.112    1.138

Learning a better strategy
--------------------------

The function `analyze_game` summarizes game information in order to make
it easier to learn from. The column `game_win` is -1 for a loss, 0 for a
tie ("NO WINNER"), and +1 for a win. Note that that is the result for
the entire game, not just the current round.

    train <- map_df(random_results, analyze_game)

So I can train a model to predict the outcome of a game from a
particular player's perspective, given the current score and the number
that player plays for that round:

    library(randomForest)
    train %>% 
        select(my_score:game_win) %>%
        randomForest(game_win ~ ., data = .) -> treemod

To turn this model into a median game player:

    tree_player <- function(myscore, others) {
        others <- sort(others)
        df <- data.frame(
            my_score = unname(myscore),
            p1_score = others[[2]],
            p2_score = others[[1]],
            my_play = 1:8
        )
        preds <- unname(predict(treemod, newdata = df))
        if (any(preds < 0)) preds <- preds - min(preds) + .01
        preds / sum(preds)
    }

The competition
---------------

Now we can play the `tree_player` against some random competition:

    final_players <- list(
        tree_player = tree_player,
        random_1 = random_player,
        random_2 = random_player
    )

    set.seed(27307)
    final_results <- replicate(1000, 
                               simulate_game(final_players), 
                               simplify = FALSE)
    map_chr(final_results, "game_winner") %>% table

    ## .
    ##   NO WINNER    random_1    random_2 tree_player 
    ##         604          99          93         204
