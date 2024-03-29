# setttleRs, v0.1

## Objective: Create a luck metric for each player during a Settlers of Catan simulation. Empirically show which players got more lucky in drawing resources compared to their expected probabilities. 
## Input: 
#### num_players: int, must be between 2 to 4, number of players in the game
#### random_seed: int, use to set random seed
#### num_rolls: int, number of rolls in the game, must be between 10 and 200.

## Output: 
Two dataframes. return_prob_dat: dataframe with 6 columns: player_id, sum_expect_prob, num_cards, obs_over_expect, lucky_rel_rank, roll_num;  cuml_dat: a dataframe with two columsn: player_id, log_cuml_obs_over_expect

## Luck metric description:
During each roll, there is an expected probability that a player gets a card, which is equivalent to the cumulative expected probability of all the hexes on the player's nodes. The observed probability can be boiled down to the number of cards an individual receives each turn. Therefore, a player's luck can be described as observed (# of cards picked up) / cumulative expected probability. A player with the higher luck score is luckier because their luck ratio (observed probability / expected probability) is greater. 

We also calculate how much more lucky someone is by using the player with the worst luck as the reference player and tracking this probability over time. The probability of multiple independent events occuring is just the multiplication of the probability of each independent event. In the cuml_dat dataframe, we calculate the log_cuml_obs_over_expect as the sum of all resources received / product of all expected probabililities (logged). 


## Example:
```
sim_game <- settlers_sim(num_players = 3, random_seed = 123, num_rolls = 100)
```

## real-time results for each roll
```
sim_game$return_prob_dat

     player_id   sum_expect_prob num_cards obs_over_expect lucky_rel_rank roll_num
  1: player_id3       0.7500000         2        2.666667              2        0
  2: player_id2       0.6111111         1        1.636364              3        0
  3: player_id1       0.6666667         2        3.000000              1        0
  4: player_id3       0.7500000         1        1.333333              3        3
  5: player_id2       0.6111111         2        3.272727              1        3
 ---                                                                             
197: player_id2       0.6111111         1        1.636364              1       99
198: player_id1       0.6666667         1        1.500000              2       99
199: player_id3       0.7500000         1        1.333333              3      100
200: player_id2       0.6111111         2        3.272727              1      100
201: player_id1       0.6666667         1        1.500000              2      100
```
## end of game results

```
## cumulative game results
sim_game$cuml_dat
   player_id  log_cuml_obs_over_expect
1: player_id3                 23.65673
2: player_id2                 37.31341
3: player_id1                 31.72004
```
