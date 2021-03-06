# setttleRs, v0.1

## Objective: Create a real-time luck metric for each player during a Settlers of Catan simulation. 
## Input: 
### num_players: int, must be between 2 to 4, number of players in the game
### random_seed: int, use to set random seed
### num_rolls: int, number of rolls in the game, must be between 10 and 200.

## Output: 
a list with two dataframes. return_prob_dat: dataframe with 6 columns: player_id, sum_expect_prob, num_cards, obs_over_expect, lucky_rel_rank, roll_num;  cuml_dat: a dataframe with two columsn: player_id, log_cuml_obs_over_expect

## Example:
sim_game <- settlers_sim(num_players = 3, random_seed = 123, num_rolls = 100)

## real-time results
sim_game$return_prob_dat
     player_id sum_expect_prob num_cards obs_over_expect lucky_rel_rank roll_num
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


## cumulative game results
sim_game$cuml_dat
    player_id log_cuml_obs_over_expect
1: player_id3                 28.28561
2: player_id2                 38.54110
3: player_id1                 46.57428

During each roll, there is an expected probability that a player gets a card, which is equivalent to the cumulative expected probability of all the hexes on the player's nodes. The probability of multiple independent events occuring is just the multiplication of the probability of each independent event. The observed probability can be boiled down to the number of cards an individual receives each turn. We can then calculate how much more lucky someone has by using the player with the worst luck as the reference player and track this probability over time. 
