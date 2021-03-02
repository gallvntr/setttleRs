# setttleRs

## Objective: Create a real-time luck metric for each player during a Settlers of Catan simulation. 
## Input: 
### num_players: int, must be between 2 to 4, number of players in the game
### random_seed: int, use to set random seed
### num_rolls: int, number of rolls in the game, must be between 10 and 200.

## Output: 
a list with two dataframes. return_prob_dat: dataframe with 6 columns: player_id, sum_expect_prob, num_cards, obs_over_expect, lucky_rel_rank, roll_num;  cuml_dat: a dataframe with two columsn: player_id, log_cuml_obs_over_expect
