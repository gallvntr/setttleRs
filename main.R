## Objective: Create a real-time luck metric for each player during a Settlers of Catan simulation. 
## Input: num_players: int, must be between 2 to 4, number of players in the game
## random_seed: int, use to set random seed
## num_rolls: int, number of rolls in the game, must be between 10 and 200.

## Output: a list with two dataframes. return_prob_dat: dataframe with 6 columns: player_id, sum_expect_prob, num_cards, obs_over_expect, lucky_rel_rank, roll_num;  cuml_dat: a dataframe with two columsn: player_id, log_cuml_obs_over_expect

library(tidyverse)
library(data.table)

settlers_sim <- function(num_players = 3, random_seed = 123, num_rolls = 100) {
  
  stopifnot(num_players %in% 2:4)
  stopifnot(num_rolls %in% 10:200)
  
  set.seed(random_seed)
  ## Step 1. Set-up number of players
  
  player_dat <- data.frame(ID = 1:num_players)
  
  ## Step 2. Create the hexagonal board 
  ##Background SO posts: https://stackoverflow.com/questions/5040295/data-structure-for-settlers-of-catan-map
  ## https://stackoverflow.com/questions/1838656/how-do-i-represent-a-hextile-hex-grid-in-memory
  
  #18 playable terrain hexes, 1 desert. Every number from 3 to 11 is on the board twice, and 2 and 12 are listed once. 
  
  ## Number of resources on the board
  resource_vec <- c(rep("wood",4),rep("wheat",4),rep("sheep",4),rep("brick",3),rep("ore",3), "desert")
  
  ## Sequence of hex_ids using the 2D array framework. This is consider an even-r board
  hex_id_seq <- c(seq(3,7, 2), seq(11,35,2), seq(39,43,2))
  
  ## Create vector of all valid hex_ids 
  hex_vec <- rep(NA,45)
  hex_vec[hex_id_seq] <- hex_id_seq
  
  ## All potential numbers on the board 
  roll_vec <- c(2, rep(3:6,2), rep(8:11,2),12)
  
  ## Here is the Catan matrix
  catan_matrix <- matrix(hex_vec, nrow=5, ncol=9, byrow=TRUE)
  
  ## What hex gets what resource? 
  hex_dat <- data.frame(hex_id = hex_id_seq, resource = sample(resource_vec, length(resource_vec), replace = FALSE))
  
  ## What number is associated with each hex? desert doesn't get a number
  hex_dat$roll_number <- NA_integer_
  hex_dat[hex_dat$resource != "desert",]$roll_number <- sample(roll_vec, length(roll_vec), replace = FALSE)
  
  ## There are 54 vertices, or potential locations to put a settlement, in a Catan.
  ### I've manually created evaluated the vertices since it's a rather small grid but can be abstracted mathimatically. 
  
  node_dat <- data.frame(node_id = 1:54)
  
  ## These are all the hex connections for each node. For example, node 1 is connected to hex_id 3.  
  node_dat$connections_list <- 
    list(list(3), list(3),list(3,5),list(5),list(5,7),list(7),list(7),
         list(11),list(3,11),list(3,11,13),list(3,5,13),list(5,13,15),list(5,7,15),list(7,15,17),list(7,17),list(17),
         
         list(19),list(11,19),list(11,19,21),list(11,13,21),list(13,21,23),list(13,15,23),list(15,23,25),list(15,17,25),list(17,25,27),list(17,27),list(17),
         
         list(19),list(19,29),list(19,21,29),list(21,29,31),list(21,23,31),list(23,31,33),list(23,25,33),list(25,33,35),list(25,27,35),list(27,35),list(27),
         
         list(29),list(29,39),list(29,31,39),list(31,39,41),list(31,33,41),list(33,41,43),list(33,35,43),list(35,43),list(35),
         list(39),list(39),list(39,41),list(41),list(41,43),list(43),list(43))
  
  ## cheat and use unnest_wider() to create new columns for our list column
  node_dat <- node_dat %>% tidyr::unnest_wider(connections_list)
  names(node_dat) <- c("node_id","connection1","connection2","connection3")
  
  ## Step 3. Get expected probability of rolling two dice
  
  full_dat <- merge(1:6,1:6,all=TRUE)
  full_dat$sum_roll <- full_dat$x + full_dat$y
  prob_dat <- as.data.frame(table(full_dat$sum_roll))
  prob_dat$prob <- prob_dat$Freq / sum(prob_dat$Freq)
  
  ## Step 4. Roll two dice function
  
  roll_dice <- function(){
    all_rolls <- sample(c(1:6),2, replace=TRUE)
    return(sum(all_rolls))
  }
  
  ## Step 5. Assign probability of each node 
  prob_dat$Freq <- NULL
  
  setDT(hex_dat)
  setDT(node_dat)
  setDT(prob_dat)
  
  hex_dat[prob_dat[, .(roll_number = as.numeric(as.character(Var1)), prob)], prob := i.prob, on = .(roll_number)]
  
  ## There can be at most three connections on one node
  node_dat[hex_dat, connection1_prob := i.prob, on = .(connection1 = hex_id)]
  node_dat[hex_dat, connection2_prob := i.prob, on = .(connection2 = hex_id)]
  node_dat[hex_dat, connection3_prob := i.prob, on = .(connection3 = hex_id)]
  
  node_dat$cumul_prob <- rowSums(node_dat[,c("connection1_prob", "connection2_prob", "connection3_prob")], na.rm=TRUE)
  
  ## Step 6. Players first roll
  
  ## All members roll. Ties roll again
  
  i <- 0
  while (i == 0) {
    player_dat$first_roll <- replicate(num_players, roll_dice())
    if (nrow(player_dat[player_dat$first_roll == player_dat[which.max(player_dat$first_roll),]$first_roll, ]) == 1) {
      i <- 1
    } else {
      i <- 0
    }
  }
  
  ## Player with highest roll goes first. Assume each player picks highest probability hexes. Don't care about ties. 
  
  ## TODO. Snake round in receiving resources. 
  
  ## initialize holdings dat, with highest roll player. Select two hexes. 
  holdings_dat <- data.frame(player_id = player_dat[player_dat$first_roll == max(player_dat$first_roll),]$ID, node_id = node_dat[order(-node_dat$cumul_prob),][1:2]$node_id)
  
  # other player(s) select two hexes at a time. 
  for (i in player_dat[player_dat$first_roll != max(player_dat$first_roll), ]$ID) {
    tmp_dat1 <- data.frame(player_id = i, node_id = node_dat[!(node_id %in% holdings_dat$node_id)][order(-cumul_prob),][1:2]$node_id)
    holdings_dat <- rbind(holdings_dat, tmp_dat1)
  }
  
  ## TODO. Initial card pick up. 
  
  ## Step 7. Run through dumb dummy game 
  
  ## Now we need to create a metric that tracks the difference between the observed and expected probability of each player. During each roll, there is an expected probability that a player gets a card, which is equivalent to the cumulative expected probability of all the hexes on the player's nodes. The probability of multiple independent events occuring is just the multiplication of the probability of each independent event. The observed probability can be boiled down to the number of cards an individual receives each turn. We can then calculate how much more lucky someone has by using the player with the worst luck as the reference player and track this probability over time. 
  
  
  return_prob_dat <- data.table()
  
  full_resource_dat <- data.table(wood = rep(0, num_players), wheat = rep(0, num_players), sheep = rep(0, num_players), brick = rep(0, num_players), ore = rep(0, num_players))
  
  player_resource_dat <- data.table(player_id = paste0("player_",1:num_players), wood = 0, wheat = 0, sheep = 0, brick = 0, ore = 0)
  
  i <- 1 # i is number of rolls
  
  for (i in 0:num_rolls) {
    
    ## account for rolled 7s. ignore rolled 7 for now, we can incorporate this later. 
    j <- 0
    while (j == 0) {
      ROLLED_NUM <- roll_dice()
      if (ROLLED_NUM != 7) {
        j <- 1
      } else {
        j <- 0
      }
    } 
    
    ## What hexes got resources in this non-7 roll? 
    tmp_hex_dat <- hex_dat[roll_number == ROLLED_NUM]
    ## What players have holdings on those hexes?
    node_dat[holdings_dat, player_id := i.player_id, on = .(node_id)]
    tmp_holdings <- node_dat[connection1 %in% tmp_hex_dat[, hex_id] | connection2 %in% tmp_hex_dat[, hex_id] | connection3 %in% tmp_hex_dat[, hex_id] ][!is.na(player_id)]
    
    
    ## If someone got resources:
    if (nrow(tmp_holdings) != 0) {
      
      ## Get expected probabilities 
      tmp_prob_dat <- node_dat[!is.na(player_id), .(sum_expect_prob = sum(cumul_prob)), by = .(player_id)]
      tmp_prob_dat[, player_id := paste0("player_id", player_id)]
      
      ## How many cards did each player get on this turn? 
      
      for (player in sort(unique(tmp_holdings$player_id))) {
        player_id <- player
        tmp_hex_dat[node_dat[player_id ==player], paste0("player_", player_id) := 1, on = .(hex_id = connection1)]
        tmp_hex_dat[node_dat[player_id ==player], paste0("player_", player_id) := 1, on = .(hex_id = connection2)]
        tmp_hex_dat[node_dat[player_id ==player], paste0("player_", player_id) := 1, on = .(hex_id = connection3)]
      }
      
      #tmp_hex_dat_sum <- colSums(tmp_hex_dat[,.SD, .SDcols = !c('hex_id','resource','roll_number','prob')], na.rm=TRUE)
      tmp_hex_dat_sum <- tmp_hex_dat[,.SD, .SDcols = !c('hex_id','roll_number','prob')][, lapply(.SD, sum, na.rm=TRUE), by=resource]
      
      tmp_player_resource_dat <- dcast(melt(tmp_hex_dat_sum, id.vars = "resource"), variable ~ resource)
      
      if (ncol(tmp_player_resource_dat) != 6) {
        missing_vars <- setdiff(names(full_resource_dat), names(tmp_player_resource_dat)[-1])
        
        missing_dat <- full_resource_dat[,.SD, .SDcols = missing_vars]
        tmp_player_resource_dat <- cbind(tmp_player_resource_dat, missing_dat)
      }
      
      player_resource_dat[tmp_player_resource_dat, `:=`(brick = brick+i.brick, wood = wood+i.wood, wheat = wheat+i.wheat, ore = ore+i.ore, sheep = sheep+i.sheep), on = .(player_id = variable)]
      
      ## Merge expected probabilities with number of cards
      
      tmp_prob_dat$num_cards <- rowSums(tmp_player_resource_dat[,c("brick","wood","wheat","sheep","ore")], na.rm=TRUE)
      
      ## Number of resources gained / expected_cumulative_probability
      tmp_prob_dat[, obs_over_expect := num_cards / sum_expect_prob]
      tmp_prob_dat[, lucky_rel_rank := frank(-obs_over_expect)]
      
      tmp_prob_dat[, roll_num := i]
      
      return_prob_dat <- rbind(return_prob_dat, tmp_prob_dat)
      ## TODO. Add post-card pick up activity. 
      
    }
    
    
    ## add to counter
    i <- i+1
  }
  ## Create cumulative metric.
  cuml_dat <- return_prob_dat[, .(log_cuml_obs_over_expect = log(prod(num_cards) / prod(sum_expect_prob))), .(player_id)]
  
  return_list <- list(return_prob_dat, cuml_dat)
  names(return_list) <- c("return_prob_dat", "cuml_dat")
  return(return_list)
}

sim_game <- settlers_sim(num_players = 3, random_seed = 123, num_rolls = 100)

## real-time results
#sim_game[[1]]
sim_game$return_prob_dat

## cumulative game results
#sim_game[[2]]
sim_game$cuml_dat
