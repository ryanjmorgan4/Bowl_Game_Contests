# Running simimulations to find probability someone wins

# Step 1: Find a way to make it so you will create simulations for the remaining games (want to retain ones that have already happened)
# Step 2: Run n simulations
# Step 3: Find scores of people of those n simulations
# Step 4: Find what percent each person wins



convert_picks_to_1_2 <- function(picks_vector, team1_team2_dataframe, 
                                 team1_champ = "Alabama",
                                 team2_champ = "Clemson",
                                 team3_champ = "Ohio State",
                                 team4_champ = "Notre Dame"){
  output_vector = rep(NA,length(picks_vector))
  output_vector[picks_vector == team1_team2_dataframe$Team1] =1
  output_vector[picks_vector == team1_team2_dataframe$Team2] = 2
  
  champ_pick = picks_vector[length(picks_vector)]
  
  if (is.na(champ_pick)) {champ_pick = "NA"}
  
  
  if (champ_pick == team1_champ) {output_vector[length(picks_vector)] = 1}
  if (champ_pick == team2_champ) {output_vector[length(picks_vector)] = 2}
  if (champ_pick == team3_champ) {output_vector[length(picks_vector)] = 3}
  if (champ_pick == team4_champ) {output_vector[length(picks_vector)] = 4}
  
  return(output_vector)
}




simulation_df_prep <- function(picks_dataframe){
  
  #convert the fpi's so that games already played have accurate FPI's
  new_fpi = picks_dataframe$Team1_FPI_Prob
  
  new_fpi[picks_dataframe$Key == picks_dataframe$Team1] = 1
  new_fpi[picks_dataframe$Key == picks_dataframe$Team2] = 0
  
  new_df=picks_dataframe
  new_df$Team1_FPI_Prob = new_fpi
  
  #Convert the game picks to either 1 or 2:
  
  
  Participants <- gsub(colnames(picks_dataframe %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")
  
  startcol = ncol(picks_dataframe)-length(Participants)
  endcol = ncol(picks_dataframe)
  
  new_picks <- apply(picks_dataframe[,startcol:endcol], MARGIN = 2, FUN = convert_picks_to_1_2, team1_team2_dataframe = picks_dataframe[,2:3])
  
  new_df[,startcol:endcol] = new_picks
  
  return(new_df)
  
  
}


calculate_score <- function(key_vector, picks_vector, 
                            index_ny6_games = c(14, 18, 20, 21, 24, 25, 26), 
                            index_semis = c(20, 21, 26),
                            index_champ = c(26)){
  return(
    sum(
      initial_score = sum(key_vector == picks_vector),
      tiebreaker1 = sum(key_vector[index_ny6_games] == picks_vector[index_ny6_games])/10,
      tiebreaker2 = sum(key_vector[index_semis] == picks_vector[index_semis])/100,
      tiebreaker3 = sum(key_vector[index_champ]== picks_vector[index_champ])/1000
    )
  )
  
}



random_scenario_fast <- function(prob_vector, key_vector){
  #Randomly select 1's and 0's
  
  numgames = length(key_vector)
  
  games_left = sum(is.na(key_vector))
  
  completed_games = numgames-games_left
  
  startsims = completed_games +1;
  
  updated_prob_vector = prob_vector[startsims:numgames]
  updated_prob_vector = 1-updated_prob_vector
  
  if (completed_games ==0) {return_vector = rbinom(length(updated_prob_vector), 1, updated_prob_vector) + 1}
  else{
  completed_games_vector = key_vector[1:completed_games]
  
  
  return_vector =(c(completed_games_vector, rbinom(length(updated_prob_vector), 1, updated_prob_vector) + 1))
  }
  
  return(return_vector)
}


multiple_random_scenarios_fastest <- function(prob_vector, key_vector, number_of_scenarios){
  
  return_list = random_scenario_fast(prob_vector, key_vector)
  
  counter = 2
  
  while(counter<number_of_scenarios+1){
    
    return_list = c(return_list, random_scenario_fast(prob_vector, key_vector))
    
    counter=counter+1
  }
  
  return_df = as.data.frame(matrix(return_list, nrow = length(prob_vector), ncol = number_of_scenarios, byrow= FALSE))
  
  return(return_df)  
}






#Make it so simulations take into account winner of championship must win semis

#Rose Bowl: 
# 1: Alabama    2: Notre Dame

#Sugar Bowl:
# 1: Clemson 2: Ohio State


#Championship:
# 1: Alabama      2: Clemson     3: Ohio State    4: Notre Dame
# 1: Rose 
# 2: Sugar

convert_champ <- function(scenario_vector, 
                          oneseed_semi = 20,
                          twoseed_semi = 21,
                          championship_index = 26){
  
  return_vector = scenario_vector
  
  if (return_vector[oneseed_semi] == 2 & return_vector[championship_index] == 1){
    return_vector[championship_index] = 4
  }
  
  if (return_vector[twoseed_semi] == 2 & return_vector[championship_index] == 2){
    return_vector[championship_index] = 3
  }
  
  return(return_vector)
  
}



add_Winner_Column <- function(Input_Df){
  
  FindWinner <- Input_Df %>% select(contains("_Score"))
  
  firstWinner <- colnames(FindWinner)[max.col(FindWinner,ties.method="first")]
  
  lastWinner <- colnames(FindWinner)[max.col(FindWinner,ties.method="last")]
  
  ReturnWinners = gsub(firstWinner,pattern = "_Score", replacement = "")
  ReturnWinners[firstWinner !=lastWinner] = "Tie"
  
  
  return(ReturnWinners)
  
}




run_simulations_function <- function(picks_dataset, numberofsimulations){
  ptm <- proc.time()
  
  #Prep dataset
  prepped_dataset <- simulation_df_prep(picks_dataset)
  
  #Run sims
  simulated_results <- multiple_random_scenarios_fastest(prepped_dataset$Team1_FPI_Prob, prepped_dataset$Key, numberofsimulations)
  
  #fix championshipgames of sims
  
  simulated_results <- as.data.frame(apply(simulated_results, MARGIN = 2, FUN = convert_champ))
  

  #find everyones score
  
  Participants <- gsub(colnames(prepped_dataset %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")
  
  startcol = 1+ncol(prepped_dataset)-length(Participants)
  
  endcol = ncol(prepped_dataset)
  
  participants_picks_colnumbers = startcol:endcol
  
  Scores_DF<- data.frame(lapply(participants_picks_colnumbers, FUN = function(x) 
    apply(simulated_results, MARGIN = 2, FUN= calculate_score, picks_vector = prepped_dataset[,x])))
  
  colnames(Scores_DF) = paste(Participants,"_Score",sep="")
  
  #find all the winners
  Scores_DF$winners <- add_Winner_Column(Scores_DF)
  
  
  #Find probability each person would win
  
  Probabilities <- Scores_DF %>% group_by(winners) %>% summarise(NumberTimesWon = n(), Percent_Won = NumberTimesWon/length(Scores_DF$winners))
  
  time_to_run=proc.time()-ptm
  
  return_list = list(time_to_run = time_to_run, 
                     simulations = simulated_results,
                     Scores_of_simulations = Scores_DF,
                     Probability_Of_Winning = Probabilities)
  
  return(return_list)
}

BowlGames_2020_pluscat <- as.data.frame(read.csv("C:\\Users\\Ryan\\Desktop\\Home\\2020 Bowl Games\\bowlgames_2020.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE)
BowlGames_2020 <- BowlGames_2020_pluscat %>% select(-Wrigley_Picks)



og_2020 <- run_simulations_function(picks_dataset = BowlGames_2020, numberofsimulations = 10000)
og_2020$time_to_run
og_2020$Probability_Of_Winning %>% arrange(-Percent_Won)

cat_2020 <- run_simulations_function(picks_dataset = BowlGames_2020_pluscat, numberofsimulations = 10000)
cat_2020$time_to_run
cat_2020$Probability_Of_Winning %>% arrange(-Percent_Won)





