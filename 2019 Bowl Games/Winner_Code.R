Picks_2020 <- as.data.frame(read.csv("C:\\Users\\MorgRy02\\Desktop\\game_test.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE)



Create_scenario_table <- function(input_df){
  
  num_games = length(input_df$Bowl)
  num_scenarios = 2^num_games
  
  output_df = data.frame(matrix(ncol= num_games+1, nrow = num_scenarios))
  
  colnames(output_df) = c("Scenario", input_df$Bowl)
  
  output_df$Scenario = 1:num_scenarios
  
  
  for (i in 1:num_games){
    Num_Loops = 2^(i-1)
    Num_Reps = 2^(num_games-i)
    
    Teams = c(unlist(input_df[i,2:3]))
    
    Repped = rep(Teams, each = Num_Reps)
    
    New_Column = rep(Repped, Num_Loops)
    
    output_df[,i+1] = New_Column
  }
  

  
  return(output_df)
  
  
}


Bowls <- Picks_2019[,1:3]

ptm <- proc.time()
Scenarios <- Create_scenario_table(Bowls)
proc.time()-ptm

#Update the championship to be accurate

Scenarios[Scenarios$Champ == "Cotton_bowl","Champ"] = Scenarios[Scenarios$Champ == "Cotton_bowl","Cotton"]
Scenarios[Scenarios$Champ == "Fiesta_bowl","Champ"] = Scenarios[Scenarios$Champ == "Fiesta_bowl","Fiesta"]



#Find People's Score?

#Transpose the scenarios
ptm <- proc.time()
Scenarios_t <- as.data.frame(t(x = Scenarios[,-1]),stringsAsFactors = FALSE)
proc.time() - ptm


#Find Scores for each scenario

#Score Dataframe

find_score <- function(picks_vector, result_vector){

  return(sum(sum(picks_vector == result_vector),sum(picks_vector[17:23] == result_vector[17:23])/10, 
         sum(picks_vector[15:23] == result_vector[15:23])/100,sum(picks_vector[23] == result_vector[23])/1000))
  
  
}

find_scores_all_scenarios <- function(picks_vector, scenarios_df){
  num_scenarios = length(scenarios_df[1,])
  
  return_list = c(find_score(picks_vector, scenarios_df[,1]))
  
  counter = 2
  
  while (counter < num_scenarios+1){
    return_list[counter] = find_score(picks_vector, scenarios_df[,counter])
    
    counter=counter+1
    
  }
  
  return(return_list)
  
  
}

find_score(picks_vector = Picks_2019$Ryan_Picks, result_vector = Scenarios_t$V1)


ptm <- proc.time()
Scenarios$Ryan_Score =apply(Scenarios_t, MARGIN = 2, FUN=find_score, picks_vector = Picks_2019$Ryan_Picks)
Scenarios$Jason_Score = apply(Scenarios_t, MARGIN = 2, FUN=find_score, picks_vector = Picks_2019$Jason_Picks)
Scenarios$Amber_Score = apply(Scenarios_t, MARGIN = 2, FUN=find_score, picks_vector = Picks_2019$Amber_Picks)
Scenarios$Becca_Score = apply(Scenarios_t, MARGIN = 2, FUN=find_score, picks_vector = Picks_2019$Becca_Picks)
proc.time()-ptm

#Find Winners


### Could try and write a function to find if there is a winner (and give that winner) or if there is a tie...


add_Winner_Column <- function(Input_Df){
  
  FindWinner <- Input_Df %>% select(contains("_Score"))
  
  firstWinner <- colnames(FindWinner)[max.col(FindWinner,ties.method="first")]
  
  lastWinner <- colnames(FindWinner)[max.col(FindWinner,ties.method="last")]
  
  ReturnWinners = gsub(firstWinner,pattern = "_Score", replacement = "")
  ReturnWinners[firstWinner !=lastWinner] = "Tie"
  
  output_df = Input_Df
  output_df$Winner = ReturnWinners
  
  return(output_df)
  
}

ptm <- proc.time()
Scenarios = add_Winner_Column(Scenarios)
proc.time()-ptm


#Only filter it down to the ones that match the key

num_completed_games <- sum(!is.na(Picks_2019$Key))


Filtered_Scenarios <- Scenarios %>% filter(Corn == Picks_2019$Key[1],
                                           Mitten == Picks_2019$Key[2])



#Find Probability of occuring (FPI)

find_prob_single <- function(team1_team2_fpi_df , scenario_vector){
  
  prod_list = c(1)
  
  num_games = length(scenario_vector)
  
  counter = 1
  
  while (counter < num_games){
    
    if (scenario_vector[counter] == team1_team2_fpi_df[counter,1]) {
      prod_list[num_games-counter] = team1_team2_fpi_df[counter,3]
    }
    else{
      prod_list[num_games-counter] = 1-team1_team2_fpi_df[counter,3]      
    }

    counter=counter+1
    
  }
  
  return(prod(prod_list,.5))
  
}


find_prob_single(Picks_2019[,2:4], Scenarios_t[,4000000])



#find prob of occuring, all scenarios

find_prob_mult <- function(team1_team2_fpi_df, scenarios_df){
  
  num_scenarios = length(scenarios_df[1,])
  
  return_probs = c(find_prob_single(team1_team2_fpi_df, scenarios_df[,1]))
  
  counter = 2
  
  while (counter < num_scenarios+1){
    return_probs[counter] = c(find_prob_single(team1_team2_fpi_df, scenarios_df[,counter]))
    
    counter=counter+1
    
  }
  
  return(return_probs)
  
  
}

ptm <- proc.time()
Scenarios$Probability = find_prob_mult(Picks_2019[,2:4], Scenarios_t)
proc.time()-ptm



ptm <- proc.time()

Scenarios$Probability= apply(Scenarios_t, MARGIN = 2, FUN = find_prob_single, team1_team2_fpi_df = Picks_2019[,2:4])

proc.time() - ptm

ptm <- proc.time()
Scenarios  %>% group_by(Winner) %>% summarise(Prob = sum(Probability))
proc.time()-ptm

#Find Probability, given some event occurs

Prob_Sum = sum(Scenarios %>% filter(Cotton == "Oklahoma") %>%select(Probability))

Scenarios %>% filter(Cotton == "Oklahoma") %>% group_by(Winner) %>% 
  summarise(Prob_overall = sum(Probability), Prob_Filtered = Prob_overall/Prob_Sum)














#### Simulation Start:


team2probs <- c(.45, .32, .68, .72, .46, .8)
team1probs = 1-team2probs

Games <- data.frame(team1 = c("Nebraska","Wisconsin","Michigan","Auburn","Georgia Tech", "Rutgers"),
                    team2 = c("Iowa","Minnesota", "Ohio State", "Alabama", "Georgia", "Penn State"),
                    stringsAsFactors =  FALSE)

random_scenario <- function(Games_df, prob_vector){
  #Randomly select 1's and 0's
  
  scenario <- rbinom(length(probs), 1, probs) + 1
  
  return_vector <- c(0)
  
  counter = 1
  
  while (counter < length(probs)+1){
    return_vector[counter] = Games_df[counter,scenario[counter]]
    
    counter = counter+1
  }
  
  return(return_vector)
  
}

random_scenario(Games, team1probs)


multiple_random_scenarios <- function(Games_df, prob_vector, number_of_scenarios){
  
  Scenario1 = random_scenario(Games_df, prob_vector)
  return_df = data.frame("S1" = Scenario1)
  
  counter = 2
  
  while(counter<number_of_scenarios+1){
    #new_name = paste("S",counter, sep="")
    #return_df$new = random_scenario(Games_df, prob_vector)
    #colnames(return_df)[counter] = new_name
    
    new_scenario = as.data.frame(random_scenario(Games_df, prob_vector))
    
    return_df = bind_cols(return_df, new_scenario)
    
    
    
    counter=counter+1
  }
  
  return(return_df)  
}

ptm <- proc.time()
simulations_1000 <- multiple_random_scenarios(Games_df = Games, prob_vector = team1probs, number_of_scenarios =  1000)
proc.time() - ptm


multiple_random_scenarios_fast <- function(Games_df, prob_vector, number_of_scenarios){
  
  return_list = random_scenario(Games_df, prob_vector)
  
  counter = 2
  
  while(counter<number_of_scenarios+1){
    #new_name = paste("S",counter, sep="")
    #return_df$new = random_scenario(Games_df, prob_vector)
    #colnames(return_df)[counter] = new_name
    
    return_list = c(return_list, random_scenario(Games_df, prob_vector))
    
    
    counter=counter+1
  }
  
  return_df = as.data.frame(matrix(return_list, nrow = length(prob_vector), ncol = number_of_scenarios, byrow= FALSE),
                            stringsasfactors=FALSE)
  
  return(return_df)  
}


ptm <- proc.time()
simulations_1000 <- multiple_random_scenarios(Games_df = Games, prob_vector = team1probs, number_of_scenarios =  10000)
proc.time() - ptm

ptm<- proc.time()
sims_1000_fast <- multiple_random_scenarios_fast(Games, team1probs, 1000)
proc.time() - ptm





random_scenario_fast <- function(Games_df, prob_vector){
  #Randomly select 1's and 0's
  
  return(rbinom(length(prob_vector), 1, prob_vector) + 1)
  
}


multiple_random_scenarios_fastest <- function(Games_df, prob_vector, number_of_scenarios){
  
  return_list = random_scenario_fast(Games_df, prob_vector)
  
  counter = 2
  
  while(counter<number_of_scenarios+1){
    
    return_list = c(return_list, random_scenario_fast(Games_df, prob_vector))
    
    counter=counter+1
  }
  
  return_df = as.data.frame(matrix(return_list, nrow = length(prob_vector), ncol = number_of_scenarios, byrow= FALSE))
  
  return(return_df)  
}

ptm<- proc.time()
sims_1000_fastest <- multiple_random_scenarios_fastest(Games, team1probs, 1000)
proc.time() - ptm

t_sims <- as.data.frame(t(sims_1000_fastest))
table(t_sims$V1)