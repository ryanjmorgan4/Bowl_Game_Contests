# Run the easy, score update information

# Calculate the current score
# Find the current number different that everyone still has
# Make that "different matrix"
# Determine who has been eliminated 
# Find what someone's best possible scenario is (and where they place)

library(tidyverse)

BowlGames_2022_pluscat <- as.data.frame(read.csv("C:\\Users\\Ryan\\OneDrive\\Desktop\\Home\\2022 Bowl Games\\bowlgames_2022.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE)
BowlGames_2022 <- BowlGames_2022_pluscat %>% select(-Wrigley_Picks)





#Score_Function


#New Years Six Games:
#   Orange Bowl (row 34)
#   Sugar Bowl (row 36)
#   Fiesta Bowl (row 37)
#   Peach Bowl (row 38)
#   Cotton Bowl (row 41)
#   Rose Bowl (row 42)
#   National Championship (row 43)

#Semi-Final Games:
#   Fiesta Bowl (semi final) (row 37)
#   Peach Bowl (semi final) (row 38)
#   National Championship (row 43)

#Championship Game:
#   National Championship (row 43)


calculate_score <- function(key_vector, picks_vector, 
                            index_ny6_games = c(34, 36, 37, 38, 41, 42, 43), 
                            index_semis = c(37, 38, 43),
                            index_champ = c(43),
                            cancelled_games = c(46)
                            ){
  return(
    sum(
  initial_score = sum(key_vector[-cancelled_games] == picks_vector[-cancelled_games]),
  tiebreaker1 = sum(key_vector[index_ny6_games] == picks_vector[index_ny6_games])/10,
  tiebreaker2 = sum(key_vector[index_semis] == picks_vector[index_semis])/100,
  tiebreaker3 = sum(key_vector[index_champ]== picks_vector[index_champ])/1000
    )
  )
  
}


calculate_score_comparison <- function(picks_dataframe){

    Participants <- gsub(colnames(picks_dataframe %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")

    startcol = 1+ncol(picks_dataframe)-length(Participants)
    endcol = ncol(picks_dataframe)
    
    Scores= apply(picks_dataframe[,startcol:endcol], MARGIN = 2, FUN = calculate_score, key_vector = picks_dataframe$Key)

    return_df = data.frame(Participants = Participants,
                             Score = Scores)

    rownames(return_df) <- c()

    return_df <- return_df %>% arrange(-Score)
    
    return(return_df)

}





#Different Picks Remaining Function

different_picks_remaining <- function(picks_dataframe){

  Participants <- gsub(colnames(picks_dataframe %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")

  Participants_DF = data.frame(Person = rep(Participants, length(Participants)),
                               Person2 = rep(Participants, each = length(Participants)), stringsAsFactors = FALSE)
  
  updated_picks_df <- picks_dataframe %>% filter(Key == "")
  
  Different_picks <- Participants_DF %>% mutate(Num_Games = length(updated_picks_df[,1]),
                                              Similar_Picks = apply(Participants_DF, MARGIN=1, 
                                                                    FUN=function(x) sum(updated_picks_df[,paste(x[1],"_Picks",sep="")] == updated_picks_df[,paste(x[2],"_Picks",sep="")])),
                                              Different_Picks = Num_Games - Similar_Picks

                                              ) 



  Difference_Matrix <- as.data.frame(matrix(data=Different_picks$Different_Picks, nrow= length(Participants), ncol = length(Participants)))


  colnames(Difference_Matrix) = Participants

  rownames(Difference_Matrix) =Participants
  
  return(Difference_Matrix)

}





#Create Someone's "best scenario"

best_scenario_function <- function(key_vector, picks_vector){
  best_scenario = key_vector
  
  best_scenario[key_vector == ""] = picks_vector[key_vector == ""]
  
  return(best_scenario)
}


#Check if someone wins their "best scenario"

best_scenario_check <- function(picks_dataframe, person_name){
  picks_vector_name = paste(person_name, "_Picks",sep="")
  picks_vector = picks_dataframe[,picks_vector_name]
  key_vector= picks_dataframe$Key
  
  best_scenario = best_scenario_function(key_vector, picks_vector)
  
  best_scenario_df = picks_dataframe
  best_scenario_df$Key = best_scenario
  
  score_df <-calculate_score_comparison(best_scenario_df)
  max_score <- score_df %>% slice(c(1)) %>% select(Score)
  your_score <- score_df %>% filter(Participants == person_name) %>% select(Score)

  
  if (max_score == your_score) {return(TRUE)} else {return(FALSE)}
  
  
}


check_if_still_in_contention <- function(picks_dataframe){


  Participants <- gsub(colnames(picks_dataframe %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")


  Contention <- unlist(lapply(Participants, FUN = best_scenario_check, picks_dataframe = picks_dataframe))


  Contenders_df <- data.frame(Participants = Participants,
                            Still_In_Contention = Contention)


  return(Contenders_df)

}



calculate_score_comparison(BowlGames_2022)
check_if_still_in_contention(BowlGames_2022)
different_picks_remaining(BowlGames_2022)



calculate_score_comparison(BowlGames_2022_pluscat)
check_if_still_in_contention(BowlGames_2022_pluscat)
different_picks_remaining(BowlGames_2022_pluscat)


