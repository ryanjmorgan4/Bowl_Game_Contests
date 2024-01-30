# Run the initial information

# Find who had the most differences
# Find who had the fewest differences
# Find the average number different
# Find which games were unanimous
# Make that "Number Different" matrix

library(dplyr)

BowlGames_2020_pluscat <- as.data.frame(read.csv("C:\\Users\\Ryan\\Desktop\\Home\\2020 Bowl Games\\bowlgames_2020.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE)
BowlGames_2020 <- BowlGames_2020_pluscat %>% select(-Wrigley_Picks)


Unique_Games <- function(picks_dataframe){
  
  Participants <- gsub(colnames(picks_dataframe %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")
  
  startcol = 1+ncol(picks_dataframe)-length(Participants)
  endcol = ncol(picks_dataframe)
  
  test =apply(apply(picks_dataframe[,startcol:endcol], MARGIN = 2, FUN=function(x)
    x == picks_dataframe$Team1
  ),
  MARGIN = 1,
  FUN = sum
  )
  
  Temp = picks_dataframe %>% mutate(Num_Picked_Team1 = test)
  
  unanimous_games <- Temp %>% filter(Bowl != "National Championship") %>%
    filter(Num_Picked_Team1 == 0 | Num_Picked_Team1 == length(Participants))%>%
    select(-c("Team1_FPI_Prob","Key","Num_Picked_Team1"))
  
  
  #Find Games that only 1 person picked a team
  
  one_person_picked <- Temp %>% filter(Bowl != "National Championship") %>%
    filter(Num_Picked_Team1 == 1 | Num_Picked_Team1 == length(Participants)-1)%>%
    select(-c("Team1_FPI_Prob","Key","Num_Picked_Team1"))
  
  return_list = list(Unanimous_Games = unanimous_games, Only_One_Picker = one_person_picked)
  
  return(return_list)
  
}


Unique_Games(BowlGames_2020)
Unique_Games(BowlGames_2020_pluscat)


pick_comparisons <- function(picks_dataframe){
  
  Participants <- gsub(colnames(picks_dataframe %>% select(contains("_Picks"))),pattern = "_Picks",replacement = "")
  
  #Make a 2 column dataframe with each combination of matchups
  
  Participants_DF = data.frame(Person = rep(Participants, length(Participants)),
                               Person2 = rep(Participants, each = length(Participants)), stringsAsFactors = FALSE)
  
  
  All_Comparisons <- Participants_DF %>% mutate(Num_Games = length(picks_dataframe[,1]),
                             Similar_Picks = apply(Participants_DF, MARGIN=1, 
                                                   FUN=function(x) sum(picks_dataframe[,paste(x[1],"_Picks",sep="")] == picks_dataframe[,paste(x[2],"_Picks",sep="")])),
                             Different_Picks = Num_Games - Similar_Picks
  ) %>%
    filter(Person != Person2) %>% filter(Person < Person2) %>% arrange(-Similar_Picks)
  
  
  #Find the average different from each person
  Comparisons_Summary <-Participants_DF %>% mutate(Num_Games = length(picks_dataframe[,1]),
                             Similar_Picks = apply(Participants_DF, MARGIN=1, 
                                                   FUN=function(x) sum(picks_dataframe[,paste(x[1],"_Picks",sep="")] == picks_dataframe[,paste(x[2],"_Picks",sep="")])),
                             Different_Picks = Num_Games - Similar_Picks
  ) %>%
    filter(Person != Person2) %>% group_by(Person) %>% summarise(Most_Different = max(Different_Picks),
                                                                   Least_Different = max(Similar_Picks),
                                                                   Avg_Different = mean(Different_Picks),
                                                                   Avg_Similar = mean(Similar_Picks))
  
  
  #Least Similar Person
  
  Most_Different_To = Participants_DF %>% mutate(Num_Games = length(picks_dataframe[,1]),
                             Similar_Picks = apply(Participants_DF, MARGIN=1, 
                                                   FUN=function(x) sum(picks_dataframe[,paste(x[1],"_Picks",sep="")] == picks_dataframe[,paste(x[2],"_Picks",sep="")])),
                             Different_Picks = Num_Games - Similar_Picks
  ) %>%
    filter(Person != Person2)  %>% arrange(-Similar_Picks) %>% group_by(Person) %>% slice(c(n())) %>%
    mutate(Most_Different_To = Person2) %>% select(Most_Different_To)
  
  
  #Most Similar Person
  
  
  Least_Different_To = Participants_DF %>% mutate(Num_Games = length(picks_dataframe[,1]),
                             Similar_Picks = apply(Participants_DF, MARGIN=1, 
                                                   FUN=function(x) sum(picks_dataframe[,paste(x[1],"_Picks",sep="")] == picks_dataframe[,paste(x[2],"_Picks",sep="")])),
                             Different_Picks = Num_Games - Similar_Picks
  ) %>%
    filter(Person != Person2)  %>% arrange(-Similar_Picks) %>% group_by(Person) %>% slice(c(1)) %>%
    mutate(Least_Different_To = Person2) %>% select(Least_Different_To)
  
  
  Comparisons_Summary <- right_join(Comparisons_Summary, right_join(Most_Different_To, Least_Different_To), by = "Person")
  
  Comparisons_Summary <- Comparisons_Summary[, c(1,6,2,7,3,4,5)]
  
  #Different picks matrix
  
  Different_picks <- Participants_DF %>% mutate(Num_Games = length(picks_dataframe[,1]),
                                                Similar_Picks = apply(Participants_DF, MARGIN=1, 
                                                                      FUN=function(x) sum(picks_dataframe[,paste(x[1],"_Picks",sep="")] == picks_dataframe[,paste(x[2],"_Picks",sep="")])),
                                                Different_Picks = Num_Games - Similar_Picks
  ) 
  
  
  Difference_Matrix <- as.data.frame(matrix(data=Different_picks$Different_Picks, nrow= length(Participants), ncol = length(Participants)))
  
  colnames(Difference_Matrix) = Participants
  rownames(Difference_Matrix) =Participants
  
  
  return_list = list(All_Comps = All_Comparisons, Comp_Summary= Comparisons_Summary, Difference_Matrix = Difference_Matrix)
  
  return(return_list)
}

pick_comparisons(BowlGames_2020)
pick_comparisons(BowlGames_2020_pluscat)
