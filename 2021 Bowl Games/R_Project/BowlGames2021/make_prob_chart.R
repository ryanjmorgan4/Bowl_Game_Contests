
library(tidyverse)

#Probability Graph


probabilities_plus_cat <- (as.data.frame(read.csv("C:\\Users\\Ryan\\Desktop\\Home\\2021 Bowl Games\\Prob_Tracker_Cat.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))
probabilities <- (as.data.frame(read.csv("C:\\Users\\Ryan\\Desktop\\Home\\2021 Bowl Games\\Prob_Tracker.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))

probabilities_plus_cat %>% gather(value = Probs, key = Person,Amber:Tie)%>%
  filter(!is.na(Probs)) %>%
  ggplot(aes(x=Games_Played, y = Probs, group = Person, color = Person))+geom_line(size=1)+theme(legend.position = "bottom")

probabilities %>% gather(value = Probs, key = Person,Amber:Tie)%>%
  filter(!is.na(Probs)) %>%
  ggplot(aes(x=Games_Played, y = Probs, group = Person, color = Person))+geom_line(size=1)+theme(legend.position = "bottom")




#Score Graph

Scores_plus_cat <- (as.data.frame(read.csv("C:\\Users\\Ryan\\Desktop\\Home\\2021 Bowl Games\\Score_Tracker_Cat.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))
Scores <-(as.data.frame(read.csv("C:\\Users\\Ryan\\Desktop\\Home\\2021 Bowl Games\\Score_Tracker.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))


Scores_plus_cat %>% gather(value = Score, key = Person,Amber:Ryan)%>%
  filter(!is.na(Score)) %>%
  ggplot(aes(x=Games_Played, y = Score, group = Person, color = Person))+geom_line(size=1)+theme(legend.position = "bottom")


Scores %>% gather(value = Score, key = Person,Amber:Ryan)%>%
  filter(!is.na(Score)) %>%
  ggplot(aes(x=Games_Played, y = Score, group = Person, color = Person))+geom_line(size=1)+theme(legend.position = "bottom")

