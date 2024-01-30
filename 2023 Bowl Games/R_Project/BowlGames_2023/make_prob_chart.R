
library(tidyverse)

#Probability Graph


probabilities_plus_cat <- (as.data.frame(read.csv("/Users/Ryan/Desktop/Home/2023 Bowl Games/Prob_Tracker_Cat.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))
probabilities <- (as.data.frame(read.csv("/Users/Ryan/Desktop/Home/2023 Bowl Games/Prob_Tracker.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))

probabilities_plus_cat %>% gather(value = Probs, key = Person,Aidan:Tie)%>%
  filter(!is.na(Probs)) %>%
  ggplot(aes(x=Games_Played, y = Probs, group = Person, color = Person, lty=Person))+geom_line(linewidth=1)+theme(legend.position = "bottom")+
  scale_linetype_manual(values=c("solid", "dashed","dotted","solid","dashed","dotted","solid","dashed","dotted","solid","dashed","dotted","solid"))+
  scale_color_manual(values=c('red','red',"red","blue","blue","blue","darkgreen","darkgreen","darkgreen","darkorange","darkorange","darkorange","purple"))

probabilities %>% gather(value = Probs, key = Person,Aidan:Tie)%>%
  filter(!is.na(Probs)) %>%
  ggplot(aes(x=Games_Played, y = Probs, group = Person, color = Person, lty=Person))+geom_line(linewidth=1)+theme(legend.position = "bottom")+
  scale_linetype_manual(values=c("solid", "dashed","dotted","solid","dashed","dotted","solid","dashed","dotted","solid","dashed","dotted"))+
  scale_color_manual(values=c('red','red',"red","blue","blue","blue","darkgreen","darkgreen","darkgreen","darkorange","darkorange","darkorange"))




#Score Graph

Scores_plus_cat <- (as.data.frame(read.csv("/Users/Ryan/Desktop/Home/2023 Bowl Games/Score_Tracker_Cat.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))
Scores <-(as.data.frame(read.csv("/Users/Ryan/Desktop/Home/2023 Bowl Games/Score_Tracker.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE))


Scores_plus_cat %>% gather(value = Score, key = Person,Aidan:Wrigley)%>%
  filter(!is.na(Score)) %>%
  ggplot(aes(x=Games_Played, y = Score, group = Person, color = Person, lty=Person))+geom_line(linewidth=1)+theme(legend.position = "bottom")+
  scale_linetype_manual(values=c("solid", "dashed","dotted","solid","dashed","dotted","solid","dashed","dotted","solid","dashed","solid"))+
  scale_color_manual(values=c('red','red',"red","blue","blue","blue","darkgreen","darkgreen","darkgreen","darkorange","darkorange","purple"))


Scores %>% gather(value = Score, key = Person,Aidan:Ryan)%>%
  filter(!is.na(Score)) %>%
  ggplot(aes(x=Games_Played, y = Score, group = Person, color = Person, lty=Person))+geom_line(linewidth=1)+theme(legend.position = "bottom")+
  scale_linetype_manual(values=c("solid", "dashed","dotted","solid","dashed","dotted","solid","dashed","dotted","solid","dashed","dotted"))+
  scale_color_manual(values=c('red','red',"red","blue","blue","blue","darkgreen","darkgreen","darkgreen","darkorange","darkorange","darkorange"))

