## Bowl Games 2018

library(tidyverse)

Bowls2018 <- read.csv(file="C:\\Users\\Ryan\\Desktop\\Home\\BowlGames2018\\2018Bowl.csv",header=TRUE,stringsAsFactors = F)


Bowls2018


person_df <- data.frame(Person1<- c(rep("Ryan",7),rep("Becca",6),rep("Jason",5),rep("Damon",4),rep("Grandpa",3),rep("Greta",2),"Amber"),
                        Person2 <- c("Becca","Jason","Damon","Grandpa","Greta","Amber","Mom","Jason","Damon","Grandpa","Greta","Amber","Mom",
                                     "Damon","Grandpa","Greta","Amber","Mom","Grandpa","Greta","Amber","Mom","Greta","Amber","Mom","Amber","Mom","Mom"), stringsAsFactors = F)

colnames(person_df) <- c("Person1","Person2")


#Function to Compare people's picks. 


pick_comparisons <- function(person1, person2){
  person1index <- paste("Text_",person1, sep="")
  
  person2index <- paste("Text_",person2, sep="")
  
  
  
  
  person1_picks<- as.character(Bowls2018[,colnames(Bowls2018)==person1index])
  
  person2_picks<- as.character(Bowls2018[,colnames(Bowls2018)==person2index])  
  
  
  numsame = sum(person1_picks == person2_picks)
  
  return(numsame)
}



compare_mult <- function(){
  
  counter = 1
  
  return_list = c(1:28)
  
  while(counter<29){
    return_list[counter]= pick_comparisons(person1 = person_df$Person1[counter], person2 = Person2[counter])
    
    counter=counter+1
  }
  
  return(return_list)
  
}



compare_mult()

person_df$Similar = compare_mult()

person_df %>% arrange(Similar)



person_df_reps<- data.frame(Person1 <- c(person_df$Person1, person_df$Person2))
person_df_reps$Person2 <- c(person_df$Person2, person_df$Person1)
person_df_reps$Similar <- c(person_df$Similar, person_df$Similar)

colnames(person_df_reps)= colnames(person_df)

person_df_reps %>% group_by(Person1) %>% summarise(avg_similar = mean(Similar)) %>% arrange(avg_similar)









pick_agreement <- function(){
  Bowl_picks <- (Bowls2018[,1:12])
  
  Pick_Team_1<- Bowls2018$Team1 == Bowls2018[,5:12]
  
  ret <- apply(Pick_Team_1, MARGIN = 1, FUN=sum)
  
  ret[ret==0] = 8
  ret[ret==1] = 7
  ret[ret==2] = 6
  ret[ret==3]=5
  
  ret_table <- table(ret)
  
  return(ret_table)
}

pick_agreement()


