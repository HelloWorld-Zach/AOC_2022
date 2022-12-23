setwd("C:/Users/15517/Downloads")
library(tidyverse)
rm(list=setdiff(ls(), c("actual", "algebra", "evaluator", "p2eval","before", "final"))) #clears object environment 
Day21 <- tibble(readLines("2022_d21.txt")) %>% rename(main = `readLines("2022_d21.txt")`) %>% separate(col = main, into=c("left", "right"), sep = ":")
numbers <- Day21 %>% filter(str_count(right, "[0-9]") != 0 ) %>% mutate(right = as.double(right))

for(i in seq_len(nrow(numbers))){
eval(parse(text=paste0(numbers$left[i], "<-", numbers$right[i])))
}

operations <- Day21 %>% anti_join(numbers, by=c("left")) %>% mutate(right = str_remove_all(right, "^ ")) #removes space in the beginning 

algebra <- function(a, b , operation , positions){
  
  if(operation == "+" && positions == c(1,1,0)){
    
    return(a-b)
  }
  else if(operation == "+" && positions == c(1,0,1)){
    
    return(a-b)
  }
  else if(operation == "+" && positions == c(0,1,1)){
    
    return(a+b)
  }
  else if(operation == "-" && positions == c(1,1,0)){
    
    return(b-a)
  }
  else if(operation == "-" && positions == c(1,0,1)){
    
    return(a+b)
  }
  else if(operation == "-" && positions == c(0,1,1)){
    
    return(a-b)
  }
  else if(operation == "*" && positions == c(1,1,0)){
    
    return(a/b)
  }
  else if(operation == "*" && positions == c(1,0,1)){
    
    return(a/b)
  }
  else if(operation == "*" && positions == c(0,1,1)){
    
    return(a*b)
  }
  else if(operation == "/" && positions == c(1,1,0)){
    
    return(b/a)
  }
  else if(operation == "/" && positions == c(1,0,1)){
    
    return(a*b)
  }
  else if(operation == "/" && positions == c(0,1,1)){
    
    return(a/b)
  }
}
evaluator <- function(n){
  op <- str_split(operations$right[n], " ")[[1]][2]
  input <- str_split(operations$right[n], " ")[[1]][-2]
  total <- append(operations$left[n], input)
  positions <- vapply(total, FUN = exists, FUN.VALUE = numeric(1))
  
  if(sum(positions) %in% c(0,1,3)){
    return(invisible(0))
  }
  
  else if(sum(positions) == 2){
     a <- eval(parse(text = total[which(positions==1)][1]))
     b <- eval(parse(text = total[which(positions==1)][2]))
    assign(total[which(positions==0)], algebra(a,b, op, positions), envir = .GlobalEnv)
    
    }
  }

repeat{                                             #repeat this for loop until we have calculated the values of enough strings to find roots value, the last value found
  
  if(exists("root")) break 
  for(i in 1:nrow(operations)){
 evaluator(i)
  }
}
options(scipen=999) #remove scientific notation 
root  #answer -----------------------------> 63119856257960


operations[operations$left=="root",] 

#PART 2: (What value of humn such that cgdh = qhpl ?)
rm(humn)
path <- c()
root = 63119856257960
repeat{                                             
  
  if(exists("humn")) break 
  for(i in 1:nrow(operations)){
    path <- append(path, evaluator(i))
  }
}
before <- which(path != 0) %% nrow(operations)
before[before == 0] <- nrow(operations)
before <- before[1:which(before==853)]
actual <- which(path != 0) %% nrow(operations)
actual[actual == 0] <- nrow(operations)  #this is the path our algorithm took to fill in all the values
actual <- actual[which(actual ==which(operations$left == "root")):length(actual)] #eliminate starting values, we just assign them per our first part solution  - 67 in length 



            
rm(humn)
path1 <- c()
root = 63119856257960

repeat{                                             
  
  if(853 %in% (which(path1 != 0) %% nrow(operations))) break 
  for(i in 1:nrow(operations)){
    path1 <- append(path1, evaluator(i))
  }
}

final <- operations[actual,] #plug this thing into online math equation solver 
# final <- final[67:1, ]
#answer ---------------------------------------->              
final %>% print(n=6)


31559928128980==((((26377562223665-(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((x-960)*7)+798)*0.5)+566))*11)-7)*0.125)-13)*0.5)+373)*3)-55)*5)+764)*(1/3))+294)*2)-678)*0.5)+127)*2)-999)*(1/7))+357)*0.5)-601)*10)-514)*2)+376)*0.25)-572)*4)+510)*0.1)+553)*11)-903)*0.2)-68)*3)+920)+463)*0.2)+860)*0.5)-988)*6)+917)+52)*(1/3))-216)*6)-816)*(1/3))+269)*3)+637)*2)-913)*(1/11)))*(1/6))+394)*10)




