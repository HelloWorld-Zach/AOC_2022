#PART 1
library(tidyverse)
setwd("C:/Users/15517/Downloads")
Day8 <- readLines("Day8.txt")
sum(nchar(Day8)) #9801, not too bad at all 
data <- c()
for(i in 1:length(Day8))
data <- append(data, as.numeric(unlist((str_split(Day8[i],""))))) # converts our block of characters to a matrix with numeric values 
tree <- matrix(data, byrow=T, nrow=length(Day8))                      # 99x99 as desired 
P1 <- matrix(0, ncol=97, nrow=97)                                      #initialize matrix with visible indicators 
for(i in 2:98) # do not examine the perimeter 
  for(j in 2:98)
P1[(i-1),(j-1)] <- ifelse(sum(all(tree[i, (j+1):99]<tree[i,j]),all(tree[i, (j-1):1]<tree[i,j]),all(tree[(i+1):99, j]<tree[i,j]), all(tree[(i-1):1, j]<tree[i,j]))!=0,1,0)# 1 means visible, 0 means not visible 

 392 + length(which(P==1))    #ANSWER perimeter value number plus all ones (visible trees not perimeter) #392 == 99*2+97*2
 
 #PART 2: SCENIC SCORE CALCULATIONS
 P2 <- matrix(0, ncol=97, nrow=97)   
 for(i in 2:98) # do not examine the perimeter 
   for(j in 2:98)
     P2[(i-1),(j-1)] <- prod(ifelse(length(which(tree[i, (j+1):99]>=tree[i,j])) ==0, (99-j), min(which(tree[i, (j+1):99]>=tree[i,j]))),  # if visible, (length==0) then
                             ifelse(length(which(tree[i, (j-1):1]>=tree[i,j])) ==0, (j-1), min(which(tree[i, (j-1):1]>=tree[i,j]))),      #choose length until edge else
                             ifelse(length(which(tree[(i+1):99, j]>=tree[i,j])) ==0, (99-i),min(which(tree[(i+1):99, j]>=tree[i,j]))),   # length until tree taller is found
                             ifelse(length(which(tree[(i-1):1, j]>=tree[i,j])) ==0, (i-1), min(which(tree[(i-1):1, j]>=tree[i,j]))))
 
 max(P2)   #ANSWER
 

 
