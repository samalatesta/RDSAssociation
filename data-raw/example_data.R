## code to prepare `example_data` dataset goes here

#
# This file simulates an RDS data set.
#
#

library(RDS)
library(dplyr)

#generate RDS data with two nodal variables (one continuous and one dichotomous)
#continuous variable is normally distributed, each recruiter can recruit up to 3 recruitees
#function inputs
# number.of.seeds = number of seeds for RDS sample
# sample.size = RDS sample size
# AA =
# BB =
# r =
# ef = effect size/mean difference for continuous variable for levels of dichotomous variable
# function returns RDS data frame

sample.size=500
number.of.seeds=5

#make tree
id <- 1:sample.size
#vector of 0s, length is number of seeds
#rec.id of 0 indicates seed in final data set
rec.id <- rep(0, number.of.seeds)
c <- length(rec.id)
initial <- 1
finish <- number.of.seeds
while(length(rec.id) <= sample.size){
  #sample with replacement, 1:3 recruits per node
  a <- sample(1:3, c, replace = T)
  #for each node assign recruiter
  b <- sample(rep(initial:finish, a))
  #number of new recruits
  c <- length(b)
  #add new rec.ids to rec.id vector
  rec.id <- c(rec.id, b)
  #update initial to be start of new wave
  initial <- max(b)+1
  #update finish to be end of new wave
  finish <- max(b)+c
}
#store rec.id variable
rec.id <- rec.id[1:sample.size]
#assign degree to each node, random sample 1:30
degree <- sample(1:30, sample.size, replace=T)
#final data frame
#edge list and degree
dat <- data.frame(id, rec.id, degree)

#coerce to RDS data frame
dat <-as.rds.data.frame(df=dat,id="id",
                        recruiter.id="rec.id",
                        network.size="degree")

dat$wave <- get.wave(dat)
dat$seed.id <- get.seed.id(dat)
dat <- as.data.frame(dat)
dat$id <- as.numeric(dat$id)
dat$rec.id <- as.numeric(dat$rec.id)
dat$seed.id <- as.numeric(dat$seed.id)

order.table <- data.frame(dat) %>% dplyr::arrange(seed.id, wave)

#make attributes
# 2 categorical
# 2 continuous


#char1
AA=.5
BB=.3

#matrix of recruitment probabilities
#AA and BB are same group probabilities
myprob.char1 <- matrix(c(AA, 1-AA, 1-BB, BB),nrow=2)
order.table$char1 <- rep(NA,sample.size)

for(i in 1:sample.size){
  #generate randomized values in first order markov process
  if(order.table$rec.id[i] == 0){order.table$char1[i] <- sample(1:2,1)}

  else{
    order.table$rec.char1.id[i] <- order.table$rec.id[i]
    order.table$rec.char1[i] <- order.table$char1[order.table$id == order.table$rec.id[i]]
    value <- rmultinom(1, 1, prob = myprob.char1[,order.table$rec.char1[i]])==1
    order.table$char1[i] <- which(value)
  }
}

order.table$sex <- factor(order.table$char1, levels = c(1,2), labels = c("Male", "Female"))



#char2
AA=.8
BB=.7

#matrix of recruitment probabilities
#AA and BB are same group probabilities
myprob.char1 <- matrix(c(AA, 1-AA, 1-BB, BB),nrow=2)
order.table$char1 <- rep(NA,sample.size)

for(i in 1:sample.size){
  #generate randomized values in first order markov process
  if(order.table$rec.id[i] == 0){order.table$char1[i] <- sample(1:2,1)}

  else{
    order.table$rec.char1.id[i] <- order.table$rec.id[i]
    order.table$rec.char1[i] <- order.table$char1[order.table$id == order.table$rec.id[i]]
    value <- rmultinom(1, 1, prob = myprob.char1[,order.table$rec.char1[i]])==1
    order.table$char1[i] <- which(value)
  }
}

order.table$disease <- factor(order.table$char1, levels = c(1,2), labels = c("Disease", "No Disease"))




#num1
r=.3
order.table$num1 <- rep(NA,sample.size)
for(i in 1:dim(order.table)[1]){
  #generate randomized values in first order markov process
  if(order.table$rec.id[i] == 0){order.table$num1[i] <- rnorm(1)}
  else{
    order.table$rec.num1.id[i] <- order.table$rec.id[i]
    order.table$rec.num1[i] <- order.table$num1[order.table$id == order.table$rec.num1.id[i]]
    value <- rnorm(1)
    order.table$num1[i] <-  order.table$rec.num1[i]*r + value*sqrt(1-r^2)
  }
}

#shift mean by EF
order.table$age <- floor(order.table$num1 + 25)

order.table$age <- ifelse(order.table$disease == "Disease", order.table$age +5 , order.table$age)

final <- order.table %>% dplyr::select(id, rec.id, degree, wave, seed.id, sex, disease, age)

######PRODUCE RDS DATA FRAME#####
RDSdata <-as.rds.data.frame(df=final,id="id",
                        recruiter.id="rec.id",
                        network.size="degree",
                        population.size=15000)



usethis::use_data(RDSdata, overwrite = TRUE)
