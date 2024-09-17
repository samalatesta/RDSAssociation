#' Semi-parametric randomization test for continuous association
#' @description
#' SPRTCA was developed to infer bivariate association between two variables
#' collected using respondent-driven sampling (RDS) when one or both variables are continuous.
#'
#' @param net A RDS data frame.See the RDS package documentation (https://cran.r-project.org/web/packages/RDS/RDS.pdf) for more detail.
#' @param var1 Variable 1, a numeric vector
#' @param var2 Variable 2, a numeric or character vector
#' @param iter Number of iterations for randomization


#### SPRTCA function

#Inputs
#iter number of times to randomize data and calculate a test statistic
#var1 character or numeric variable
#var2 character or numeric variable
#net RDS data frame
#net<- RDSdata
#var1="age"
#var2="sex"

#net=RDSdata
#var1="age"
sprtca <-function(net,var1, var2, iter=5000){
  #tree data to test
  dat <- net
  dat$var1 <- dat[,var1]
  dat$var2 <- dat[,var2]
  if(is.numeric(dat$var1)==T & (is.character(dat$var2) == T | is.factor(dat$var2))){
  mean_var1 <- mean(dat$var1)
  sd_var1 <- sqrt(var(dat$var1))
  dat$var1_std <- (dat$var1 - mean_var1)/sd_var1
  #run t-test on original tree data and save p-value and test statistic
  obs.pval <- t.test(dat$var1_std  ~ dat$var2 )$p.value
  obs.res <- t.test((dat$var1_std ~ dat$var2))$statistic

  #get wave and seed for each node
  dat$wave <- RDS::get.wave(net)
  dat$seed.id <- RDS::get.seed.id(net)

  #initialize empty vector to store results
  permuted.res <- rep(NA, iter)
  dat$rec.id2 <- dat$rec.id
  rec <- dat %>% dplyr::select(id, var1_std)
  rec$rec.id2 = rec$id
  dat2 <- dplyr::left_join(dat, rec, by = "rec.id2") %>% dplyr::filter(is.na(var1_std.y)==F)
  r = cor(dat2$var1_std.x, dat2$var1_std.y)

  #create new df that will store permuted results
  permute.table <- dat
  #initialize permuted variable
  permute.table$permute.var1 <- permute.table$var1_std
  #save values for seeds and set the rest to missing
  permute.table$permute.var1[permute.table$rec.id != 0] <- NA
  #create new df that is ordered by seed and wave so we permute in the correct order
  order.permute.table <- permute.table %>% dplyr::arrange(seed.id, wave)

  for(j in 1:iter){
    for(i in 1:dim(order.permute.table)[1]){
      #generate randomized values in first order markov process
      if(order.permute.table$rec.id[i] != 0){
        rec.id <- order.permute.table$rec.id[i]
        rec.var1 <- order.permute.table$permute.var1[order.permute.table$id == rec.id]
        value <- rnorm(1)
        order.permute.table$permute.var1[i] <-  rec.var1*r + value*sqrt(1-r^2)
      }
    }
    permuted.res[j] <- t.test((order.permute.table$permute.var1 ~ order.permute.table$var2))$statistic

  }


  b <- sum(ifelse(abs(permuted.res) >= abs(obs.res), 1, 0))
  p.value <- b/length(permuted.res)
  p.value.mc <- (b+1)/(length(permuted.res)+1)

  print(paste0("SPRTCA p-value = ", p.value.mc))
  }

  if(is.numeric(dat$var1)==T & is.numeric(dat$var2) == T){

    mean_var1 <- mean(dat$var1)
    sd_var1 <- sqrt(var(dat$var1))
    dat$var1_std <- (dat$var1 - mean_var1)/sd_var1
    mean_var2 <- mean(dat$var2)
    sd_var2 <- sqrt(var(dat$var2))
    dat$var2_std <- (dat$var2 - mean_var2)/sd_var2
    #run t-test on original tree data and save p-value and test statistic
    obs.pval <- cor.test(dat$var1_std, dat$var2_std)$p.value
    obs.res <- cor.test(dat$var1_std, dat$var2_std)$statistic
    mean_numeric.var <- mean(dat$var1)
    sd_numeric.var <- sqrt(var(dat$var1))
    #get wave and seed for each node
    dat$wave <- get.wave(net)
    dat$seed.id <- get.seed.id(net)

    #initialize empty vector to store results
    permuted.res <- rep(NA, iter)
    dat$rec.id2 <- dat$rec.id
    rec <- dat %>% dplyr::select(id, var1_std)
    rec$rec.id2 = rec$id
    dat2 <- dplyr::left_join(dat, rec, by = "rec.id2") %>% dplyr::filter(is.na(var1_std.y)==F)
    r = cor(dat2$var1_std.x, dat2$var1_std.y)

    #create new df that will store permuted results
    permute.table <- dat
    #initialize permuted variable
    permute.table$permute.var1 <- permute.table$var1_std
    #save values for seeds and set the rest to missing
    permute.table$permute.var1[permute.table$rec.id != 0] <- NA
    #create new df that is ordered by seed and wave so we permute in the correct order
    order.permute.table <- permute.table %>% dplyr::arrange(seed.id, wave)

    for(j in 1:iter){
      for(i in 1:dim(order.permute.table)[1]){
        #generate randomized values in first order markov process
        if(order.permute.table$rec.id[i] != 0){
          rec.id <- order.permute.table$rec.id[i]
          rec.var1 <- order.permute.table$permute.var1[order.permute.table$id == rec.id]
          value <- rnorm(1)
          order.permute.table$permute.var1[i] <-  rec.var1*r + value*sqrt(1-r^2)
        }
      }
      permuted.res[j] <- cor.test(order.permute.table$permute.var1, order.permute.table$var2)$statistic

    }


    b <- sum(ifelse(abs(permuted.res) >= abs(obs.res), 1, 0))
    p.value <- b/length(permuted.res)
    p.value.mc <- (b+1)/(length(permuted.res)+1)
    print(paste0("SPRTCA p-value = ", p.value.mc))
  }
}
