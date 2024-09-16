#' Semi-parametric randomization test for continuous association
#' @description
#' SPRTCA was developed to infer bivariate association between two variables
#' collected using respondent-driven sampling (RDS) when one or both variables are continuous.
#'
#' @param net A  RDS data frame.
#' @param character.var Variable 1
#' @param numeric.var Variable 2
#' @param iter Number of iterations for randomization


#### SPRTCA function

#Inputs
#iter number of times to randomize data and calculate a test statistic
#var1 character or numeric variable
#var2 character or numeric variable
#net RDS data frame

sprtca <-function(iter, character.var, numeric.var, net){
  #tree data to test
  dat <- net
  dat$numeric.var <- dat[,numeric.var]
  dat$character.var <- dat[,character.var]
  mean_numeric.var <- mean(dat$numeric.var)
  sd_numeric.var <- sqrt(var(dat$numeric.var))
  dat$numeric.var_std <- (dat$numeric.var - mean_numeric.var)/sd_numeric.var
  #run t-test on original tree data and save p-value and test statistic
  obs.pval <- t.test(dat$numeric.var_std  ~ dat$character.var )$p.value
  obs.res <- t.test((dat$numeric.var_std ~ dat$character.var))$statistic

  #get wave and seed for each node
  dat$wave <- RDS::get.wave(net)
  dat$seed.id <- RDS::get.seed.id(net)

  #initialize empty vector to store results
  permuted.res <- rep(NA, iter)
  dat$rec.id2 <- dat$rec.id
  rec <- dat %>% dplyr::select(id, numeric.var_std)
  rec$rec.id2 = rec$id
  dat2 <- dplyr::left_join(dat, rec, by = "rec.id2") %>% dplyr::filter(is.na(numeric.var_std.y)==F)
  r = cor(dat2$numeric.var_std.x, dat2$numeric.var_std.y)

  #create new df that will store permuted results
  permute.table <- dat
  #initialize permuted variable
  permute.table$permute.numeric.var <- permute.table$numeric.var_std
  #save values for seeds and set the rest to missing
  permute.table$permute.numeric.var[permute.table$rec.id != 0] <- NA
  #create new df that is ordered by seed and wave so we permute in the correct order
  order.permute.table <- permute.table %>% dplyr::arrange(seed.id, wave)

  for(j in 1:iter){
    for(i in 1:dim(order.permute.table)[1]){
      #generate randomized values in first order markov process
      if(order.permute.table$rec.id[i] != 0){
        rec.id <- order.permute.table$rec.id[i]
        rec.numeric.var <- order.permute.table$permute.numeric.var[order.permute.table$id == rec.id]
        value <- rnorm(1)
        order.permute.table$permute.numeric.var[i] <-  rec.numeric.var*r + value*sqrt(1-r^2)
      }
    }
    permuted.res[j] <- t.test((order.permute.table$permute.numeric.var ~ order.permute.table$character.var))$statistic

  }


  b <- sum(ifelse(abs(permuted.res) >= abs(obs.res), 1, 0))
  p.value <- b/length(permuted.res)
  p.value.mc <- (b+1)/(length(permuted.res)+1)

  print(paste0("SPRTCA p-value = ", p.value.mc))
  # print("Homophily for variable 1 = ")
  #print("Homophily for variable 2 = ")
  #return(list(obs.pval, obs.res,p.value.mc, permuted.res,r, mean_numeric.var, sd_numeric.var))
}
