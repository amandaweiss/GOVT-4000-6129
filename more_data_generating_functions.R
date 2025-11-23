selection_dataset <- function(){
  
  # These three covariates affect selection into treatment; X2 and X3 also 
  # affect the outcome directly.
  X1 <- factor(sort(sample(1:5, 1000, replace = TRUE)))
  X2 <- 1:1000 + rnorm(1000, 20, sd = 10)
  X3 <- rf(1000, 10, 30)
  
  D <- vector()
  for (i in 1:1000){
    if (X1[i] == 1)
      if (X2[i] > quantile(X2, 0.1) & X3[i] < quantile(X3, 0.9)){
        D[i] <- rbinom(1, 1, 0.8)
      }else{
        D[i] <- rbinom(1, 1, 0.2)
      }
    if (X1[i] == 2)
      if (X2[i] > quantile(X2, 0.3) & X3[i] < quantile(X3, 0.85)){
        D[i] <- rbinom(1, 1, 0.8)
      }else{
        D[i] <- rbinom(1, 1, 0.2)
      }
    if (X1[i] == 3)
      if (X2[i] > quantile(X2, 0.5) & X3[i] < quantile(X3, 0.8)){
        D[i] <- rbinom(1, 1, 0.8)
      }else{
        D[i] <- rbinom(1, 1, 0.2)
      }
    if (X1[i] == 4)
      if (X2[i] > quantile(X2, 0.7) & X3[i] < quantile(X3, 0.75)){
        D[i] <- rbinom(1, 1, 0.8)
      }else{
        D[i] <- rbinom(1, 1, 0.2)
      }
    if (X1[i] == 5)
      if (X2[i] > quantile(X2, 0.9) & X3[i] < quantile(X3, 0.7)){
        D[i] <- rbinom(1, 1, 0.8)
      }else{
        D[i] <- rbinom(1, 1, 0.2)
      }
  }
  
  
  # These two covariates affect the outcome but do not guide selection into
  # treatment
  X4 <- rbinom(n = 1000, size = 1, prob = 0.3)
  X5 <- rnorm(n = 1000, sd = 5)
  
  e <- rnorm(1000)
  
  Y <- 0.5 * D + X2 + X3 + X4 + X5 + e
  
  cat("This data frame has the following columns:\n  * Y for outcome\n  * D for treatment\n  * X1, X2, X3, X4, and X5 for covariates\n")
  
  return(data.frame(Y = Y, 
                    D = D,
                    X1 = X1,
                    X2 = X2,
                    X3 = X3,
                    X4 = X4,
                    X5 = X5))
}



synth_dataset <- function(){
  
  periods <- 10
  
  effect_size <- 0.3
  
  # INITIALIZING VECTORS
  
  i <- 1:25
  t <- 1:periods
  
  # UNIT/TIME FACTORS 
  
  # Year fixed effects normally distributed
  time <- data.frame(
    t = 1:periods,
    time_fe = rnorm(periods)
  )
  
  # Unit fixed effects are literally just index values 1:25 times 0.1. This is
  # quite arbitrary.
  unit <- data.frame(
    i = 1:25,
    unit_fe = 1:25   * 0.1
  )
  
  # COVARIATE DATA 
  innovations1 <- rnorm(15, 0, 0.1)
  innovations2 <- rnorm(10, 5, 0.1)
  X1 <- data.frame(
    i = 1:25,
    X1 = c(arima.sim(list(ar = 0.7), 15, innov = innovations1),
           arima.sim(list(ar = 0.7), 10, innov = innovations2))
  )
  
  X2 <- data.frame(
    i = 1:25,
    X2 = c(rexp(1), rexp(5, 2), rexp(19, 1))
  )
  
  X3 <- data.frame(
    i = 1:25,
    X3 = rbinom(25, 1, 0.5)
  )
  
  X4 <- data.frame(
    i = 1:25,
    X4 = c(rep(3, 13), rep(4, 12))
  )
  
  X5 <- data.frame(
    i = 1:25,
    X5 = rnorm(25)
  )
  
  # TREATMENT ASSIGNMENT 
  
  g <- c(10, rep(0, 24))
  
  df <- merge(data.frame(i = i, g = g), data.frame(t = t), by = NULL)
  
  df$d <- 0
  
  df$d <- ifelse(test = (df$g != 0 & df$t >= df$g), yes = 1, no = df$d)
  
  # ADDING IN THE FACTORS 
  
  df <- merge(df, unit, by = "i")
  df <- merge(df, time, by = "t")
  
  # ADDING IN COVARIATES
  df <- merge(df, X1, by = "i")
  df <- merge(df, X2, by = "i")
  df <- merge(df, X3, by = "i")
  df <- merge(df, X4, by = "i")
  df <- merge(df, X5, by = "i")
  
  
  # OUTCOMES 
  
  df$e <- rnorm(nrow(df), sd = 0.1)
  
  df$b <- effect_size
  
  df$y <- df$d   * df$b + df$unit_fe + df$time_fe + df$X1 + df$X2 + df$X3 + df$X4 + df$X5 + df$e
  
  cat("This data frame has the following columns:\n  * `d` for treatment status for the observation\n  * `y` for outcome\n  * `X1`, `X2`, `X3`, `X4`, and `X5` for unit-specific covariates\n  * `i` for unit index\n  * `t` for time index\n")
  
  return(df[ , c("i", "t", "d", "y", "X1", "X2", "X3", "X4", "X5")])
  
}

panel_dataset2 <- function(){
  
  periods <- 10
  
  effect_size <- 0.3
  
  # INITIALIZING VECTORS
  
  i <- 1:200
  t <- 1:periods
  
  # UNIT/TIME FACTORS 
  
  # Year fixed effects normally distributed
  time <- data.frame(
    t = 1:periods,
    time_fe = rnorm(periods)
  )
  
  # Unit fixed effects are literally just index values 1:50 times 0.1. This is
  # quite arbitrary.
  unit <- data.frame(
    i = 1:200,
    unit_fe = 1:200   * 0.1
  )
  
  X1 <- data.frame(
    i = 1:200,
    X1 = rnorm(200))
  
  # TREATMENT ASSIGNMENT 
  
  # The unit fixed effects increase with the index of the unit;
  # this line of code assigns the units with the 25 largest fixed effects to
  # treatment, but randomizes when exactly those treated units will get treated.
  if (periods == 2){
    g <- c(rep(0, 100), rep(2, 100))
  }else{
    g <- c(rep(0, 100), replicate(100, sample(2:periods, 1)))
  }
  
  df <- merge(data.frame(i = i, g = g), data.frame(t = t), by = NULL)
  
  df$d <- 0
  
  df$d <- ifelse(test = (df$g != 0 & df$t >= df$g), yes = 1, no = df$d)
  
  # ADDING IN THE FACTORS 
  
  df <- merge(df, unit, by = "i")
  df <- merge(df, time, by = "t")
  
  # ADDING IN A COVARIATE
  df <- merge(df, X1, by = "i")
  
  # OUTCOMES 
  
  df$e <- rnorm(nrow(df), sd = 0.1)
  
  df$b <- effect_size
  
  df$y <- df$d   * df$b + df$unit_fe + df$time_fe + df$X1 + df$e
  
  cat("This data frame has the following columns:\n  * `d` for treatment status for the observation\n  * `g` for period first-treated\n  * `y` for outcome\n  * `X1` for for a covariate\n  * `i` for unit index\n  * `t` for time index")
  return(df[ , c("i", "t", "d", "g", "y", "X1")])
  
}

