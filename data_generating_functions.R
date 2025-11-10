experimental_dataset <- function(N = 10000){
  
  N1 <- floor(0.5   * N)
  N2 <- N - N1
  
  floor1 <- floor(0.1   * N1)
  floor2 <- floor(0.6   * N1)
  
  df1 <- data.frame(i = 1:N1,
                    Y1 = sample(c(rep(1, floor1), rep(0, N1 - floor1))),
                    Y0 = sample(c(rep(1, floor2), rep(0, N1 - floor2)))
  )
  
  floor3 <- floor(0.3   * N2)
  floor4 <- floor(0.4   * N2)
  
  df2 <- data.frame(i = 1:N2,
                    Y1 = sample(c(rep(1, floor3), rep(0, N2 - floor3))),
                    Y0 = sample(c(rep(1, floor4), rep(0, N2 - floor4)))
  )
  
  df <- rbind(df1, df2)
  
  df$ai <- sort(rnorm(N, mean = 40000, sd = 10000))
  
  
  df$D_random <- sample(c(rep(1, N1), rep(0, N2)))
  df$D_selection <- c(rep(1, N1), rep(0, N2))
  
  df$ITE <- df$Y1 - df$Y0
  
  df$Y_random <- ifelse(df$D_random == 1, 
                        df$Y1,
                        df$Y0)
  
  df$Y_selection <- ifelse(df$D_selection == 1, 
                           df$Y1,
                           df$Y0)
  
  
  df$i = 1:N
  
  cat("This data frame has the following columns:\n  *`i` indicating index\n  * `ai` for a fixed effect-type(you can imagine something like income)\n  * `Y1` for potential outcome under treatment\n  * Y0 for poential oucome under control\n  *`D_random` for treatment assignment under randomization\n  *`D_selection` for treatment assignment under selection (units with smaller fixed effects `ai` are assigned to treatment)\n  *`Y_selection` for realized outcome under selection assignment into treatment\n  *`Y_random` for realized outcome under random assignment into treatment")
  return(df)
  
}


iv_dataset <- function(N = 10000){
  
  Nc <- floor(0.6   * N)
  Nat <- floor(0.2   * N)
  Nnt <- N - Nc - Nat
  
  stratum <- c(rep("C", Nc), rep("AT", Nat), rep("NT", Nnt))
  
  df <- data.frame(Z = sample(c(rep(1, floor(0.5   * N)), 
                                rep(0, N - floor(0.5   * N)) 
  )
  )
  )
  
  df$D <- NA
  for (i in 1:nrow(df)){
    if (stratum[i] == "C"){
      df[i, "D"] <- df[i, "Z"]
    }
    if (stratum[i] == "AT"){
      df[i, "D"] <- 1
    }
    if (stratum[i] == "NT"){
      df[i, "D"] <- 0
    }
  }
  
  df$Y <- df$D   * 1.5 + rnorm(n = N, sd = 0.1) 
  
  cat("This data frame has the following columns:\n  * `Y` is the realized outcome\n  * `Z` is the instrument\n  * `D` is the treatment")
  return(df)
  
}

panel_dataset <- function(){
  
  periods <- 10
  
  effect_size = 0.3
  
  # INITIALIZING VECTORS
  
  i <- 1:50
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
    i = 1:50,
    unit_fe = 1:50   * 0.1
  )
  
  # TREATMENT ASSIGNMENT 
  
  # The unit fixed effects increase with the index of the unit;
  # this line of code assigns the units with the 25 largest fixed effects to
  # treatment, but randomizes when exactly those treated units will get treated.
  if (periods == 2){
    g <- c(rep(0, 25), rep(2, 25))
  }else{
    g <- c(rep(0, 25), replicate(25, sample(2:periods, 1)))
  }
  
  df <- merge(data.frame(i = i, g = g), data.frame(t = t), by = NULL)
  
  df$d <- 0
  
  df$d <- ifelse(test = (df$g != 0 & df$t >= df$g), yes = 1, no = df$d)
  
  # ADDING IN THE FACTORS 
  
  df <- merge(df, unit, by = "i")
  df <- merge(df, time, by = "t")
  
  
  # OUTCOMES 
  
  df$e <- rnorm(nrow(df), sd = 0.1)
  
  df$b <- effect_size
  
  df$y <- df$d   * df$b + df$unit_fe + df$time_fe + df$e
  
  cat("This data frame has the following columns:\n  * `d` for treatment status for the observation\n  * `g` for period first-treated\n  * `y` for outcome\n  * `unit_fe` for unit fixed effects\n  * `time_fe` for time fixed effects\n  * `i` for unit index\n  * `t` for time index")
  return(df[ , c("i", "t", "d", "g", "y", "unit_fe", "time_fe")])
  
}


rd_dataset <- function(){
  
  Y <- sort(rexp(1000)) #+ rnorm(1000, 0.001)
  
  Y[501:1000] <- Y[501:1000] - 1
  
  Y <- jitter(Y, amount = 0.1)
  
  cutoff_side <- c(rep("pre", 500), rep("post", 500))
  
  cat("This data frame has the following columns:\n  * `Y` for the outcome\n  * `X` for the running variable\n  * `c` for the cutoff - just one constant, but stored in a column for convenience")\n  * `cutoff_side` for whether an observation is pre- or post-cutoff")
  return(data.frame(Y = Y,
                    X = 1:1000,
                    c = 501,
                    cutoff_side = cutoff_side))
}
