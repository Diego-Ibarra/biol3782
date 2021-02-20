run_model <- function(days=365, dt=0.01, mu=0.5, K=1, gamma=0.4, Lambda=0.1, 
                      alpha=0.004, mP=0.07, mZ=0.03, Pinitial=4,
                      Ninitial=10, Zinitial=2) {
  
  # This is an NPZ model loosely based on Fennel et al (2006). See full reference below.
  #
  # Model outout is returned as a data frame. Model output can be plotted with the plot_run() function.
  #
  # Arguments ----------------------------------------------
  #
  # Framework
  # days : number of days of experiment run (units: days)
  # dt   : time-step (units: days)
  #
  # Parameters
  # mu     : growth rate of phytoplankton (units: d^-1)
  # K      : half-saturation for nutrient absoption by phytoplankton (units: mmolN m^-3)
  # gamma  : growth rate of zooplankton (units: d^-1)
  # Lambda : initial slope of ingestion saturation of zooplankton (units: mmol N^-1 m^3)
  # alpha  : initial slope of P vs I curve (units: {mmolN m^-3}^-1)
  # mP     : mortality rate of phytoplankton (units: d^-1)
  # mZ     : mortality rate of zooplankton (units: d^-1)
  # 
  # Initial conditions
  # Pinitial : initial phytoplankton concentration (units: mmolN m^-3)
  # Ninitial : initial nutrients concentration (units: mmolN m^-3)
  # Zinitial : initial zooplankton concentration (units: mmolN m^-3)
  #
  # Reference ----------------------------------------------
  # Fennel , K., Wilkin, J., Levin, J., Moisan, J., O'Reilly, J., Haidvogel, D., Nitrogen cycling
  # in the Mid Atlantic Bight and implications for the North Atlantic nitrogen budget: Results 
  # from a three-dimensional model. Global Biogeochemical Cycles 20, GB3007, doi:10.1029/2005GB002456. (2006)
  
  
  
  
  library(ggplot2)

  # Chores (calculate number of steps, create time vector, create zero vectors, ...)
  NoSTEPS <- as.integer(days / dt) # Calculates the number of steps by dividing days by dt and rounding down
  time <- seq(from = 0, to = days, length = NoSTEPS) # Create time vector of equally spaced numbers from zero to "days"
  P <- numeric(NoSTEPS)      # Make Phytoplankton "empty" vector by making a vector array of zeros (size: NoSTEPS rows by ONE column) (units: mmolN m^-3) 
  N <- numeric(NoSTEPS)      # Make Nutrients "empty" vector (same process as above) (units: mmolN m^-3)
  Z <- numeric(NoSTEPS)      # Make Zooplankton "empty" vector (same process as above) (units: mmolN m^-3)
  L_N <- numeric(NoSTEPS)    # Make "Limitation of N on Phytoplankton" empty vector (same process as above) (units: dimensionless)
  L_P <- numeric(NoSTEPS)    # Make "Limitation of P on Zooplankton" empty vector (same process as above) (units: dimensionless)
  I  <- numeric(NoSTEPS)     # Make Light (I) "empty" vector (same process as above)  (units: umol m^-2 s^-1)
  L_I <- numeric(NoSTEPS)    # Make "Limitation of I on Phytoplankton" empty vector (same process as above)  (units: dimensionless)
  
  
  # Creating sunlight ------------------------------------------------------------
  for (i in 1:length(I)) {
    I[i] <- 600 * sin((2 * pi * time[i])/1) + 500 * sin((2*pi*time[i])/365) # Eq 14
    
    # We can't have negative light. Make negative values equal to zero
    if (I[i] < 0) {
      I[i] <- 0
    }
  }
  
  
  # Initializing with initial conditions -----------------------------------------
  P[1] <- Pinitial # Initializing phytoplankton vector
  N[1] <- Ninitial # Initializing nutrients vector
  Z[1] <- Zinitial # Initializing zooplankton vector
  
  
  # ******************************************************************************
  # MAIN MODEL LOOP **************************************************************
  for (t in 1:(NoSTEPS-1)) {
    L_N[t] <- N[t]/(K+N[t])       # Calculate Limitation due to (low) nutrients on phytoplankton. Eq. 12 
    L_P[t] <- 1-exp(-Lambda*P[t]) # Calculate Limitation due to (low) phytoplankton on zooplankton. Eq. 13
    L_I[t] <- 1-exp(-alpha*I[t])  # Calculate Limitation due to (low) light on phytoplankton Eq. 16
    
    # Estimate model state at time t+1 
    P[t+1] <- P[t] + (((mu*L_N[t]*L_I[t]*P[t]) - (gamma*L_P[t]*Z[t]) - (mP*P[t])) * dt) # Eq. 9
    N[t+1] <- N[t] + (((mP*P[t]) + (mZ*Z[t]) - (mu*L_N[t]*L_I[t]*P[t])) * dt)           # Eq. 10
    Z[t+1] <- Z[t] + (((gamma*L_P[t]*Z[t]) - (mZ*Z[t])) * dt)                           # Eq. 11
  }
  # end of main model LOOP********************************************************
  # ******************************************************************************
  
  
  # For cleanliness, let's pack everything into a data frame
  output <- data.frame(tiime=time,
                       N=N,
                       P=P,
                       Z=Z,
                       L_I=L_I,
                       L_N=L_N,
                       L_P=L_P)  
  return(output)
}

plot_run <- function(output) {
  
  # Uses the output from the NPZ model genertaed with the run_model() function to
  # make two generic plots:
  #
  # (1) Time-series pLot of all state variables, and
  # (2) Time-series pLot of all limiting functions
  
  # Plotting ---------------------------------------------------------------------
  # Plot 1: Main variables
  plot1 <- ggplot(data = output, aes(x=time)) +
    geom_line(aes(y = P, color="P")) +
    geom_line(aes(y = N, color="N")) +
    geom_line(aes(y = Z, color="Z")) +
    labs(x = "time (days)",
         y = expression(Nitrogen~(mmol~N~m^-3))) +
    scale_color_manual("Variables",
                       breaks = c("P", "N", "Z"),
                       labels = c("P", "N", "Z"),
                       values = c("green", "blue", "red"))
  
  # Plot 2: Limitations
  plot2 <- ggplot(data = output, aes(x=time)) +
    geom_line(aes(y = L_I, color="L_I")) +
    geom_line(aes(y = L_N, color="L_N")) +
    geom_line(aes(y = L_P, color="L_P")) +
    ylim(0, 1) + 
    labs(x = "time (days)",
         y = "Limitation (dimensionless)") +
    scale_color_manual("Limitations",
                       breaks = c("L_I", "L_N", "L_P"),
                       labels = c("L_I: Lim of I on Phy", "L_N: Lim of N on Phy", "L_P: Lim of P on Zoo"),
                       values = c("cyan", "red", "blue"))
  
  return(list(plot1, plot2))
}
