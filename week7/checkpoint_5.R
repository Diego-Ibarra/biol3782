library(ggplot2)

# User settings ----------------------------------------
# Framework
days <- 365 * 2    # number of days of experiment run (units: days)
dt   <- 0.01  # time-step  (units: days)

# Parameters
mu     <- 0.5   # growth rate of phytoplankton (units: d^-1)
K      <- 1     # half-saturation for nutrient absoption by phytoplankton (units: mmolN m^-3)
gamma  <- 0.4   # growth rate of zooplankton (units: d^-1)
Lambda <- 0.1   # initial slope of ingestion saturation of zooplankton (units: mmol N^-1 m^3)
alpha  <- 0.004 # initial slope of P vs I curve (units: {mmolN m^-3}^-1)
mP     <- 0.07  # mortality rate of phytoplankton (units: d^-1)
mZ     <- 0.03  # mortality rate of zooplankton (units: d^-1)

# Initial conditions
Pinitial <- 4  # initial phytoplankton concentration (units: mmolN m^-3)
Ninitial <- 10 # initial nutrients concentration (units: mmolN m^-3)
Zinitial <- 2  # initial zooplankton concentration (units: mmolN m^-3)

# End of User settings ---------------------------------

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
output <- data.frame(time=time,
                     N=N,
                     P=P,
                     Z=Z)

# Print to screen
print("Model run is done!")  


# Plotting ---------------------------------------------------------------------
# Plot 1: Main variables
ggplot(data = output, aes(x=time)) +
  geom_line(aes(y = P, color="P")) +
  geom_line(aes(y = N, color="N")) +
  geom_line(aes(y = Z, color="Z")) +
  labs(x = "time (days)",
       y = expression(Nitrogen~(mmol~N~m^-3))) +
  scale_color_manual("Variables",
                     breaks = c("P", "N", "Z"),
                     labels = c("P", "N", "Z"),
                     values = c("green", "blue", "red"))