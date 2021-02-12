library(ggplot2)

# Framework
days <- 365 * 2 # number of days of experiment run (units: days)
dt   <- 0.01    # time-step  (units: days)

# Parameters
mu     <- 0.5   # growth rate of phytoplankton (units: d^-1)
K      <- 1     # half-saturation for nutrient absoption by phytoplankton (units: mmolN m^-3)
gamma  <- 0.4   # growth rate of zooplankton (units: d^-1)
Lambda <- 0.1   # initial slope of ingestion saturation of zooplankton (units: mmol N^-1 m^3)
alpha  <- 0.004 # initial slope of P vs I curve (units: {mmolN m^-3}^-1)
psi    <- 0.01  # half-saturation for phytoplankton absoption by mussels (units: mmolN m^-3)
mP     <- 0.07  # mortality rate of phytoplankton (units: d^-1)
mZ     <- 0.03  # mortality rate of zooplankton (units: d^-1)

# Initial conditions
Pinitial <- 4  # initial phytoplankton concentration (units: mmolN m^-3)
Ninitial <- 10 # initial nutrients concentration (units: mmolN m^-3)
Zinitial <- 2  # initial zooplankton concentration (units: mmolN m^-3)

# Chores (calculate number of steps, create zero vectors, create time vector...)
NoSTEPS <- as.integer(days / dt) # Calculates the number of steps by dividing days by dt and rounding down
time <- seq(from = 0, to = days, length = NoSTEPS) # Create time vector of equally spaced numbers from zero to "days"
N <- numeric(NoSTEPS)      # Make Nutrients "empty" vector by making a vector array of zeros (size: NoSTEPS rows by ONE column)
P <- numeric(NoSTEPS)      # Make Phytoplankton "empty" vector (same process as above)
Z <- numeric(NoSTEPS)      # Make Zooplankton "empty" vector (same process as above)
TotNit <- numeric(NoSTEPS) # Make Total-Nitrogen "empty" vector (same process as above)
L_N <- numeric(NoSTEPS)    # Make "Limitation of N on Phytoplankton" empty vector (same process as above)
L_P <- numeric(NoSTEPS)    # Make "Limitation of P on Zooplankton" empty vector (same process as above)
I  <- numeric(NoSTEPS)     # Make Light (I) "empty" vector (same process as above)
L_I <- numeric(NoSTEPS)    # Make "Limitation of I on Phytoplankton" empty vector (same process as above)

# Creating sunlight ------------------------------------------------------------
for (i in 1:length(I)) {
  I[i] <- 600 * sin((2 * pi * time[i])/1) + 500 * sin((2*pi*time[i])/365)
  
  # We can't have negative light. Make negative values equal to zero
  if (I[i] < 0) {
    I[i] <- 0
  }
}

# Initializing with initial conditions -----------------------------------------
P[1] <- Pinitial # Initializing phytoplankton vector
N[1] <- Ninitial # Initializing nutrients vector
Z[1] <- Zinitial # Initializing zooplankton vector
TotNit[1] <- P[1] + N[1] + Z[1] # Initializing Total-Nitrogen vector

# ******************************************************************************
# MAIN MODEL LOOP **************************************************************
for (t in 1:(NoSTEPS-1)) {
  L_N[t] <- N[t]/(K+N[t])       # Calculate Limitation due to (low) nutrients on phytoplankton
  L_P[t] <- 1-exp(-Lambda*P[t]) # Calculate Limitation due to (low) phytoplankton on zooplankton
  L_I[t] <- 1-exp(-alpha*I[t])  # Calculate Limitation due to (low) light on phytoplankton
  
  # Estimate model state at time t+1 
  P[t+1] <- P[t] + (((mu*L_N[t]*L_I[t]*P[t]) - (gamma*L_P[t]*Z[t]) - (mP*P[t])) * dt)
  N[t+1] <- N[t] + (((mP*P[t]) + (mZ*Z[t]) - (mu*L_N[t]*L_I[t]*P[t])) * dt)
  Z[t+1] <- Z[t] + (((gamma*L_P[t]*Z[t]) - (mZ*Z[t])) * dt)
  TotNit[t+1] = P[t+1] + N[t+1] + Z[t+1] # Calculate total nitrogen
}
# end of main model LOOP********************************************************
# ******************************************************************************

# For cleanliness, let's pack everything into a data frame
output <- data.frame(t=time,
                     N=N,
                     P=P,
                     Z=Z,
                     TotNit=TotNit,
                     L_I=L_I,
                     L_N=L_N,
                     L_P=L_P)

# Plotting ---------------------------------------------------------------------
# Plot 1: Main variables
ggplot(data = output, aes(x=time)) +
  geom_line(aes(y=TotNit, color="TotNit")) +
  geom_line(aes(y = P, color="P")) +
  geom_line(aes(y = N, color="N")) +
  geom_line(aes(y = Z, color="Z")) +
  ylim(0, max(output$TotNit)) + 
  labs(x = "time (days)",
       y = expression(Nitrogen~(mmol~N~m^-3))) +
  scale_color_manual("Variables",
                     breaks = c("TotNit", "P", "N", "Z"),
                     labels = c("TotNit", "P", "N", "Z"),
                     values = c("yellow", "green", "blue", "red"))

# Plot 2: Limitations
ggplot(data = output, aes(x=time)) +
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

