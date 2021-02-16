library(ggplot2)

# Framework
days <- 365 * 2 # number of days of experiment run (units: days)
dt   <- 0.01    # time-step  (units: days)

# Initial conditions
Pinitial <- 4  # initial phytoplankton concentration (units: mmolN m^-3)
Ninitial <- 10 # initial nutrients concentration (units: mmolN m^-3)
Zinitial <- 2  # initial zooplankton concentration (units: mmolN m^-3)

# Chores (calculate number of steps, create time vector, create zero vectors, ...)
NoSTEPS <- as.integer(days / dt) # Calculates the number of steps by dividing days by dt and rounding down
time <- seq(from = 0, to = days, length = NoSTEPS) # Create time vector of equally spaced numbers from zero to "days"
P <- numeric(NoSTEPS)      # Make Phytoplankton "empty" vector by making a vector array of zeros (size: NoSTEPS rows by ONE column) (units: mmolN m^-3) 
N <- numeric(NoSTEPS)      # Make Nutrients "empty" vector (same process as above) (units: mmolN m^-3)
Z <- numeric(NoSTEPS)      # Make Zooplankton "empty" vector (same process as above) (units: mmolN m^-3)
L_N <- numeric(NoSTEPS)    # Make "Limitation of N on Phytoplankton" empty vector (same process as above) (units: dimensionless)
L_P <- numeric(NoSTEPS)    # Make "Limitation of P on Zooplankton" empty vector (same process as above) (units: dimensionless)

# Initializing with initial conditions -----------------------------------------
P[1] <- Pinitial # Initializing phytoplankton vector
N[1] <- Ninitial # Initializing nutrients vector
Z[1] <- Zinitial # Initializing zooplankton vector

# For cleanliness, let's pack everything into a data frame
output <- data.frame(t=time,
                     N=N,
                     P=P,
                     Z=Z)

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