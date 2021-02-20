# User settings ----------------------------------------
# Framework
days <- 10    # number of days of experiment run (units: days)
dt   <- 0.01  # time-step  (units: days)

# End of User settings ---------------------------------

# Chores (calculate number of steps, create time vector, create zero vectors, ...)
NoSTEPS <- as.integer(days / dt) # Calculates the number of steps by dividing days by dt and rounding down
time <- seq(from = 0, to = days, length = NoSTEPS) # Create time vector of equally spaced numbers from zero to "days"
P <- numeric(NoSTEPS)      # Make Phytoplankton "empty" vector by making a vector array of zeros (size: NoSTEPS rows by ONE column) (units: mmolN m^-3) 
N <- numeric(NoSTEPS)      # Make Nutrients "empty" vector (same process as above) (units: mmolN m^-3)
Z <- numeric(NoSTEPS)      # Make Zooplankton "empty" vector (same process as above) (units: mmolN m^-3)
L_N <- numeric(NoSTEPS)    # Make "Limitation of N on Phytoplankton" empty vector (same process as above) (units: dimensionless)
L_P <- numeric(NoSTEPS)    # Make "Limitation of P on Zooplankton" empty vector (same process as above) (units: dimensionless)