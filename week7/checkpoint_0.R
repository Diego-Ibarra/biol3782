# Framework
days <- 10    # number of days of experiment run (units: days)
dt   <- 0.01  # time-step  (units: days)

# Chores (calculate number of steps, create time vector, create zero vectors, ...)
NoSTEPS <- as.integer(days / dt) # Calculates the number of steps by dividing days by dt and rounding down
time <- seq(from = 0, to = days, length = NoSTEPS) # Create time vector of equally spaced numbers from zero to "days"