# Scatter plot
DM_surveys <- subset(surveys, species_id == "DM")

plot(DM_surveys$weight,DM_surveys$hindfoot_length,
     col="red",
     xlab="Weight (gr)",
     ylab="Foot length (mm)")