# Scatter plot
DM_surveys <- subset(surveys, species_id == "DM")

plot(DM_surveys$weight,DM_surveys$hindfoot_length,
     col="red",
     xlab="Weight (gr)",
     ylab="Foot length (mm)")


# ggplot scatter plot ----------------------------
# Scatter plot
DM_surveys <- subset(surveys, species_id == "DM")

ggplot(data = DM_surveys, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.5, color = "red") + 
  geom_smooth()

# violinplot ----------------------------------------
SH_surveys <- subset(surveys, species_id == "SH")

ggplot(data = SH_surveys, mapping = aes(x = species_id, y = weight)) +
  geom_violin(trim=FALSE, fill='red', color="darkred",alpha=0.2) +
  geom_boxplot(width=0.1,
               # custom outliers
               outlier.colour="blue",
               outlier.fill="darkblue",
               outlier.size=3,
               )


ggplot(data = subset_surveys, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3,
    
    width = 0.5
    
  )