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
OLOT_surveys <- subset(surveys, species_id == "OL" |
                       species_id == "OT")

ggplot(data = OLOT_surveys, mapping = aes(x = species_id, y = weight)) +
  geom_violin(trim=FALSE, fill='blue', color="darkblue",alpha=0.4) +
  geom_boxplot(width=0.1,notch=TRUE,
               # custom outliers
               outlier.colour="red",
               outlier.fill="darkred",
               alpha=0.2,
               outlier.size=3,
               ) +
  ylab("weight (gr)") +
  ggtitle("Violin/Scatterplot made by My_Name")

# time-series with thick lines
ggplot(data = yearly_counts, aes(x = year, y = n, color = genus)) +
  geom_line()