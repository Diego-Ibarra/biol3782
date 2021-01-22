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
library(dplyr)

yearly_counts <- surveys %>%
  count(year, genus)

# Think lines
ggplot(data = yearly_counts, aes(x = year, y = n, color = genus)) +
  geom_line(size=2) + 
  ggtitle("Time-series plot made by My_Name")


#Facet columns
yearly_sex_counts <- surveys %>%
  count(year, genus, sex)

ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(cols = vars(genus)) + 
  ggtitle("Facet plot made by My_Name")
  
