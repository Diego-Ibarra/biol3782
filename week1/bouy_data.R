# Define buoy's URL
bouyURL <- "http://columbia.loboviz.com/cgi-data/nph-data.cgi?x=date&y=temperature&min_date=20180820&max_date=20180907&node=32&data_format=text"

# Read data directly from buoy
data <- read.csv(bouyURL, sep = "\t", skip = 2)

# Do quick plot
plot.ts(data[2])