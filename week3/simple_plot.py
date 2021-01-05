import pandas as pd

# Read data from hard-drive
data = pd.read_csv('phytoplankton_data.csv',sep=',',header=0)

# Do quick plot
data.plot(x='time (UTC)',y='phytoplankton (milimoles/m3)', rot=90)