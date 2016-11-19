#!/usr/bin/env python

# Canada housing price index data from:
#
# http://www5.statcan.gc.ca/cansim/pick-choisir?lang=eng&id=03270046&p2=33

import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import time

matplotlib.style.use('ggplot')

FILE="housing-price-index.csv"

print "Loading housing data from: " + FILE
hpi = pd.read_csv(FILE, header=0, index_col=0,
  names=["geo", "class", "part", "vector", "coordinate", "value"])

print "Munging data..."
hpi = hpi.apply(pd.to_numeric, args=('ignore',))
hpi.value = pd.to_numeric(hpi.value, errors="coerce")
hpi.index = pd.to_datetime(hpi.index)

print hpi.geo.value_counts()
print
print "Plotting Kitchener-Cambridge-Waterloo..."
hpi[(hpi.geo=="Kitchener-Cambridge-Waterloo, Ontario") & (hpi.part=="Total (house and land)")].value.plot(label="total")
hpi[(hpi.geo=="Kitchener-Cambridge-Waterloo, Ontario") & (hpi.part=="Land only")].value.plot(label="land")
hpi[(hpi.geo=="Kitchener-Cambridge-Waterloo, Ontario") & (hpi.part=="House only")].value.plot(label="house")

plt.legend(loc="best")
plt.axes().set_title("Kitchener-Cambridge-Waterloo")
plt.show()