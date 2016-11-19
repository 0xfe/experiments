#!/usr/bin/env python
#
# Tesla Model S energy cost calculator. Also provides a
# cost comparison vs. gas prices (in local area).
#
# Author: mohit@muthanna.com (0xfe@github)

# Usage:
#
# $ mongod --config /usr/local/etc/mongod.conf &
# $ ~/w/stats/tesla-restore.py (optional, to restore mongodb backup from S3)
# $ ./tesla.py
#
# Exploring the data:
#
# $ ipython --pylab
#
# In [1]: %run ./tesla.py
#
# In [2]: frames['climateState'].outside_temp.dropna().resample('D',how='max').plot(kind='bar')
#
# Make sure MongoDB is running, and that you've restored
# a recent TeslaMS backup. See: tesla-restore.py.

import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import time
import pymongo as pm

matplotlib.style.use('ggplot')

# Load gas prices: http://www.energy.gov.on.ca/en/fuel-prices/
# gas = pd.read_csv("ONTPRM2015.csv", header=1, index_col=0)

# Connect to local database
client = pm.MongoClient()

# Database: tesla, Collection: tesla_stream
db = client.tesla
coll = db.tesla_stream
aux = db.tesla_aux

forever = (datetime.now() - timedelta(hours=(24 * 31000)))
all_month = (datetime.now() - timedelta(hours=(24 * 31)))
all_week = (datetime.now() - timedelta(hours=(24 * 7)))

filter_ms = time.mktime(forever.timetuple()) * 1000

# Get all rows
# c = coll.find()
# Get last week's rows
c = coll.find({"ts": {"$gt": filter_ms}})
ca = aux.find({"ts": {"$gt": filter_ms}})

print "Loading data..."
df = pd.DataFrame([r['record'] for r in list(c)],
                  columns=['ts', 'speed', 'odometer', 'soc', 'elevation',
                           'est_heading', 'est_lat', 'est_lng', 'power',
                           'shift_state', 'range', 'est_range', 'heading'])
print "Loaded."

print "Loading aux..."

arrays = {
  'vehicles': [],
  'chargeState': [],
  'vehicleState': [],
  'climateState': []
}

for r in ca:
  for t in arrays.keys():
    if r.has_key(t):
      # Append record to the relevant array. We
      # don't create a dataframe and append incrementally
      # because it's super slow: DataFrame appends copies
      # and recreates new frames.
      r[t]['ts'] = r['ts']
      arrays[t].append(r[t])
      break

frames = {
  'stream': df
}

# Lump create DataFrame from each array.
for k, array in arrays.iteritems():
  frames[k] = pd.DataFrame(array)

print "Loaded."

for k in frames.keys():
  # Convert all strings to numeric
  print "Processing: " + k + " size " + str(frames[k].size) + "..."
  if frames[k].size < 1:
    print "Skipping empty frame."
    continue

  frames[k] = frames[k].apply(pd.to_numeric, args=('coerce',))

  # Convert 'ts' to datetime
  frames[k].ts = pd.to_datetime(frames[k].ts / 1000, unit='s')

  # Index by timestamp, and set timezone to local
  frames[k] = frames[k].set_index('ts').tz_localize('UTC').tz_convert('US/Eastern')

  print frames[k].info()

# Cost of residential power:
# From: https://www.kwhydro.on.ca/en/residential/residential-electricity-rates.asp
KW_PEAK = 17.5     # weekdays 7am to 11am and 5pm to 7pm
KW_OFF_PEAK = 12.8 # weekends and holidays full day, and weekdays 7pm to 7am
KW_MID_PEAK = 8.3  # weekdays 11am to 5pm
# electricity + delivery network + delivery connection + distribution + regulatory + debt
KW_EXTRAS = 0.72 + 0.7 + 0.15 + 0.15 + 0.5 + 0.7
KW_MONTHLY = 11.64

charged_kw = (frames['chargeState'].charger_actual_current * frames['chargeState'].charger_voltage) / 1000
charged_kwh = charged_kw.resample('min').fillna(0).resample('H') # Resample to hourly, defaults to mean.

# Merge weekend and offpeak hours
weekend_watts = charged_kwh[(charged_kwh.index.dayofweek == 5) | (charged_kwh.index.dayofweek == 6)]
weekday_watts = charged_kwh[(charged_kwh.index.dayofweek >= 0) & (charged_kwh.index.dayofweek < 5)]

offpeak_watts = pd.concat([weekend_watts, charged_kwh.between_time('19:00', '6:59')]).drop_duplicates()
peak_watts = pd.concat([weekday_watts.between_time('7:00', '10:59'), weekday_watts.between_time('17:00', '18:59')]).drop_duplicates()
midpeak_watts = weekday_watts.between_time('11:00',' 16:59')

offpeak_cost = offpeak_watts * ((KW_OFF_PEAK + KW_EXTRAS) / 100)
peak_cost = peak_watts * ((KW_PEAK + KW_EXTRAS) / 100)
midpeak_cost = midpeak_watts * ((KW_MID_PEAK + KW_EXTRAS) / 100)

daily_cost = pd.DataFrame({
  "kwh": charged_kwh.resample("D", how="sum"),
  # alternate miles: frames['stream'].odometer.diff().resample("D", how="sum")
  "miles": frames['stream']['odometer'].resample('D', how=lambda a: max(a) - min(a)),
  "offpeak": offpeak_cost.resample('D', how="sum"),
  "peak": peak_cost.resample('D', how="sum"),
  "midpeak": midpeak_cost.resample('D', how="sum"),
  }).fillna(0)

daily_cost['km'] = daily_cost.miles * 1.609344
daily_cost['total'] = daily_cost.offpeak + daily_cost.peak + daily_cost.midpeak
daily_cost = daily_cost.reindex(columns=["km", "miles", "kwh", "offpeak", "midpeak", "peak", "total"])

# Weekly summarized on Sunday (Mon - Sun). To summarize on, say, Wednesday, use "W-WED"
weekly_cost = daily_cost.resample('W', how="sum", kind="period")

# No need for fixed costs, because you'd be paying it anyway:
# weekly_cost['fixed'] = KW_MONTHLY / 4.5 # prorated fixed monthly cost
# weekly_cost['final'] = weekly_cost.total + weekly_cost.fixed

# nissan fuel economy: 8km/L, avg. premium gas cost: $1.24/L
nissan_cost = (weekly_cost.km.sum() / 8.0) * 1.24

print weekly_cost
print
print "Tesla: $" + str(weekly_cost.total.sum()) + ", Nissan: $" + str(nissan_cost)
print "Ratio: " + str((weekly_cost.total.sum() / nissan_cost) * 100) + "%"

df = frames['stream']