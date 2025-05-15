from dotenv import load_dotenv
import os
from alpha_vantage.timeseries import TimeSeries
import yfinance as yf
import pandas as pd

load_dotenv()

#Alpha Vantage Download
"""api_key = os.getenv('alpha_vantage_api')
ts = TimeSeries(key=api_key, output_format='pandas')

# Example for one ticker
data, meta_data = ts.get_daily(symbol='VIXCLS', outputsize='full')

# Restrict to March 2024
data_march = data.loc["2023-01-01":"2023-12-31"]

# Save to CSV
data_march.to_csv("2023_vix_alpha_vantage.csv")"""


#yfinance download
# Example ticker list
tickers = ["^VIX"]

# Download daily historical stock data
data = yf.download(tickers, start="2023-01-01", end="2023-12-31")

# Save to CSV
data.to_csv("2023_vix_yfinance.csv")
