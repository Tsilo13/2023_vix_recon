from datetime import datetime
from concurrent import futures
import pandas as pd
from pandas import DataFrame
import pandas_datareader.data as web

def download_stock(stock):
    """ Try to query Yahoo Finance for a stock, if failed note with print """
    try:
        print(f"Downloading: {stock}")
        stock_df = web.DataReader(stock, 'yahoo', start_time, now_time)
        stock_df['Name'] = stock
        # Sanitize output name (remove ^ from VIX)
        output_name = stock.replace('^', '') + '_data.csv'
        stock_df.to_csv(output_name)
    except Exception as e:
        bad_names.append(stock)
        print(f"Bad: {stock} — {e}")

if __name__ == '__main__':

    """ Set the download window """
    start_time = datetime(2024, 3, 1)
    now_time = datetime(2024, 3, 31)

    """ List of tickers (just VIX) """
    vix_data = ['^VIX']

    bad_names = []  # To keep track of failed queries

    """ Download in parallel """
    max_workers = 5  # Only 1 ticker, no need for 50 threads
    workers = min(max_workers, len(vix_data))

    with futures.ThreadPoolExecutor(workers) as executor:
        res = executor.map(download_stock, vix_data)

    """ Save failed queries """
    if len(bad_names) > 0:
        with open('failed_queries.txt', 'w') as outfile:
            for name in bad_names:
                outfile.write(name + '\n')

    """ Timing """
    finish_time = datetime.now()
    duration = finish_time - now_time
    minutes, seconds = divmod(duration.seconds, 60)
    print('VIX Downloader')
    print(f'The threaded script took {minutes} minutes and {seconds} seconds to run.')
