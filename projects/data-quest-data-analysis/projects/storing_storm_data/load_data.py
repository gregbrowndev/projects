import csv
from datetime import datetime, timezone
import logging
import time

import psycopg2


logger = logging.getLogger(__name__)


def main():
    conn = psycopg2.connect(
        host='localhost',
        port=5432,
        user='postgres',
        password='mysecret',
        dbname='dataquest'
    )

    conn.autocommit = True
    cur = conn.cursor()

    with open('data/storm_data.csv', 'r') as fin:
        reader = csv.reader(fin)
        header = next(reader)

        for row in reader:
            # Extract data
            fid = int(row[0])
            ts = datetime(
                year=int(row[1]),
                month=int(row[2]),
                day=int(row[3]),
                hour=int(row[4][:2]),
                minute=int(row[4][2:4]),
                tzinfo=timezone.utc
            )

            btid = int(row[5])
            name = row[6]
            lat = float(row[7])
            long = float(row[8])
            wind_kts = int(row[9])
            pressure = int(row[10])
            cat = row[11],
            basin = row[12]
            shape_length = float(row[13])

            # Insert
            # Note - this is very slow! See my notes for inserting using mogrify in data_quest_data_engineering_path.md
            sql = """
                INSERT INTO storm
                    (fid, ts, btid, name, lat, long, wind_kts, pressure, category, basin, shape_length)
                VALUES 
                    (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                """
            data = (fid, ts, btid, name, lat, long, wind_kts, pressure, cat, basin, shape_length)
            cur.execute(sql, data)


if __name__=='__main__':
    logger.info('Start load Storm data task')
    start = time.time()
    main()
    logger.info(f'Finished in f{time.time() - start}s')
