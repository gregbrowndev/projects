# Import models to 'register' them in MetaData
from db.models.base import Base
from db.models.scraper import Scraper
from db.models.station import Station
from db.models.system import System
from db.utils import create_session

if __name__ == '__main__':
    session = create_session()

    # create scraper object
    # scraper = Scraper(name='bcycle_gbfs')

    # create system object
    system = System(
        source_id='1',
        scraper_id=1,
        name='TfL Bikeshare',
        phone_number='0343 222 6666',
        timezone='Europe/London',
        url='https://tfl.gov.uk/modes/cycling/santander-cycles',
        language='en',
    )

    station = Station(
        source_id='1',
        scraper_id=1,
        name='Station 1',
        latitude=0,
        longitude=0,
        system=system
    )

    try:
        session.add(system)
        session.commit()

        # query again
        obj = session.query(System).first()
        print(obj)
    except Exception as e:
        print('ROLLING BACK ', e)
        session.rollback()
        raise
    finally:
        session.close()
