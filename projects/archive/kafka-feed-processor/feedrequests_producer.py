import json

import sys
from confluent_kafka import Producer
import socket

"""
This is the essentially the main entry point of the data pipeline. This producer can
periodically request the bikeshare Plugin API to get the list of active plugins and datafeeds.

It publishes the datafeed request message to the feedrequests topic, ready to be
consumed by the next part of the pipeline.
"""

if(__name__=="__main__"):

    conf = {'bootstrap.servers': "localhost:9092",
            'client.id': socket.gethostname(),
            'default.topic.config': {'acks': 1}}

    p = Producer(conf)

    # TODO - request bikeshare Plugin API for enabled plugins and publish each datafeed to the feedrequests topic

    # https://gbfs.bcycle.com/bcycle_madison/gbfs.json
    url = "https://gbfs.bcycle.com/bcycle_madison/system_information.json"
    headers = None
    params = None
    timeout = 30

    datafeed_id = 2
    plugin_id = 1

    message = {
        'request': {
            'url': url,
            'headers': headers,
            'params': params,
            'timeout': timeout
        },
        'datafeed_id': datafeed_id,
        'plugin_id': datafeed_id
    }

    # Optional per-message delivery callback (triggered by poll() or flush())
    # when a message has been successfully delivered or permanently
    # failed delivery (after retries).
    def delivery_callback(err, msg):
        if err:
            sys.stderr.write('%% Message failed delivery: %s\n' % err)
        else:
            sys.stderr.write('%% Message delivered to %s [%d]\n' %
                             (msg.topic(), msg.partition()))

    p.produce('feedrequests', value=json.dumps(message), callback=delivery_callback)

    try:
        pass
        #json.dumps(message)
        # Publish this message to the feedrequests topic for processing.

    except BufferError as e:
        sys.stderr.write('%% Local producer queue is full ({messages} messages awaiting delivery): try again\n'.format(messages=len(p)))

    except:
        pass