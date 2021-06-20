import json
import socket
import sys
import requests

from confluent_kafka import Consumer, Producer, KafkaError, KafkaException


def process(msg):
    global p

    value = json.loads(msg.value().decode())

    request = value.get('request', None)
    if not request:
        sys.stderr.write('%% Request missing from feedrequest message\n')
        return

    url = request.get('url', None)
    if not url:
        sys.stderr.write('%% URL missing from request\n')
        return

    headers = request.get('headers', None)
    params = request.get('params', None)
    timeout = request.get('timeout', 10)

    r = requests.get(url, headers=headers, params=params, timeout=timeout)

    if(r.ok):
        p.produce('feedmessages', r.content)
    else:
        sys.stderr.write('Request {url} failed with status code {status_code}\n'
                             .format(url=url, status_code=r.status_code))


if __name__ == '__main__':

    p = Producer({'bootstrap.servers': "localhost:9092",
                  'client.id': socket.gethostname(),
                  'default.topic.config': {'acks': 1}})

    c = Consumer({
        'bootstrap.servers': 'localhost:9092',
        'group.id': 'mygroup',
        'default.topic.config': {
                'auto.offset.reset': 'smallest'
         }
    })

    # Subscribe to topics
    c.subscribe(['feedrequests'])

    # Read messages from Kafka, print to stdout
    try:
        while True:
            msg = c.poll()

            if msg is None:
                continue
            if msg.error():
                # Error or event
                if msg.error().code() == KafkaError._PARTITION_EOF:
                    # End of partition event
                    sys.stderr.write('%% %s [%d] reached end at offset %d\n' %
                                     (msg.topic(), msg.partition(), msg.offset()))
                elif msg.error():
                    # Error
                    raise KafkaException(msg.error())
            else:
                # Proper message
                sys.stderr.write('%% %s [%d] at offset %d with key %s:\n' %
                                 (msg.topic(), msg.partition(), msg.offset(),
                                  str(msg.key())))
                print(msg.value())
                # process(msg)
                p.produce('feedmessages', msg.value())

    except KeyboardInterrupt:
        sys.stderr.write('%% Aborted by user\n')

    # Close down consumer to commit final offsets.
    c.close()