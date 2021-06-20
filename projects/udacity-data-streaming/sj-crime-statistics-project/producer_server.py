import json
import time
import logging

from kafka import KafkaProducer


class ProducerServer(KafkaProducer):

    def __init__(self, input_file, topic, **kwargs):
        super().__init__(**kwargs)
        self.input_file = input_file
        self.topic = topic

    def generate_data(self):
        with open(self.input_file) as f:
            print(f"Parsing file: {self.input_file!r}")
            items = json.load(f)
            for item in items:
                print(f"Sending item: {item!r}")
                message = self.dict_to_binary(item)
                self.send(self.topic, message)
                time.sleep(1)

    def dict_to_binary(self, json_dict):
        return json.dumps(json_dict).encode('utf-8')
        