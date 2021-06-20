import producer_server


KAFKA_TOPIC = "org.sfpd.crime.calls"
BOOTSTRAP_SERVER = "localhost:9092"


def run_kafka_server():
    input_file = "police-department-calls-for-service.json"
    producer = producer_server.ProducerServer(
        input_file=input_file,
        topic=KAFKA_TOPIC,
        bootstrap_servers=[BOOTSTRAP_SERVER],
        client_id="1"
    )

    return producer


def feed():
    producer = run_kafka_server()
    producer.generate_data()


if __name__ == "__main__":
    feed()

    # DEBUG connect Kafka console consumer
    # kafka-console-consumer --bootstrap-server localhost:9092 --topic org.sfpd.crime.calls --from-beginning