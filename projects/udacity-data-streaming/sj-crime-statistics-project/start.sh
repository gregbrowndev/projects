conda install --yes --file requirements.txt

# Start processes
# systemctl start confluent-zookeeper > start.log 2>&1
# systemctl start confluent-kafka > start.log 2>&1

/usr/bin/zookeeper-server-start -daemon config/zookeeper.properties
/usr/bin/kafka-server-start -daemon config/server.properties

# Configure Kafka topic
kafka-topics --delete --zookeeper localhost:2181 --topic org.sfpd.crime.calls > start.log 2>&1
kafka-topics --create --zookeeper localhost:2181 --topic org.sfpd.crime.calls --replication-factor 1 --partitions 10 > start.log 2>&1