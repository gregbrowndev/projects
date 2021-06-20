# Chicago Transit Authority (CTA) Transit Status Dashboard

## Overview


Kafka Topics:

* org.chicago.cta.station.arrival.{station_name}
  - Producer: Kafka Producer (simulation)
  - Contains events describing the arrival of a train to the station given by `station_name`
* org.chicago.cta.turnstile
  - Producer: Kafka Producer (simulation)
* org.chicago.cta.weather
  - Producer: Kafka Producer (simulation)
* org.chicago.cta.connect.stations
  - Producer: Kafka Connect reading from Postgres stations table
* org.chicago.cta.stations.table.v1
  - Producer: Faust application
  
### Kafka Connect - Faust

A `JdbcSourceConnector` is configured with Kafka Connect to read station data from PostgreSQL
and produce the data to the `org.chicago.cta.connect.stations` topic. From there, a Faust 
application consumes and transforms the station data and produces it to the
`org.chicago.cta.connect.stations.table.v1` topic.


## Design Decisions

### Station Producer / Topic

* A Kafka Topic is created by the Station producer for each station. An initial `num_partitions` 
  of 5 was set to provide some parallism. However, since there is only a single broker in the 
  current environment, `num_replicas` is set to 1. In a production environment, this could be
  increased to add redundancy.
  
* I've chosen to write the `train_status` as a string, e.g. "out_of_service", to the station topic
  rather than the IntEnum value, i.e. 0, to make the data independent of the Python code and to make
  it easier to read and handle changes  at a later date. However, using the integer value 
  would yield smaller storage so it could be something to consider for an improvement.
 

### Kafka Connect - Faust

Faust is used to transform data in Kafka produced from the stations PostgreSQL table. However, some
of the station rows do not contain the necessary 'red', 'green', or 'blue' line information. It appears
there are additional rows in the DB table for 'brown' and 'purple' lines. These rows are ignored by the 
Faust application and thus will not be produced to `org.chicago.cta.connect.stations.table.v1`.
  
  
## Notes

### Producers

All producers inherit the base Producer class found in producer.py. The Producer class sets up 
the Avro Producer and Kafka topic.

The Station producer composes a Turnstile producer, i.e. a station has a turnstile. You can see
the Turnstile is cleaned up in the Station close method. 

All producers, apart from Kafka Connect (PostreSQL), produce Avro data. The Avro schemas are found 
in the schemas directory.


## Consumers

Fuast is used to ingest and transform data produced from PostgreSQL via Kafka Connect.

KSQL is used to combine station and turnstile data produced to a Kafka topic.


## Helpful Commands

### Kafka

List Kafka topics: 

```
kafka-topics --zookeeper localhost:2181 --list
```

Drop a topic:

```
kafka-topics --zookeeper localhost:2181 --delete --topic TURNSTILE_SUMMARY
```

Connect console consumer:

```
kafka-console-consumer --bootstrap-server localhost:9092 --topic org.chicago.cta.connect.stations --from-beginning
```

### KSQL

Enter KSQL CLI:

```
ksql
```

Show topics:

```
SHOW TOPICS;
```