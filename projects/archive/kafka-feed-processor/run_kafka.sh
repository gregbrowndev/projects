#!/usr/bin/env bash

sudo /usr/bin/zookeeper-server-start /etc/kafka/zookeeper.properties

sudo /usr/bin/kafka-server-start /etc/kafka/server.properties

sudo /usr/bin/schema-registry-start /etc/schema-registry/schema-registry.properties

# Create a command line producer for the feedrequests topic
kafka-console-producer --broker-list localhost:9092 --topic feedrequests

# Create a command line consumer for the feedmessages topic
kafka-console-consumer --bootstrap-server localhost:9092 --topic feedmessages

# Create FeedRequests topic

kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic feedrequests

# Create RawData topic

kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic rawdata

# Create System, Station, OpeningHours and Calendar topics

kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic system
kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic station
kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic openinghours
kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic calendar

# Create topics for matching up feed objects to model objects

# Create topics for storing the update patches to each model object (this would be a relatively longlived topic so we can diff the model when required)
