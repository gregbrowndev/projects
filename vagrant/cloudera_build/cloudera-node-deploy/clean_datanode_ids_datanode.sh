#!/bin/bash
################## install cdh components for datanodes   ####################

service cloudera-scm-agent stop
sleep 2
cd /var/lib/cloudera-scm-agent
rm -rf uuid
rm -rf response.avro

