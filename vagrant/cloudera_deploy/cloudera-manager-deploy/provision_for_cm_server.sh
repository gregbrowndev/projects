#!/bin/bash

service cloudera-scm-server stop
sleep 4
service cloudera-scm-server-db stop
sleep 4

service cloudera-scm-server-db start
sleep 4
service cloudera-scm-server start
sleep 4

chkconfig cloudera-scm-server-db on
chkconfig cloudera-scm-server on
