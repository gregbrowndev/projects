#!/bin/bash

yum install -y cloudera-manager-daemons cloudera-manager-server
yum install -y cloudera-manager-server-db-2

service cloudera-scm-server-db start
service cloudera-scm-server start

chkconfig cloudera-scm-server-db on
chkconfig cloudera-scm-server on
