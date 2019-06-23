#!/bin/bash
################## start agent ####################

CM_OLD_HOST_NAME="cm572-manager"
CM_HOST_NAME=$1

cd /etc/cloudera-scm-agent
sed -i.bak s/server_host=${CM_OLD_HOST_NAME}/server_host=${CM_HOST_NAME}/g config.ini

service cloudera-scm-agent restart
