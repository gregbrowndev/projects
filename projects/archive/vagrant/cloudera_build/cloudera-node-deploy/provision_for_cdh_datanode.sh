#!/bin/bash
################## install cdh components for datanodes   ####################

CM_HOST_NAME=$1
CM_START_SERVICES=$2

yum install -y cloudera-manager-agent cloudera-manager-daemons
sleep 3

cd /etc/cloudera-scm-agent
cp config.ini config.ini.bak
sed -i.bak s/server_host=localhost/server_host=${CM_HOST_NAME}/g config.ini

echo "## ###################################################"
echo "CM_HOST_NAME : ${CM_HOST_NAME}"
echo "CM_START_SERVICES : ${CM_START_SERVICES}"
echo "## ###################################################"

if [ "${CM_START_SERVICES}" == "start" ]; then
	echo "## ###################################################"
	echo "Starting CM Agent"
	echo "## ###################################################"
	service cloudera-scm-agent restart
fi

chkconfig cloudera-scm-agent on
