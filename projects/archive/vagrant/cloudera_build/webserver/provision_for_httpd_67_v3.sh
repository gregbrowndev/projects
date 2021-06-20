#!/bin/bash

CM_REPO_SERVER=$1
WEB_SERVER_ALIAS_DIR=$2
WEB_SERVER_DIR=$3

export WEB_SERVER_TEMPLATE="http-addRepoDir-centos6.template"

yum install -y httpd
chkconfig --levels 235 httpd on
#systemctl enable httpd --levels 235

## ########################################################################

# view configuration files for httpd package
rpm -qc httpd

# Should be running on ports - 
# - 80
# netstat -nltp
# netstat -nlup
## netstat -nltp | grep ":80"
ss -nltp | grep ":80"

## ########################################################################

HTTP_TEMPLATE_DIR="/vagrant/webserver/template"
cd ${HTTP_TEMPLATE_DIR}

# Add dummy web contents
yum localinstall -y figlet-2.2.2-1.el6.rf.x86_64.rpm
# figlet "${CM_REPO_SERVER} running" > /var/www/html/index.html
echo "${CM_REPO_SERVER} running" > /var/www/html/index.html
 
## ########################################################################

echo "WEB SERVER HOST NAME : http://${CM_REPO_SERVER}:80"
echo "WEB SERVER DIR : http://${CM_REPO_SERVER}:80/${WEB_SERVER_ALIAS_DIR}/"

pwd
\cp -rf ${WEB_SERVER_TEMPLATE} http-addRepoDir.txt

sed -i.bak s,REP_SERVERNAME_REP,${CM_REPO_SERVER},g http-addRepoDir.txt
sed -i.bak s,REP_WEB_SERVER_ALIAS_DIR_REP,${WEB_SERVER_ALIAS_DIR},g http-addRepoDir.txt
sed -i.bak s,REP_WEB_SERVER_DIR_REP,${WEB_SERVER_DIR},g http-addRepoDir.txt

cd /etc/httpd/conf

cat ${HTTP_TEMPLATE_DIR}/http-addRepoDir.txt >> httpd.conf

service httpd restart
service httpd status

curl http://${CM_REPO_SERVER}
curl http://${CM_REPO_SERVER}:80/${WEB_SERVER_ALIAS_DIR}/
