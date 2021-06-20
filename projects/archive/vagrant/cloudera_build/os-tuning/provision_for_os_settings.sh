#!/bin/bash

sudo yum -y install ntp
# sudo chkconfig ntpd on
# sudo /etc/init.d/ntpd start
sudo systemctl enable ntpd
sudo systemctl start ntpd

# sudo chkconfig iptables off
# sudo /etc/init.d/iptables stop
sudo systemctl disable firewalld
sudo systemctl stop firewalld
sudo setenforce 0

sudo sed -i 's/SELINUX=enforcing/SELINUX=disabled/g' /etc/selinux/config
sudo sh -c 'echo "* soft nofile 10000" >> /etc/security/limits.conf'
sudo sh -c 'echo "* hard nofile 10000" >> /etc/security/limits.conf'
