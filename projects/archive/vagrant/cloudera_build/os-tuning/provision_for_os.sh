#!/bin/bash
################## Update O/S ports / TCP buffers / swappiness ####################

cd /etc
\cp -rf sysctl.conf sysctl.conf.bak

echo '' >> /etc/sysctl.conf
echo '# Ephemeral ports' >> /etc/sysctl.conf
echo 'net.ipv4.ip_local_port_range = 41000 65000' >> /etc/sysctl.conf
echo '' >> /etc/sysctl.conf
echo '# TCP buffers' >> /etc/sysctl.conf
echo 'net.core.rmem_max = 16777216' >> /etc/sysctl.conf
echo 'net.core.wmem_max = 16777216' >> /etc/sysctl.conf
echo 'net.ipv4.tcp_rmem = 4095 87380 16777216' >> /etc/sysctl.conf
echo 'net.ipv4.tcp_wmem = 4095 65536 16777216' >> /etc/sysctl.conf
echo '' >> /etc/sysctl.conf
echo '# Decrease swappiness' >> /etc/sysctl.conf
echo 'vm.swappiness = 5' >> /etc/sysctl.conf
echo '' >> /etc/sysctl.conf

# These entries causes errors on running sysctl -p. comment out.
# Disable netfilter on bridges.
sed -i.bak s/net.bridge.bridge-nf-call/#net.bridge.bridge-nf-call-/g /etc/sysctl.conf

sysctl -p
