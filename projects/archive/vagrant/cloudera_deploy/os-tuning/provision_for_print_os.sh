#!/bin/bash
################## Update O/S ports / TCP buffers / swappiness ####################

VM_SWAPPINESS=$(cat /proc/sys/vm/swappiness)

NET_IPV4_IP_LOCAL_PORT_RANGE=$(cat /proc/sys/net/ipv4/ip_local_port_range)

NET_CORE_RMEM=$(cat /proc/sys/net/core/rmem_max)
NET_CORE_wMEM=$(cat /proc/sys/net/core/wmem_max)

NET_IPV4_TCP_RMEM=$(cat /proc/sys/net/ipv4/tcp_rmem)
NET_IPV4_TCP_WMEM=$(cat /proc/sys/net/ipv4/tcp_wmem)

echo "O/S settings ..."
echo "VM_SWAPPINESS : $VM_SWAPPINESS"
echo "NET_IPV4_IP_LOCAL_PORT_RANGE : $NET_IPV4_IP_LOCAL_PORT_RANGE"

echo "NET_CORE_RMEM : $NET_CORE_RMEM"
echo "NET_CORE_wMEM : $NET_CORE_wMEM"

echo "NET_IPV4_TCP_RMEM : $NET_IPV4_TCP_RMEM"
echo "NET_IPV4_TCP_WMEM : $NET_IPV4_TCP_WMEM"

