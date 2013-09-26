#!/bin/bash
#Installs Rabbit via debian package and dpkg -i.  Ignores the Erlang dependency and assumes Erlang is already installed.

RABBIT_VERSION=2.8.1
RABBIT_BUILD=1

ORIG_PWD=`pwd`

cd
wget http://www.rabbitmq.com/releases/rabbitmq-server/v$RABBIT_VERSION/rabbitmq-server_${RABBIT_VERSION}-${RABBIT_BUILD}_all.deb
sudo dpkg -i --force-depends rabbitmq-server_${RABBIT_VERSION}-${RABBIT_BUILD}_all.deb
sudo rabbitmq-plugins enable rabbitmq_management
sudo /etc/init.d/rabbitmq-server restart

cd $ORIG_PWD
echo "Done installing Erlang ${RABBIT_VERSION}-${RABBIT_BUILD}."