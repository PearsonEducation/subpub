#!/bin/sh
#CKC - 3/19/2012 - This script is pretty much obsolete.

ORIG_DIR=`pwd`

cd `dirname $0`
cd ..
ROOT_DIR=`pwd`

cd $ROOT_DIR

sudo ./deps/install_erlang.sh
sudo ./deps/install_rabbit.sh
sudo ./deploy/scripts/setup.sh
sudo ./deploy/scripts/config.sh

sudo /etc/init.d/prospero start
sleep 5
./sbin/create-test-principal $1

cd $ORIG_DIR
