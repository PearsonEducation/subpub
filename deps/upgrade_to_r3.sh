#!/bin/sh

ORIG_DIR=`pwd`

cd `dirname $0`
ROOT_DIR=`pwd`

cd $ROOT_DIR

#Stop the old prospero if it is running
sudo /etc/init.d/prospero stop

#The old prospero was installed to /opt/prospero, so lets' move it to /opt/prospero-<version>
PREV_VERSION=`grep VERSION /opt/prospero/include/prospero.hrl | sed -E "s/-define\(PROSPERO_VERSION, (.*)\)\./\1/g"`
sudo mv /opt/prospero /opt/prospero-${PREV_VERSION}

./install_erlang.sh
./install_rabbit.sh

#Set up the EnvVars.sh file from the sample and make sure secure_error_messages is disabled
cat ../deploy/environmental/EnvVars.sh.sample | sed -E "s/REST_SECURE_AUTH_MESSAGES=.*/REST_SECURE_AUTH_MESSAGES=false/g" > ../deploy/environmental/EnvVars.sh

#Run the standard Prospero setup
sudo ../deploy/scripts/setup.sh
sudo ../deploy/scripts/config.sh

cat ../prospero.config | sed -E "s/serverout/serverin/g" > ../prospero.config.tmp
mv ../prospero.config.tmp ../prospero.config

#Copy the database from the old version to the new one before starting back up
sudo cp -r /opt/prospero-${PREV_VERSION}/data ../

sudo /etc/init.d/prospero start

cd $ORIG_DIR