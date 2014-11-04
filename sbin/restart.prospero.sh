#!/bin/bash

cat << EOF

Usage: ./restart.prospero.sh [clean]

If "clean" is added as an argument, all the logs will be deleted.

EOF

if [[ $(sudo -n whoami > /dev/null) ]]; then
    echo "Verified sudo access..."
else
	sudo whoami > /dev/null
	if [[ $? -ne 0 ]]; then
		echo "Sudo access failed."
		sleep 2
		exit
	fi
	echo "Verified sudo access..."
fi

sudo /etc/init.d/prospero stop
echo

while [ `ps aux | grep beam | wc -l` -gt 1 ]; do
	echo "...waiting for prospero to stop..."
	sleep 2
done

if [[ $1 == 'clean' ]]
then
	echo
	echo "...cleaning logs..."
	echo
	sudo rm -fr /var/log/prospero/*
fi

echo
sudo /etc/init.d/prospero start
