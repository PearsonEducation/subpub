#!/bin/bash

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

echo
sudo /etc/init.d/prospero start
