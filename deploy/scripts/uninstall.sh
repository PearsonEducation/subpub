#!/bin/sh

. "`dirname $0`/always_included.sh"

if [ -e "/etc/init.d/prospero" ]; then 
	sudo /etc/init.d/prospero stop
	sudo update-rc.d -f prospero remove
	sudo rm -fR /etc/init.d/prospero
fi

#Unlink erlang deps
if [ -e "$installed_erlang_libs_file" ]; then 

	while read -r line
  	do
    	echo "Removing symlink for ${line}"
		sudo rm -fR ${line}
	done < "$depsdir/erlang/installed_erlang_libs.history"
fi

#Uninstall the YAWS package
sudo apt-get -y remove yaws





