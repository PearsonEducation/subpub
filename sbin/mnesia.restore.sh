#!/bin/bash

BASE_DIR=/opt/prospero
MNESIA_DIR=$BASE_DIR/data
BACKUP_DIR=$BASE_DIR/backup

TS=`date +%Y%m%d%H%M%S`

echo "Enter the number of the backup directory you'd like to restore:"

PS3="Your choice: "
QUIT="Quit"
cd $BACKUP_DIR
sudo touch $QUIT

select RESTORE_DIR in *;
do
	case $RESTORE_DIR in
		"$QUIT")
			echo "Exiting."
			break
			;;
		*)
			echo "First we'll backup the old directory..."
			sudo cp -rv $MNESIA_DIR $BACKUP_DIR/data.replaced.$TS
			echo "Now restoring $RESTORE_DIR..."
			sudo rm -fr $MNESIA_DIR
			sudo cp -rv $RESTORE_DIR $BASE_DIR/data
			;;
	esac
done

rm -fr "$QUIT"


