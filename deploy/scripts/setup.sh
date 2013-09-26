#!/bin/sh

. "`dirname $0`/always_included.sh"

sudo ln -fs "$scriptdir/prospero.init" /etc/init.d/prospero
sudo update-rc.d prospero defaults 20

sudo mkdir -vp "$LOG_PATH"
sudo mkdir -vp "$PROSPERO_MNESIA_DIR_BASE"
