#!/bin/bash

TS=$(date +%Y%m%d%H%M%S)
CP=$(which cp)
FIND=$(which find)
TAR=$(which tar)
RM=$(which rm)
WC=$(which wc)
HOST=$(hostname)

BASE_DIR=/opt/prospero
MNESIA_DIR=${BASE_DIR}/data
BACKUP_DIR=${BASE_DIR}/backup
ARCHIVE_DIR=/mnt/prosperobackup
LOG_FILE=${ARCHIVE_DIR}/${HOST}.archive.log.${TS}

exec > ${LOG_FILE} 2>&1

if [ $(mount | grep ${ARCHIVE_DIR} | ${WC} -l) != 1 ]; then
  echo; echo "${ARCHIVE_DIR} is not mounted correctly...exiting."; echo
  exit
fi

# copy the data directory to the backup directory and add a timestamp
echo; echo "Creating the Archive..."; echo
${TAR} czvPf ${ARCHIVE_DIR}/${HOST}.${TS}.tar.gz ${BACKUP_DIR}

# cleanup backups older than 14 days
echo; echo "Cleaning up old Archives..."; echo
${FIND} ${ARCHIVE_DIR} -depth -iname ${HOST} -mtime +14 -exec ${RM} -frv '{}' \;

