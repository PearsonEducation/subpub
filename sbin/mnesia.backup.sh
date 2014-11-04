#!/bin/bash

TS=$(date +%Y%m%d%H%M%S)
CP=$(which cp)
FIND=$(which find)
MKDIR=$(which mkdir)
RM=$(which rm)

BASE_DIR=/opt/prospero
MNESIA_DIR=${BASE_DIR}/data
BACKUP_DIR=${BASE_DIR}/backup
LOG_FILE=${BACKUP_DIR}/mnesia.backup.log.${TS}

exec > ${LOG_FILE} 2>&1

if [ ! -d ${BACKUP_DIR} ]; then
  echo; echo "Creating the Backup directory..."; echo
  ${MKDIR} -pv ${BACKUP_DIR}
fi

# copy the data directory to the backup directory and add a timestamp
echo; echo "Creating the Backup..."; echo
${CP} -rv ${MNESIA_DIR} ${BACKUP_DIR}/data.${TS}

# export the principals and subscriptions
${MKDIR} ${BACKUP_DIR}/export.${TS}
${BASE_DIR}/sbin/prospero-export-principals ${BACKUP_DIR}/export.${TS}/principals.xml
${BASE_DIR}/sbin/prospero-export-subscriptions ${BACKUP_DIR}/export.${TS}/subscriptions.xml

# cleanup backups older than 2 days
echo; echo "Cleaning up old Backups..."; echo
${FIND} ${BACKUP_DIR} -depth -mtime +1 -exec ${RM} -frv '{}' \;

