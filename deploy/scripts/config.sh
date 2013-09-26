#!/bin/bash

. "`dirname $0`/always_included.sh"

run_template()
{
  infile=$1
  outfile=$2

  echo "$(eval "echo -e \"$(cat $infile | awk '{ gsub("\"", "\\\\\""); print }' )\"")" > "$outfile.tmp"
  cat "$outfile.tmp" | sed 's/\\/"/g' >> "$outfile" && rm "$outfile.tmp"
  echo "Generated $outfile from template $infile."
}

BROKERS_SECTION=""
DELIM=""

for KEY in $AMQP_KEYS
do
  AMQP_EXCHANGE_NAME=AMQP_EXCHANGE_$KEY
  AMQP_EXCHANGE=${!AMQP_EXCHANGE_NAME}

  AMQP_HOST_NAME=AMQP_HOST_$KEY
  AMQP_HOST=${!AMQP_HOST_NAME}

  AMQP_PORT_NAME=AMQP_PORT_$KEY
  AMQP_PORT=${!AMQP_PORT_NAME}

  AMQP_VIRTUAL_HOST_NAME=AMQP_VIRTUAL_HOST_$KEY
  AMQP_VIRTUAL_HOST=${!AMQP_VIRTUAL_HOST_NAME}

  AMQP_USERNAME_NAME=AMQP_USERNAME_$KEY
  AMQP_USERNAME=${!AMQP_USERNAME_NAME}

  AMQP_PASSWORD_NAME=AMQP_PASSWORD_$KEY
  AMQP_PASSWORD=${!AMQP_PASSWORD_NAME}

  AMQP_HEARTBEAT_MILLIS_NAME=AMQP_HEARTBEAT_MILLIS_$KEY
  AMQP_HEARTBEAT_MILLIS=${!AMQP_HEARTBEAT_MILLIS_NAME}

  TMP=`echo $(eval "echo -e \"$(cat $prosperodir/deploy/templates/prospero.broker.config.template | awk '{ gsub("\"", "\\\\\""); print }' )\"")`

  BROKERS_SECTION="$BROKERS_SECTION$DELIM$TMP"
  DELIM=",\n"
done

rm -f "$prosperodir/prospero.config"
run_template "$prosperodir/deploy/templates/prospero.config.template" "$prosperodir/prospero.config"