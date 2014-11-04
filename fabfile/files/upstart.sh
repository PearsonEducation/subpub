description "statsd"
author "Ryan Brown"

start on runlevel [2345]
stop on runlevel [!2345]

respawn limit 5 5

pre-start script
  touch /var/log/statsd.log
  chown sysadmin:sysadmin /var/log/statsd.log
end script

script
  su - sysadmin -c "cd /opt/statsd/node_modules/statsd; PATH=/opt/nodejs/nodejs/bin:$PATH; node stats.js /opt/statsd/statsdConfig.js  >> /var/log/statsd.log 2>&1"
end script
