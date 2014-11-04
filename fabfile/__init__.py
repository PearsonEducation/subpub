from fabric.api import env
from fabric.operations import run, put, sudo, local
from fabric.context_managers import cd, settings
from fabric.contrib.files import upload_template, exists
import json
import nibiru
from fabric.tasks import execute
import fabric.operations
from fabric.operations import put
import fabric.contrib.files
import config
import os
import datetime

def environment(environment):
	env.user = "" + config.environment['user']
	env.password = config.environment['password']
	env.hosts = config.environment['hosts']
	keyFile = config.environment.get('keyfile', None)
	if keyFile is not None:
		env.key_filename = keyFile
#
# setup erlang
#
def setupErlang():
	#
	# This set of packages was determined by dpkg -i on the erlang package and
	# then running apt-get install -f and capturing the packages that were
	# installed.
	#
	sudo("apt-get update")
	sudo("apt-get -qy install ca-certificates-java default-jre-headless fontconfig fontconfig-config hicolor-icon-theme icedtea-6-jre-cacao icedtea-6-jre-jamvm java-common libatk1.0-0 libatk1.0-data libavahi-client3 libavahi-common-data libavahi-common3 libcairo2 libcups2 libdatrie1 libfontconfig1 libgdk-pixbuf2.0-0 libgdk-pixbuf2.0-common libgl1-mesa-dri libgl1-mesa-glx libglapi-mesa libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 libgtk2.0-0 libgtk2.0-bin libgtk2.0-common libice6 libjasper1 libjpeg-turbo8 libjpeg8 libllvm3.0 libnspr4 libnss3 libnss3-1d liborc-0.4-0 libpango1.0-0 libpixman-1-0 libsm6 libthai-data libthai0 libtiff4 libwxbase2.8-0 libwxgtk2.8-0 libx11-xcb1 libxcb-glx0 libxcb-render0 libxcb-shm0 libxcomposite1 libxcursor1 libxdamage1 libxfixes3 libxft2 libxi6 libxinerama1 libxrandr2 libxrender1 libxxf86vm1 openjdk-6-jre-headless openjdk-6-jre-lib shared-mime-info ttf-dejavu-core tzdata-java x11-common tzdata")
	sudo("wget -nc --header='Accept-Encoding: gzip' http://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_15.b.1~ubuntu~precise_amd64.deb")
	sudo("dpkg -i esl-erlang_15.b.1~ubuntu~precise_amd64.deb")

#
# setup rabbitmq
#
def setupRabbitmq():
	sudo("apt-get -qy install erlang-nox erlang-diameter")
	#fabric.operations.put("misc/rabbitmq-server_2.8.1-1_all.deb")
	sudo("wget -nc --header='Accept-Encodeing: gzip' http://www.rabbitmq.com/releases/rabbitmq-server/v3.2.2/rabbitmq-server_3.2.2-1_all.deb")
	#sudo("dpkg -i rabbitmq-server_2.8.1-1_all.deb")	
	sudo("dpkg -i rabbitmq-server_3.2.2-1_all.deb")
	sudo("rabbitmq-plugins enable rabbitmq_management")
	sudo("/etc/init.d/rabbitmq-server start", pty=False)

#
# install and setup the prospero distribution
#
def installProspero():
	stopProspero()
#	cleanup()
	backupProspero()
	safeCleanup()
	createDistribution()
	copyProspero()
	setupProsperoConfig()
	setupEnvVars()
#	restartProspero()
#	startProspero()

#
# issue a stop command if a control script exists 
#
def stopProspero():
	if fabric.contrib.files.exists("/etc/init.d/prospero",use_sudo=True):
		sudo("/etc/init.d/prospero stop", pty=False)

#
# Backup the live Prospero directory with a timestamp
#
def backupProspero():
	ts = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
	sudo("cp -r /opt/prospero /opt/prospero." + ts)

#
# remove the various files and directories that prospero uses
#
def cleanup():
	sudo ("rm -rf /var/log/prospero")
	sudo ("rm -rf /opt/prospero")
	sudo ("rm -rf /opt/prospero/data")
	sudo ("rm -f /etc/init.d/prospero")

#
# remove the various files and directories that prospero uses
#
def safeCleanup():
	sudo ("rm -rf /var/log/prospero")
	sudo ("rm -rf /tmp/prospero/data")
	sudo ("mkdir -p /tmp/prospero/data")
	if not os.path.exists("/opt/prospero/data"):
		sudo ("mkdir -p /opt/prospero/data")

	sudo ("cp -r /opt/prospero/data /tmp/prospero/")
	sudo ("rm -rf /opt/prospero/")
	sudo ("rm -f /etc/init.d/prospero")


#
# copy over the prospero distribution
#
# This method also removes any previous distribution
#
def copyProspero():
	fabric.operations.put("/tmp/subpub.tar.gz", "/tmp/subpub.tar.gz")
	sudo ("mkdir /opt/prospero")
	with cd("/opt/prospero"):
		sudo("tar xmzpf /tmp/subpub.tar.gz")
		sudo("/opt/prospero/compile")
	sudo ("ln -fs /opt/prospero/deploy/scripts/prospero.init /etc/init.d/prospero")
	sudo ("update-rc.d prospero defaults 20")
	sudo ("mkdir -p /opt/prospero/data")
	sudo ("cp -r /tmp/prospero/data /opt/prospero/")
	sudo ("rm -rf /tmp/prospero/data")
	sudo ("mkdir -p /var/log/prospero")
#
# create a prospero.config file 
#
def setupProsperoConfig():
	fabric.contrib.files.upload_template("prospero.config.jinja", "/opt/prospero/prospero.config", use_jinja=True, context=config.misc, template_dir="deploy/templates", use_sudo=True)
	print "Copied jinja template"

#
# create an EnvVars.sh file
#
# This is created by running a jinja template with the values obtained 
# from the misc.py properties file.
#
def setupEnvVars():
	fabric.contrib.files.upload_template("EnvVars.sh.jinja", "/opt/prospero/deploy/environmental/EnvVars.sh", use_jinja=True, context=config.misc, template_dir="deploy/templates", use_sudo=True)
	print "Uploaded EnvVars.sh"

def startProspero():
	sudo ("/etc/init.d/prospero start", pty=False)

def restartProspero():
	sudo ("/opt/prospero/sbin/restart.prospero.sh")

#
# create the deployment distribution
#
def createDistribution():
	local("rm -rf /tmp/subpub.staging")
	local("cp -r . /tmp/subpub.staging")
	local("rm -rf /tmp/subpub.staging/examples")
	local("rm -rf /tmp/subpub.staging/.git")
	local("rm -rf /tmp/subpub.staging/misc/*.deb")
	local("tar cfz /tmp/subpub.tar.gz -C /tmp/subpub.staging .")

def installNodejs():
	sudo("apt-get -qy install make g++")
  	sudo("mkdir -p /tmp/node/")
	sudo("wget --header='Accept-Encoding: gzip' http://nodejs.org/dist/v0.10.15/node-v0.10.15.tar.gz -P /tmp/node/")
	with cd("/tmp/node/"):
		sudo("tar xmzpf /tmp/node/node-v0.10.15.tar.gz")
  	with cd("/tmp/node/node-v0.10.15/"):
		sudo("./configure")
		sudo("make")
		sudo("make install") 

def installStatsd():
	sudo('rm -rf /opt/statsd')
	sudo('mkdir /opt/statsd')
	sudo('export PATH=/opt/nodejs/nodejs/bin:$PATH; cd /opt/statsd; npm install statsd')
	put(os.path.abspath(os.path.join(os.path.dirname(__file__), 'files', 'statsdConfig.js')), '/opt/statsd/statsdConfig.js', use_sudo=True)
	put(os.path.abspath(os.path.join(os.path.dirname(__file__), 'files', 'upstart.sh')), '/etc/init/statsd.conf', use_sudo=True)

def startStatsd():
	sudo('start statsd')
	sudo('status statsd')

def stopStatsd():
	sudo('stop statsd')

