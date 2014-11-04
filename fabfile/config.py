#
# brokers is a list of RabbitMQ hosts that prospero will try
# to connect to.
#
brokers=[
    {
        "name":     "one",
        "host":     "",
        "username": "guest",
        "password": "guest"
    }
]

#
# rest controls the way that the server handles the REST port
# that the system presents to fulfill services
#
rest = {
	#
	# The port that the system will listen to for REST requests.
	#
    'port': 80,

	#
	# this allows the REST port to do things like clear all subscriptions.
	# It should essentially never have a value of true for production.
	#
    'enableTestServices': 'false',

    'secureAuthMessages': 'false',
    'serverNickname':     'prospero'
}

#
# miscellaneous properties for the system.  This also includes the 
# brokers and the REST properties.
#
misc = {
	#
	# The bootstrap property controls whether this installation will be 
	# standalone or clustered.
	#
    'bootstrap': 'standalone',

	#
	# The location in the file system that prospero will put various 
	# logfiles and the like
	#
	'logPath': '/var/log/prospero',

	#
	# The file where prospero will put the audit logfile
	#
    'auditLogPath': '/var/log/prospero/prospero.audit.log',

	#
	# The number of times that the system will try to deliver a message to 
	# a subscriber before giving up on the message
	#
    'maxFailedAttempts': 10,

	# 
	# The number of milliseconds that the system will wait for a reply to 
	# a delivery (HTTP POST) of a message to one of the clients 
	# 
    'callbackTimeoutMillis': 10000,

	#
	# this is the erlang cookie that all nodes in the prospero cluster 
	# should use.  It is essentially a password that all the nodes need to
	# share in order to communicate.
	#
	'cookie': 'COOKIE',

	#
	# The directory where all the database files live
	#
	'dataPath': '/opt/prospero/data',

	'high_water_mark': 16000000000,

    'rest':                   rest,
    'brokers':                brokers,
    'high_water_mark':		8000000000

}

environment = {
	'user': 'ubuntu',
	'password': '',
	'hosts': [''],
	'keyfile': ''
}
