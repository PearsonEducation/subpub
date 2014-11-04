This fabric script has several options and commands. Listed below is each task and what you can expect from them.

setupErlang()

* This task will simply install Erlang 15B (The recommended minimum version for running SubPub) and all dependent packages.  

setupRabbitmq()

* Sets-up RabbitMq version 2.8.1 (the version currently running in production) and the dependent packages.

installProspero()

* Stops SubPub
* Packages SubPub based on the code on the deployment machine
* Copies the package to the remote /tmp directory and extracts it
* Copies the files from the /tmp directory to /home/<provided_user>/subpub
* Cleans-up the tmp directory
* Creates the prospero.config based on the prospero.config.jinja template
* Set-up EnvVars
* Starts SubPub

installNodejs()

* Downloads and installs the latest version of Nodejs. (This is a requirement for statsd.)

installStatsd()

* Installs statsd from source
* Copies config from files directory 
* Copies upstart script from files directory


Deploying SubPub (F.K.A. prospero)

To install SubPub to completely clean box you will want to first set-up the configuration appropritate for the environment you are deploying to. Each environment that you plan to deploy to should have a related 'environment'_settings.env in the fabfile directory with the following format:

{
    "user": "sudo_user", // Username for either the sudo user for deployment or the user you wish to deploy with. If not a sudo user, must provide keyfile below for sudo user.
    "password": "sudo_password", // Password for either the sudo user for deployment or the user you wish to deploy with. If not a sudo user, must provide keyfile below for sudo user.
    "hosts": [ // List of hosts to perform task on
        "prospero01.ecollege.net",
        "prospero02.ecollege.net"
    ],
    "keyfile": "/path/to/keyfile.pem" //This value is optional if providing a sudo user and password for deploying the application.
}

With this file in pace you can run any command providing the env task the environment to deploy to then the task(s) you want to perform.

ex:

  fab environment:staging installProspero

This will read the associated staging_settings.env file to get the user, password, keyfile (if provided) and list of hosts and perform the second task (in this case installProspero) on each host with the given credentials.
