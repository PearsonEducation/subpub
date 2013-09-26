Overview
========
True to it's name, SubPub is a system-to-system messaging solution designed to be a "black box", hiding as much
as possible the complexity inherent in messaging.  It's design goals are:

1. Interoperability
2. Ease of use / low barrier to adoption
3. Standard messaging stereotype (Pub/Sub, Request/Reply)
4. "Always On" architecture

In short, events are published into SubPub to which multiple consumers may subscribe to, for which SubPub
guarantees *at least once* delivery.
SubPub services wrap two sides (publishing and delivery) of the RabbitMQ AMQP messaging broker.
On the front-end/publishing side is an HTTP RESTful endpoint that allows a Principal to manage subscriptions
and publish messages.  The back-end/delivery side manages several processes which consume messages from the
AMQP broker and delivers them to Consumer Endpoints.  Consumer Endpoints are typically RESTful services.

Getting Started
===============
Installing pre-requisites

Installing Erlang
-----------------
Erlang will need to be installed locally.  On OS X, it's as simple as

    brew install erlang

Alternatively, you could download and install from source:

    wget http://www.erlang.org/download/otp_src_R15B01.tar.gz
    tar zxvf otp\_src\_R15B01.tar.gz
    cd otp\_src\_R15B01
    ./configure
    make
    sudo make install

Installing RabbitMQ
-------------------
SubPub utilizes RabbitMQ internally as it's message broker.  You will need to 
[download/install] (http://www.rabbitmq.com/download.html) from VMWare.  
If you are on OS X, Homebrew is the easiest way to get up and running.

    brew update
    brew install rabbitmq

And if you're installing from source:

    wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.8.1/rabbitmq-server\_2.8.1-1\_all.deb
    sudo dpkg -i --force-depends rabbitmq-server\_2.8.1-1\_all.deb
    sudo rabbitmq-plugins enable rabbitmq\_management
    sudo /etc/init.d/rabbitmq-server restart

Developing
==========
Let's get 'er running...

Building
--------
    export ERL\_LIBS="deps/erlang:$ERL\_LIBS"
    ./rebar clean compile

Running locally
---------------
Ensure that RabbitMQ is running locally and can accept connections from Prosper.io.  Typically, you start via:

    rabbitmq-server

And fire up Prosper.io:

    ./shell

Assuming all goes well, you can check your local Prosper.io service at 
[the status link] (http://localhost:4778/v1/status).

It should show something like this:

    {
       "status": {
          "served_by": "prospero@obi-wan.eclg.org",
          "current_time": "2012-08-24T14:55:01Z",
          "up_nodes": [
            {
              "node": {
                "name": "prospero@obi-wan.eclg.org",
                "version": "r3",
                "start_time": "2012-08-24T14:53:43Z",
                "uptime_seconds": 78,
                "num_messages_posted": 0,
                "num_messages_delivered": 0,
                "num_failed_deliveries": 0,
                "num_watched_subscriptions": 0,
                "rest_status": "serverin",
                "watched_subscriptions": [],
                "processes_awaiting_broker_reconnection": []
             }
           }
          ],
          "down_nodes": []
       }
    }
