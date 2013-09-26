var config = require('./config.js');
var express = require('express');
var Prospero = require('eclg-prospero');
var callbackUrl = "http://" + config.deliveryHost + ":" + config.deliveryPort + "/prospero";
var console = require('console');
var util = require('util');
var mongodb = require('mongodb');
var argv = require('optimist').demand(['clear']).boolean('clear').argv;
var Table = require('cli-table');
var http = require('http');
var cluster = require('cluster');
var metrics = require('metrics');


config.prospero.prosperoDefaults = {client: "harness", clientString: "harness", system: "harness", subSystem: "harness", messageType: config.messageType};

var publishTimer;
var publishErrorCounter;
var deliveryTimer;

var db = new mongodb.Db('harness', new mongodb.Server(config.mongo.host, 27017, {}));
var dbClient;
var messagesCollection;

var output = new Table({
    head: ['Published','Pub Rate','Pub Err','Pub Min','Pub Max','Pub Mean','Pub StDev','Pub 50%','Pub 90%','Pub 95%','Delivered','Pending','Undelivered','Del Rate','Del Mean','Del Stddev','Del Max','Dupes']
  , colWidths: [12, 9, 9, 9, 9, 11, 11, 9, 9, 9, 11, 9, 13, 10, 10, 12, 10, 9]
});
output.push([]);


db.open(function (error, _client) {
	dbClient = _client;
	if (error) throw error;

	messagesCollection = new mongodb.Collection(dbClient, 'messages');

	if(argv.clear && cluster.isMaster) {
		messagesCollection.drop(function() {
			console.log("Cleared the mongo collection");
		});
	}

	if(cluster.isMaster) {
		publishTimer = new metrics.Timer;
		deliveryTimer = new metrics.Timer;
		publishErrorCounter = new metrics.Counter;

		for (var i = 0; i < config.numProducers; i++) {
			console.log("Forking producer " + i);
			var worker = cluster.fork();

			worker.on('message', function(msg) {
				if(msg.published) {
					publishTimer.update(msg.time);
				}
				else if(msg.publish_error) {
					publishErrorCounter.inc();
				}
				else if(msg.delivered) {
					deliveryTimer.update(msg.time);
				}
			});

		}
		setInterval(printStats, 5000);
		console.log("Running!");	
	} else {
		var prospero = new Prospero(config.prospero);

		http.globalAgent.maxSockets = 100;

		function initProspero() {	
			prospero.subscribe({messageType: config.messageType, callbackUrl: callbackUrl}, function(error, result) {
				if(error) {
					console.log("Error subscribing to Prospero: " + util.inspect(error, false, 3));
					return;
				}
				console.log("Subscribed to Prospero: " + result.data.subscription.id);
				publishProsperoMessage();
			});
		}

		var app = express.createServer();

		app.post("/prospero", function(req, res, next) {
			prospero.receive(req, res, receiveProsperoMessage);
		});

		app.listen(config.deliveryPort);

		initProspero();

		var numMessagesPublished = 0;

		function receiveProsperoMessage(error, data, done) {
			if(error) {
				done(false, error.toString());
				return;
			}

			now = new Date().getTime();
			var pubTime = data['JSON-PAYLOAD'].time;
			var recordId = new mongodb.ObjectID(data['JSON-PAYLOAD'].record);

			var latency = now - pubTime;
			messagesCollection.update({_id: recordId}, {"$push" : {deliveries: latency}, "$inc" : {delivery_count: 1}}, {upsert:true});

			process.send({ delivered: true, time: latency });

			if(config.deliveryWait !== undefined) {
				var waitPeriod = config.deliveryWait();
				setTimeout(function() {done(true, "OK!");}, waitPeriod);
			} else {
				done(true, "OK!");			
			}
		}

		function publishProsperoMessage() {
			messagesCollection.save({}, {safe: true}, function(err, record) {
		      	if(err) {
		        	console.log("Problem writing initial message row: " + err);
			        return;
			    }

			    var now = new Date().getTime();
				prospero.publish({realm: "realm16", payload: JSON.stringify({record: record._id.toString(), time: now})}, function(error, data, publishTime) {
					if(error) {
						process.send({ publish_error: true });
						return;
					}
					var msgId = data.data.message.id;

					process.send({ published: true, time: publishTime });
					messagesCollection.update({_id: record._id},{"$set" : {published: {at: now, duration: publishTime}, msg: msgId}}, {upsert: true}, function(error2, data) {
						++numMessagesPublished;
						if(numMessagesPublished < config.numMessagesToPublish) {
							setTimeout(publishProsperoMessage, config.publishFrequencyMillis);
						}
					});
				});
		    });
		}
	}
});

function printStats() {
	messagesCollection.count({deliveries: {"$exists": false}, published: {"$exists": true}}, function(err, totalNotDelivered) {
		thirtySecondsAgo = new Date().getTime() - 30000;

		messagesCollection.count({deliveries: {"$exists": false}, "published.at": {"$lt": thirtySecondsAgo}}, function(err2, undeliveredCount) {

			messagesCollection.count({deliveries: {"$exists": true}}, function(err3, totalDeliveries) {
				messagesCollection.count({delivery_count: {"$gt": 1}}, function(err4, dupeDeliveries) {
					var tmp1 = publishTimer.percentiles();
					var tmp = [
						publishTimer.count(), 
						publishTimer.oneMinuteRate(), 
						publishErrorCounter.count, 
						publishTimer.min(), 
						publishTimer.max(), 
						publishTimer.mean(), 
						publishTimer.stdDev(), 
						tmp1['0.5'], 
						tmp1['0.9'], 
						tmp1['0.95'], 
						totalDeliveries, 
						totalNotDelivered, 
						undeliveredCount,  //At least 30 seconds late
						deliveryTimer.oneMinuteRate(), 
						deliveryTimer.mean(), 
						deliveryTimer.stdDev(), 
						deliveryTimer.max(),
						dupeDeliveries
					];
					output.shift();
					output.push(tmp);
					console.log(output.toString());					
				})
			});
		});
	});
}
