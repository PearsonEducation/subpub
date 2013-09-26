module.exports = {
	prospero: {
		sharedKey: "1234567890123456",
		principal: "ONE",
		//rootUrl: "http://sm4nic02.dmz.arch.ecollege.com"
		rootUrl: "http://localhost:4778"
	},
	deliveryPort: 8081,
	deliveryHost: "10.180.121.38",
	//deliveryHost: "localhost",
	//deliveryWait: function() { return 900 + Math.random() * 100 }, //Between 100ms and 1100ms
	messageType: "harness15",
	publishFrequencyMillis: 0,
	numProducers: 2,
	numMessagesToPublish: 10000,
	mongo: {
		host: "localhost"
	}
}
