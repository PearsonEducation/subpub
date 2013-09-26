package com.ecollege.prospero.messagegenerator.examples;

import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class GetSubscriptionExample {
	public static void main(String[] args) throws Exception
	{
		String principalId = "ONE";
		String prosperoHost = "localhost";
		int prosperoPort = 4778;
		String sharedKey = "1234567890123456";
		String subscriptionId = "put your sub id here";
		
		ProsperoPrincipal principal = new ProsperoPrincipal();
		principal.setKey(sharedKey);
		principal.setPrincipalId(principalId);
		principal.setProsperoHost(prosperoHost);
		principal.setProsperoPort(prosperoPort);
				
		Logger logger = new SystemOutLogger();

		String response = Prospero.getSubscription(subscriptionId, principal, logger);
		
		logger.log("Response: " + response);
		logger.log("Done");
	}
}
