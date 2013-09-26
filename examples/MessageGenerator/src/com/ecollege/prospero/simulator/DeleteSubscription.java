package com.ecollege.prospero.simulator;

import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.UnsubscribeResponse;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class DeleteSubscription {
	public static void main(String[] args)
	{
		if(args.length != 5)
			printUsageAndQuit();
		
		String prosperoHost = args[0];
		int prosperoPort = Integer.parseInt(args[1]);
		String principalId = args[2];
		String sharedKey = args[3];
		String subscriptionId = args[4];
		
		ProsperoPrincipal principal = new ProsperoPrincipal();
		principal.setKey(sharedKey);
		principal.setPrincipalId(principalId);
		principal.setProsperoHost(prosperoHost);
		principal.setProsperoPort(prosperoPort);
				
		Logger logger = new SystemOutLogger();

		try {
			UnsubscribeResponse response = Prospero.unsubscribe(subscriptionId, principal, logger);
			if(response.getException() == null)
			{
				logger.log("Deleted subscription " + response.getSubscriptionId());
			}
			logger.log("Done");
		} catch (Exception e) {
			logger.log(e);
		}
	}
	
	private static void printUsageAndQuit()
	{
		System.out.println("USAGE: java -cp messagegenerator.jar com.ecollege.prospero.simulator.DeleteSubscription prosperoHost prosperoPort principalId sharedKey subscriptionId");
		System.exit(1);
	}
}
