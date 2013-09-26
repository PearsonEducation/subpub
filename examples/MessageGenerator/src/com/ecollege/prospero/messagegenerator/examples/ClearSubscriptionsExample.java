package com.ecollege.prospero.messagegenerator.examples;

import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class ClearSubscriptionsExample {
	public static void main(String[] args)
	{
		String principalId = "ONE";
		String prosperoHost = "localhost";
		int prosperoPort = 4778;
		String sharedKey = "1234567890123456";
		
		ProsperoPrincipal principal = new ProsperoPrincipal();
		principal.setKey(sharedKey);
		principal.setPrincipalId(principalId);
		principal.setProsperoHost(prosperoHost);
		principal.setProsperoPort(prosperoPort);

		Logger logger = new SystemOutLogger();

		try {
			Prospero.clearSubscriptions(principal, logger);
			logger.log("Done");
		} catch (Exception e) {
			logger.log(e);
		}
	}
}
