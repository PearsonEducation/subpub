package com.ecollege.prospero.simulator;

import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class ClearSubscriptions {
	public static void main(String[] args)
	{
		if(args.length != 4)
			printUsageAndQuit();
		
		String prosperoHost = args[0];
		int prosperoPort = Integer.parseInt(args[1]);
		String principalId = args[2];
		String sharedKey = args[3];
		
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
	
	private static void printUsageAndQuit()
	{
		System.out.println("USAGE: java -cp messagegenerator.jar com.ecollege.prospero.simulator.ClearSubscriptions prosperoHost prosperoPort principalId sharedKey");
		System.exit(1);
	}
}
