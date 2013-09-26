package com.ecollege.prospero.simulator;

import com.ecollege.prospero.messagegenerator.ContextTag;
import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.SubscribeInput;
import com.ecollege.prospero.messagegenerator.SubscribeResponse;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class Subscribe {
	public static void main(String[] args)
	{
		if(args.length != 7)
			printUsageAndQuit();
		
		String prosperoHost = args[0];
		int prosperoPort = Integer.parseInt(args[1]);
		String principalId = args[2];
		String sharedKey = args[3];
		String messageType = args[4];
		String callbackUrl = args[5];
		String wsdlUri = args[6];
		
		ProsperoPrincipal principal = new ProsperoPrincipal();
		principal.setKey(sharedKey);
		principal.setPrincipalId(principalId);
		principal.setProsperoHost(prosperoHost);
		principal.setProsperoPort(prosperoPort);
		
		
		SubscribeInput input = new SubscribeInput();
		input.setCallbackUrl(callbackUrl);
		input.setMessageType(messageType);
		input.setWsdlUri(wsdlUri);
		input.addTag(new ContextTag("Test", "two"));
		
		Logger logger = new SystemOutLogger();

		try {
			SubscribeResponse response = Prospero.subscribe(input, principal, logger);
			logger.log("Created subscription " + response.getSubscriptionId());
			logger.log("Done");
		} catch (Exception e) {
			logger.log(e);
		}
	}
	
	private static void printUsageAndQuit()
	{
		System.out.println("USAGE: java -cp messagegenerator.jar com.ecollege.prospero.simulator.Subscribe prosperoHost prosperoPort principalId sharedKey messageType callbackurl wsdluri");
		System.exit(1);
	}
}
