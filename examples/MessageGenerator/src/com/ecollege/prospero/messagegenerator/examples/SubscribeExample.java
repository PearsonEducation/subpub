package com.ecollege.prospero.messagegenerator.examples;

import com.ecollege.prospero.messagegenerator.ContextTag;
import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.SubscribeInput;
import com.ecollege.prospero.messagegenerator.SubscribeResponse;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class SubscribeExample {
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
		

		SubscribeInput input = new SubscribeInput();
		input.setClient("Client1");
		input.setClientString("ClientOne");
		input.setMessageType("MessageType1");	
		input.setSystem("Test");
		input.setSubSystem("Generator");
		input.addTag(new ContextTag("UserId", "UserOne"));
		input.setWsdlUri("http://localhost:8088/mockDeliveryServiceSoap11Binding?WSDL");
		
		Logger logger = new SystemOutLogger();

		SubscribeResponse response = Prospero.subscribe(input, principal, logger);
		
		if(response.getException() != null)
		{
			logger.log("An error occurred while attempting to subscribe!");
			logger.log(response.getException());
			System.exit(1);
		}
		else
		{
			logger.log("Subscription Id: " + response.getSubscriptionId());
		}
		logger.log("Done");
	}
}
