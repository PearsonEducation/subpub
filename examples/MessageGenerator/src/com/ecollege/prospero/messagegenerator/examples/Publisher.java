package com.ecollege.prospero.messagegenerator.examples;

import com.ecollege.prospero.messagegenerator.ContextTag;
import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.PublishInput;
import com.ecollege.prospero.messagegenerator.PublishResponse;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class Publisher {
	public interface PayloadSetter
	{
		void setPayload(String payload, PublishInput input);
	}
	
	public void Publish(PayloadSetter setter)
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
		

		PublishInput input = new PublishInput();
		input.setClient("Client1");
		input.setClientString("ClientOne");
		input.setMessageType("MessageType1");	
		input.setRealm("*");
		input.setSystem("Test");
		input.setSubSystem("Generator");
		input.addTag(new ContextTag("UserId", "UserOne"));
		
		setter.setPayload("Hello", input);
		
		Logger logger = new SystemOutLogger();

		PublishResponse response = Prospero.publish(input, principal, logger);
		
		if(response.getException() != null)
		{
			logger.log("An error occurred while attempting to publish!");
			logger.log(response.getException());
			System.exit(1);
		}
		else
		{
			logger.log("Message Id: " + response.getMessageId());
		}
		logger.log("Done");
	}
}
