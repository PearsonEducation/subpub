package com.ecollege.prospero.simulator;

import com.ecollege.prospero.messagegenerator.ContextTag;
import com.ecollege.prospero.messagegenerator.Prospero;
import com.ecollege.prospero.messagegenerator.ProsperoPrincipal;
import com.ecollege.prospero.messagegenerator.PublishInput;
import com.ecollege.prospero.messagegenerator.PublishResponse;
import com.ecollege.prospero.messagegenerator.logger.Logger;
import com.ecollege.prospero.messagegenerator.logger.SystemOutLogger;

public class Grade {
	public static void main(String[] args)
	{
		if(args.length < 13)
			printUsageAndQuit();
		
		MessageProperties props = new MessageProperties();
		
		String prosperoHost = args[0];
		int prosperoPort = Integer.parseInt(args[1]);
		String principalId = args[2];
		String sharedKey = args[3];
		
		ProsperoPrincipal principal = new ProsperoPrincipal();
		principal.setKey(sharedKey);
		principal.setPrincipalId(principalId);
		principal.setProsperoHost(prosperoHost);
		principal.setProsperoPort(prosperoPort);

		props.put("User.ID", args[4]);
		props.put("User.LoginID", args[5]);
		props.put("Course.ID", args[6]);
		props.put("Course.CourseCallNumbers", args[7]);
		props.put("User.Grade", args[8]);
		props.put("Grade.ID", args[9]);
		props.put("GradableItem.PointsPossible", args[10]);
		props.put("Grade.PointsAchieved", args[11]);
		props.put("UTCDate", args[12]);
		
		String json = props.asJSON("Event.GradeBook.GradeEvent");
		PublishInput input = new PublishInput();
		input.setClient(args.length >= 14 ? args[13] : SimulatorSettings.CLIENT);
		input.setClientString(args.length >= 15 ? args [14] : SimulatorSettings.CLIENT_STRING);
		input.setMessageType("Event.GradeBook.GradeEvent");
		input.setPayloadContentType("application/x-javascript");
		input.setPayloadString(json);
		input.setRealm("*");
		input.setSubSystem(SimulatorSettings.SUB_SYSTEM);
		input.setSystem(SimulatorSettings.SYSTEM);
		input.addTag(new ContextTag("Test", "one"));
		input.addTag(new ContextTag("Test", "two"));
		
		
		Logger logger = new SystemOutLogger();
		
		logger.log("Payload: " + json);

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
	
	private static void printUsageAndQuit()
	{
		System.out.println("USAGE: java -cp messagegenerator.jar com.ecollege.prospero.simulator.Grade prosperoHost prosperoPort principalId sharedKey userId loginId courseId courseCallNumbers grade gradeId pointsPossible pointsAchieved date [clientId] [clientString]");
		System.exit(1);
	}
}
