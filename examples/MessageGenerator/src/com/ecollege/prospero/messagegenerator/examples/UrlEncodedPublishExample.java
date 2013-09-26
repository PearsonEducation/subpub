package com.ecollege.prospero.messagegenerator.examples;

import com.ecollege.prospero.messagegenerator.PublishInput;
import com.ecollege.prospero.messagegenerator.examples.Publisher.PayloadSetter;

public class UrlEncodedPublishExample {
	public static void main(String[] args)
	{
		new Publisher().Publish(new PayloadSetter() {
			@Override
			public void setPayload(String payload, PublishInput input) {
				input.setPayloadString("Hello"); //Calling this will force a url-encoded posting instead of a multipart one
				input.setPayloadContentType("text/plain");
			}
		});
	}	
}
