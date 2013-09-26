package com.ecollege.prospero.messagegenerator.examples;

import com.ecollege.prospero.messagegenerator.PublishInput;
import com.ecollege.prospero.messagegenerator.examples.Publisher.PayloadSetter;


public class MultipartPublishExample {
	public static void main(String[] args)
	{
		new Publisher().Publish(new PayloadSetter() {
			@Override
			public void setPayload(String payload, PublishInput input) {
				input.setPayloadBytes("Hello".getBytes()); //Calling this will force a multipart posting instead of a url-encoded one
				input.setPayloadContentType("application/x-protobuf");
			}
		});
	}	
}
