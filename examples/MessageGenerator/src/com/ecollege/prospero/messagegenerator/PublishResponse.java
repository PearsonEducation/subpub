package com.ecollege.prospero.messagegenerator;

public class PublishResponse {
	private String messageId;
	private Exception exception;

	public PublishResponse(String messageId) {
		super();
		this.messageId = messageId;
	}

	public PublishResponse(Exception exception) {
		super();
		this.exception = exception;
	}

	public String getMessageId() {
		return messageId;
	}

	public Exception getException() {
		return exception;
	}
}
