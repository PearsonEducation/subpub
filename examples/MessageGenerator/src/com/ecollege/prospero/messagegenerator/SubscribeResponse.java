package com.ecollege.prospero.messagegenerator;

public class SubscribeResponse {
	private String subscriptionId;
	private Exception exception;

	public SubscribeResponse(String subscriptionId) {
		super();
		this.subscriptionId = subscriptionId;
	}

	public SubscribeResponse(Exception exception) {
		super();
		this.exception = exception;
	}

	public String getSubscriptionId() {
		return subscriptionId;
	}

	public Exception getException() {
		return exception;
	}
}
