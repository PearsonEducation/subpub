package com.ecollege.prospero.messagegenerator;

public class UnsubscribeResponse {
	private String subscriptionId;
	private Exception exception;

	public UnsubscribeResponse(String subscriptionId) {
		super();
		this.subscriptionId = subscriptionId;
	}

	public UnsubscribeResponse(Exception exception) {
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
