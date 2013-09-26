package com.ecollege.prospero.wsdl.delivery;

public class Delivery {
	private String attemptId;
	private String messageType;
	private String messageId;
	private String authenticationToken;
	private String authenticationDelimiter;
	private byte[] payload;
	private String payloadContentType;
	
	public String getAttemptId() {
		return attemptId;
	}
	public void setAttemptId(String attemptId) {
		this.attemptId = attemptId;
	}
	public String getMessageType() {
		return messageType;
	}
	public void setMessageType(String messageType) {
		this.messageType = messageType;
	}
	public String getMessageId() {
		return messageId;
	}
	public void setMessageId(String messageId) {
		this.messageId = messageId;
	}
	public String getAuthenticationToken() {
		return authenticationToken;
	}
	public void setAuthenticationToken(String authorizationToken) {
		this.authenticationToken = authorizationToken;
	}
	public String getuthenticationDelimiter() {
		return authenticationDelimiter;
	}
	public void setAuthenticationDelimiter(String authorizationDelimiter) {
		this.authenticationDelimiter = authorizationDelimiter;
	}
	public byte[] getPayload() {
		return payload;
	}
	public void setPayload(byte[] payload) {
		this.payload = payload;
	}
	public String getPayloadContentType() {
		return payloadContentType;
	}
	public void setPayloadContentType(String payloadContentType) {
		this.payloadContentType = payloadContentType;
	}
	

	
}
