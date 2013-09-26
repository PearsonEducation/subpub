package com.ecollege.prospero.wsdl.intake;

import java.util.ArrayList;

public class Message {
	private String messageType;
	private ContextTag[] contextTags;
	private String authenticationToken;
	private String authenticationDelimiter;
	private byte[] payload;
	
	public String getMessageType() {
		return messageType;
	}
	public void setMessageType(String messageType) {
		this.messageType = messageType;
	}
	public ContextTag[] getContextTags() {
		return contextTags;
	}
	public void setContextTags(ContextTag[] contextTags) {
		this.contextTags = contextTags;
	}
	public String getAuthenticationToken() {
		return authenticationToken;
	}
	public void setAuthenticationToken(String authorizationToken) {
		this.authenticationToken = authorizationToken;
	}
	public String getAuthenticationDelimiter() {
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
}
