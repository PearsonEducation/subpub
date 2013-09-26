package com.ecollege.prospero.messagegenerator;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;

import com.ecollege.prospero.messagegenerator.logger.Logger;

public class SubscribeInput {
	private String callbackUrl = "";
	private String wsdlUri = "";
	private final ContextTagHashMap tags = new ContextTagHashMap();
	
	public String getCallbackUrl() {
		return callbackUrl;
	}
	public void setCallbackUrl(String callbackUrl) {
		this.callbackUrl = callbackUrl;
	}
	public String getWsdlUri() {
		return wsdlUri;
	}
	public void setWsdlUri(String wsdlUri) {
		this.wsdlUri = wsdlUri;
	}
	
	public void addTag(ContextTag tag)
	{
		tags.put(tag);
	}

	public ContextTagHashMap getTags() {
		return tags;
	}
	
	public void setMessageType(String value) {
		addTag(new ContextTag("MessageType", value));
	}

	public void setClient(String value) {
		addTag(new ContextTag("Client", value));
	}
	
	public void setClientString(String value) {
		addTag(new ContextTag("ClientString", value));
	}

	public void setSystem(String value) {
		addTag(new ContextTag("System", value));
	}

	public void setSubSystem(String value) {
		tags.put(new ContextTag("SubSystem", value));
	}
	
	public byte[] getAuthData()
	{
		return (getCallbackUrl() + getWsdlUri() + getTags().toCombinedShortString()).getBytes();
	}
	
	public HttpEntity createEntity(String authToken, Logger logger) throws Exception
	{
		List<NameValuePair> nvps = new ArrayList<NameValuePair>();
		nvps.add(new BasicNameValuePair("CALLBACK-URL", getCallbackUrl()));
		nvps.add(new BasicNameValuePair("WSDL-URI", getWsdlUri()));
		nvps.add(new BasicNameValuePair("TAGS", getTags().toCombinedShortString()));
		nvps.add(new BasicNameValuePair("AUTHORIZATION", authToken));
		
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(nvps, HTTP.ASCII);

		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		entity.writeTo(baos);
		logger.log("Url-Encoded Body: " + new String(baos.toByteArray()));
		
		return entity;
	}
}
