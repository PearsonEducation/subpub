package com.ecollege.prospero.messagegenerator;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;

import com.ecollege.prospero.messagegenerator.logger.Logger;

public class PublishInput {
	private String messageType;
	private String client;
	private String clientString;
	private String system;
	private String subSystem;
	private byte[] payload;
	private String realm = "*";
	private String payloadContentType;

	private final ContextTagHashMap tags = new ContextTagHashMap();
	
	public String getMessageType() {
		return messageType;
	}
	
	public void setMessageType(String messageType) {
		this.messageType = messageType;
	}
	
	public String getClient() {
		return client;
	}
	
	public void setClient(String client) {
		this.client = client;
	}
	
	public String getClientString() {
		return clientString;
	}
	
	public void setClientString(String clientString) {
		this.clientString = clientString;
	}
	
	public String getSystem() {
		return system;
	}
	
	public void setSystem(String system) {
		this.system = system;
	}
	
	public String getSubSystem() {
		return subSystem;
	}
	
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}
	
	public byte[] getPayloadBytes() {
		return payload;
	}
	
	public void setPayloadBytes(byte[] payload) {
		this.payload = payload;
	}
	
	public void setPayloadAsSerializedXml(Object o) throws UnsupportedEncodingException {
		setPayloadBytes(MessageSerializer.serialize(o));
	}

	public String getPayloadString() {
		return new String(getPayloadBytes());
	}

	public void setPayloadString(String payloadString) {
		this.payload = payloadString.getBytes();
	}

	public String getRealm() {
		return realm;
	}

	public void setRealm(String realm) {
		this.realm = realm;
	}
	
	public String getPayloadContentType() {
		return payloadContentType;
	}

	public void setPayloadContentType(String payloadContentType) {
		this.payloadContentType = payloadContentType;
	}
	
	public void addTag(ContextTag tag)
	{
		tags.put(tag);
	}
	
	public ContextTagHashMap getTags()
	{
		return tags;
	}
	
	private boolean isBinaryPayload()
	{
		if( this.payloadContentType.startsWith("text/") )
			return false;
		else if( this.payloadContentType == "application/javascript" )
			return false;
		else if( this.payloadContentType == "application/x-javascript" )
			return false;
		else if( this.payloadContentType == "application/json" )
			return false;
		else if( this.payloadContentType == "application/x-json" )
			return false;
		else if( this.payloadContentType == "application/jsonp" )
			return false;
		else if( this.payloadContentType == "text/x-json" )
			return false;
		else if( this.payloadContentType == "text/json" )
			return false;
		else if( this.payloadContentType == "text/javascript" )
			return false;
		else
			return true;
	}
	
	public byte[] getAuthData()
	{
		if(this.isBinaryPayload())
		{
			byte[] result = (getClient() + getClientString() + getSystem() + getSubSystem() + getRealm() + getTags().toCombinedShortString() + getMessageType() + getPayloadContentType()).getBytes();
			return Util.mergeByteArrays(result, getPayloadBytes());
		}
		else
		{
			return (getClient() + getClientString() + getSystem() + getSubSystem() + getRealm() + getTags().toCombinedShortString() + getMessageType() + getPayloadContentType() + getPayloadString()).getBytes();
		}
	
	}
	
	public HttpEntity createEntity(String authToken, Logger logger) throws IOException
	{
		if(this.isBinaryPayload())
		{
			MultipartEntity entity = new MultipartEntity();
			entity.addPart("CLIENT", new StringBody(getClient()));
			entity.addPart("CLIENT-STRING", new StringBody(getClientString()));
			entity.addPart("MESSAGE-TYPE", new StringBody(getMessageType()));
			entity.addPart("SYSTEM", new StringBody(getSystem()));
			entity.addPart("SUB-SYSTEM", new StringBody(getSubSystem()));
			entity.addPart("REALM", new StringBody(getRealm()));			
			entity.addPart("AUTHORIZATION", new StringBody(authToken));
			entity.addPart("PAYLOAD-CONTENT-TYPE", new StringBody(getPayloadContentType()));
			entity.addPart("TAGS", new StringBody(getTags().toCombinedShortString()));
			entity.addPart("PAYLOAD", new KnownSizeInputStreamBody(new ByteArrayInputStream(getPayloadBytes()),getPayloadBytes().length,"application/octet-stream","payload"));
			
			logger.log("IsChunked: " + entity.isChunked());
			return entity;
		}
		else
		{
			List<NameValuePair> nvps = new ArrayList<NameValuePair>();
			nvps.add(new BasicNameValuePair("CLIENT", getClient()));
			nvps.add(new BasicNameValuePair("CLIENT-STRING", getClientString()));
			nvps.add(new BasicNameValuePair("MESSAGE-TYPE", getMessageType()));
			nvps.add(new BasicNameValuePair("SYSTEM", getSystem()));
			nvps.add(new BasicNameValuePair("SUB-SYSTEM", getSubSystem()));
			nvps.add(new BasicNameValuePair("REALM", getRealm()));
			nvps.add(new BasicNameValuePair("AUTHORIZATION", authToken));
			nvps.add(new BasicNameValuePair("PAYLOAD", getPayloadString()));
			nvps.add(new BasicNameValuePair("PAYLOAD-CONTENT-TYPE", getPayloadContentType()));
			nvps.add(new BasicNameValuePair("TAGS", getTags().toCombinedShortString()));
			
			UrlEncodedFormEntity entity = new UrlEncodedFormEntity(nvps, HTTP.ASCII);
			
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			entity.writeTo(baos);
			logger.log("Url-Encoded Body: " + new String(baos.toByteArray()));
			
			return entity;
		}
	}
}
