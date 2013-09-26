package com.ecollege.prospero.messagegenerator;

import java.net.URLEncoder;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;
import org.bouncycastle.crypto.engines.AESFastEngine;
import org.bouncycastle.crypto.macs.CMac;
import org.bouncycastle.crypto.params.KeyParameter;
import org.bouncycastle.util.encoders.Hex;

import com.ecollege.prospero.messagegenerator.logger.Logger;

public class Prospero {
	private static DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
	
	private Prospero()
	{
		//Static-only class, no need to instantiate this...
	}
	
	public static String getSubscription(String subscriptionId, ProsperoPrincipal principal, Logger logger) throws Exception
	{
		df.setTimeZone(TimeZone.getTimeZone("UTC"));
		String dateTime = df.format(new Date());
		
		byte[] authData = subscriptionId.getBytes();
		logger.log("Authentication Token Data: " + new String(authData));

		String authToken = generateAuthToken(dateTime, principal.getPrincipalId(), principal.getKey(), authData);
		logger.log("Generated Auth Token: " + authToken);

		HttpClient client = new DefaultHttpClient();
		HttpGet get = new HttpGet("http://" + principal.getProsperoHost() + ":" + principal.getProsperoPort() + "/v1/subscription/" + subscriptionId);
		get.addHeader("Authorization", authToken);
		
		HttpResponse response = client.execute(get);

		if(response.getStatusLine().getStatusCode() != 200)
		{
			throw new Exception("Unexpected response HTTP status code: " + response.getStatusLine().getStatusCode());
		}

		String responseBody = null;
		HttpEntity responseEntity = response.getEntity();
		if (responseEntity != null) {
			responseBody = new String(EntityUtils.toString(responseEntity));
		}

		return responseBody;
	}
	
	public static void clearSubscriptions(ProsperoPrincipal principal, Logger logger) throws Exception
	{
		HttpClient client = new DefaultHttpClient();
		HttpPost post = new HttpPost("http://" + principal.getProsperoHost() + ":" + principal.getProsperoPort() + "/v1/test/clear_subscriptions");
		HttpResponse response = client.execute(post);

		if(response.getStatusLine().getStatusCode() != 200)
		{
			throw new Exception("Unexpected response HTTP status code: " + response.getStatusLine().getStatusCode());
		}
	}
	
	public static SubscribeResponse subscribe(SubscribeInput input, ProsperoPrincipal principal, Logger logger)
	{
		try
		{
			df.setTimeZone(TimeZone.getTimeZone("UTC"));
			String dateTime = df.format(new Date());
			
			byte[] authData = input.getAuthData();
			logger.log("Authentication Token Data: " + new String(authData));
			
			String authToken = generateAuthToken(dateTime, principal.getPrincipalId(), principal.getKey(), authData);
			logger.log("Generated Auth Token: " + authToken);
					
			HttpClient client = new DefaultHttpClient();
			HttpPost post = new HttpPost("http://" + principal.getProsperoHost() + ":" + principal.getProsperoPort() + "/v1/subscription");
			post.setEntity(input.createEntity(authToken, logger));
						
			HttpResponse response = client.execute(post);
								
			String responseBody = null;
			HttpEntity responseEntity = response.getEntity();
			if (responseEntity != null) {
				responseBody = new String(EntityUtils.toString(responseEntity));
			}
			
			if(response.getStatusLine().getStatusCode() != 200)
			{
				throw new Exception("Unexpected response HTTP status code: " + response.getStatusLine().getStatusCode() + " (Body: " + responseBody + ")");
			}
	
			//Response body is currently a JSON snippet that looks like this:  {"message": {"id": "<message-id>"}}
			Matcher matcher = Pattern.compile("^\\{\"subscription\":\\{\"id\":\"([0-9a-zA-Z\\-]{10,200})\"").matcher(responseBody);
			if(matcher.find())
			{
				return new SubscribeResponse(matcher.group(1));				
			}
			else
			{
				throw new Exception("Unable to find subscription id in response body: " + responseBody);
			}
		} catch (Exception e) {
			logger.log(e);
			return new SubscribeResponse(e);
		}
	}
	
	public static UnsubscribeResponse unsubscribe(String id, ProsperoPrincipal principal, Logger logger)
	{
		try
		{
			df.setTimeZone(TimeZone.getTimeZone("UTC"));
			String dateTime = df.format(new Date());
			
			byte[] authData = id.getBytes();
			logger.log("Authentication Token Data: " + new String(authData));
			
			String authToken = generateAuthToken(dateTime, principal.getPrincipalId(), principal.getKey(), authData);
			logger.log("Generated Auth Token: " + authToken);
			
			String safeAuthToken = URLEncoder.encode(authToken, "UTF-8");
					
			HttpClient client = new DefaultHttpClient();
			HttpDelete post = new HttpDelete("http://" + principal.getProsperoHost() + ":" + principal.getProsperoPort() + "/v1/subscription/" + id + "?Authorization=" + safeAuthToken);
			HttpResponse response = client.execute(post);
								
			String responseBody = null;
			HttpEntity responseEntity = response.getEntity();
			if (responseEntity != null) {
				responseBody = new String(EntityUtils.toString(responseEntity));
			}
			
			if(response.getStatusLine().getStatusCode() != 200)
			{
				throw new Exception("Unexpected response HTTP status code: " + response.getStatusLine().getStatusCode() + " (Body: " + responseBody + ")");
			}
	
			return new UnsubscribeResponse(id);				
		} catch (Exception e) {
			logger.log(e);
			return new UnsubscribeResponse(e);
		}		
	}
	
	public static PublishResponse publish(PublishInput input, ProsperoPrincipal principal, Logger logger)
	{
		try {
			df.setTimeZone(TimeZone.getTimeZone("UTC"));
			String dateTime = df.format(new Date());
			
			byte[] authData = input.getAuthData();
			logger.log("Authentication Token Data: " + new String(authData));
			
			String authToken = generateAuthToken(dateTime, principal.getPrincipalId(), principal.getKey(), authData);
			logger.log("Generated Auth Token: " + authToken);
			
			HttpClient client = new DefaultHttpClient();
			HttpPost post = new HttpPost("http://" + principal.getProsperoHost() + ":" + principal.getProsperoPort() + "/v1/message");
			post.setEntity(input.createEntity(authToken, logger));
						
			HttpResponse response = client.execute(post);
								
			String responseBody = null;
			HttpEntity responseEntity = response.getEntity();
			if (responseEntity != null) {
				responseBody = new String(EntityUtils.toString(responseEntity));
			}
			
			if(response.getStatusLine().getStatusCode() != 200)
			{
				throw new Exception("Unexpected response HTTP status code: " + response.getStatusLine().getStatusCode() + " (Body: " + responseBody + ")");
			}

			//Response body is currently a JSON snippet that looks like this:  {"message": {"id": "<message-id>"}}
			Matcher matcher = Pattern.compile("^\\{\"message\": \\{\"id\": \"([0-9a-zA-Z\\-]{10,200})\"\\}\\}$").matcher(responseBody);
			if(matcher.find())
			{
				return new PublishResponse(matcher.group(1));				
			}
			else
			{
				throw new Exception("Unable to find message id in response body: " + responseBody);
			}
			
		} catch (Exception e) {
			return new PublishResponse(e);
		}
	}
	
	private static String generateAuthToken(String dateTime, String principalId, String key, byte[] payload)
	{
		byte[]dateTimeBytes = dateTime.getBytes();
		byte[] macInput = Util.mergeByteArrays(dateTimeBytes, payload);
		
        byte[] macOutput = new byte[16];

        CMac macProvider = new CMac( new AESFastEngine(), 128 );
        macProvider.init( new KeyParameter(key.getBytes()) );
        macProvider.update( macInput, 0, macInput.length );
        macProvider.doFinal( macOutput, 0 );
        
        String macOutputHex = new String( Hex.encode( macOutput ) );
        
        return principalId + "|" + dateTime + "|" + macOutputHex;

	}
}
