package com.ecollege.ei.demonspawn;

import java.util.List;

import org.jboss.netty.handler.codec.http.QueryStringDecoder;

public class Message {
    private String messageType;
    private String payloadContentType;
    private String rawTags;
    private String client;
    private String clientString;
    private String system;
    private String subSystem;
    private String realm;
    
    private Message(String mt, String pct, String c, String cs, String s, String ss, String r)
    {
    	messageType = mt;
    	payloadContentType = pct;
    	client = c;
    	clientString = cs;
    	system = s;
    	subSystem = ss;
    	realm = r;
    }
    
    public File writeToFile(String guid)
    {
    	
    }
    
	private static String getParam(QueryStringDecoder decoder, String param)
	{
		List<String> tmp = decoder.getParameters().get(param);
		if( tmp == null || tmp.size() == 0 )
			return null;
		return tmp.get(0);
	}
    
    public static Message fromUrlEncodedBody(String body)
    {
	    QueryStringDecoder decoder = new QueryStringDecoder("?" + body);
	    
	    String messageType = getParam(decoder, "MESSAGE-TYPE");
	    String payloadContentType = getParam(decoder, "PAYLOAD-CONTENT-TYPE");
	    //String rawTags = getParam(decoder, "TAGS"); //This is a level 2 task... :)
	    String client = getParam(decoder, "CLIENT");
	    String clientString = getParam(decoder, "CLIENT-STRING");
	    String system = getParam(decoder, "SYSTEM");
	    String subSystem = getParam(decoder, "SUB-SYSTEM");
	    String realm = getParam(decoder, "REALM");
	    
	    return new Message(messageType, payloadContentType, client, clientString, system, subSystem, realm);
    }
}
