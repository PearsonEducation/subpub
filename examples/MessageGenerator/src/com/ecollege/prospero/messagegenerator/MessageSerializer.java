package com.ecollege.prospero.messagegenerator;

import java.io.UnsupportedEncodingException;

import com.thoughtworks.xstream.XStream;

public class MessageSerializer {
	private static XStream xstream = new XStream();
	
	public static byte[] serialize(Object o) throws UnsupportedEncodingException
	{
		return xstream.toXML(o).getBytes("UTF-8");
	}
}
