package com.ecollege.prospero.simulator;

import java.util.HashMap;

public class MessageProperties {
	private HashMap<String, String> props = new HashMap<String, String>();
	
	public void put(String key, String value)
	{
		props.put(key, value);
	}
	
	public String asJSON(String rootName)
	{
		StringBuilder b = new StringBuilder();
		
		b.append("{\"" + rootName + "\": {");
		
		String delim = "";
		for(String key: props.keySet()) {
			b.append(delim + "\"" + key + "\":\"" + props.get(key) + "\"");
			delim = ",";
		}
		
		b.append("}}");
		
		return b.toString();
	}
	
}
