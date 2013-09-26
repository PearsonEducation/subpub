package com.ecollege.prospero.messagegenerator;

import java.util.ArrayList;

public class ContextTagHashMap {
	private static final long serialVersionUID = -7378568901106145299L;
	
	private final ArrayList<ContextTag> inner = new ArrayList<ContextTag>();
	
	public void put(ContextTag tag)
	{
		inner.add(tag);
	}
	
	public String toCombinedShortString()
	{
		String result = "";
		
		String delimiter = "";
		for(ContextTag tag: inner)
		{
			result += delimiter + tag.toShortString();
			delimiter = ",";
		}
		
		return result;
	}
}
