package com.ecollege.prospero.messagegenerator;

public class ContextTag {
	private final String name;
	private final String value;
	
	public ContextTag(String name, String value) {
		super();
		this.name = name;
		this.value = value;
	}

	public String getName() {
		return name;
	}

	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "ContextTag [name=" + name + ", value=" + value + "]";
	}
	
	public String toShortString() {
		return name + ":" + value;
	}
	
}
