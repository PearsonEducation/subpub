package com.ecollege.prospero.messagegenerator;

public class ProsperoPrincipal {
	private String prosperoHost;
	private int prosperoPort;
	private String key;
	private String principalId;
	
	public String getProsperoHost() {
		return prosperoHost;
	}
	
	public void setProsperoHost(String prosperoHost) {
		this.prosperoHost = prosperoHost;
	}
	
	public int getProsperoPort() {
		return prosperoPort;
	}
	
	public void setProsperoPort(int prosperoPort) {
		this.prosperoPort = prosperoPort;
	}
	
	public String getKey() {
		return key;
	}
	
	public void setKey(String key) {
		this.key = key;
	}
	
	public String getPrincipalId() {
		return principalId;
	}
	
	public void setPrincipalId(String principalId) {
		this.principalId = principalId;
	}
}
