package com.ecollege.prospero.messagegenerator.logger;

public class SystemOutLogger implements Logger {

	@Override
	public void log(String message) {
		System.out.println(message);
	}

	@Override
	public void log(Exception e) {
		e.printStackTrace(System.out);
	}

}
