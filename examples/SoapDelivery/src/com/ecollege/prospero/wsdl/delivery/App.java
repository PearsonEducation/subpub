package com.ecollege.prospero.wsdl.delivery;

import javax.xml.ws.Endpoint;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

public class App {
	private static void printHelp(Options options) {
		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp( "java com.ecollege.prospero.wsdl.delivery.App", options );		
	}

	public static void main(String[] args) throws ParseException
	{
    	Options options = new Options();
    	options.addOption("mh", true, "MySQL host name");
    	options.addOption("n", true, "Instance Name");

    	CommandLineParser parser = new PosixParser();
    	CommandLine cmdLine = parser.parse(options, args);
    	
    	if( cmdLine.hasOption("help") )
    	{
    		printHelp(options);
    		System.exit(0);
    	}
    	
    	String mysqlHost = cmdLine.getOptionValue("mh", "localhost");
		String mysqlConnString = "jdbc:mysql://" + mysqlHost + "/prosperlang_test?user=root&password=password";

		DeliveryServicePortTypeImpl impl = new DeliveryServicePortTypeImpl(mysqlConnString, cmdLine.getOptionValue("n", "soapdelivery"));
		String address = "http://localhost:9001/deliver";
		Endpoint.publish(address, impl);
	}
}
