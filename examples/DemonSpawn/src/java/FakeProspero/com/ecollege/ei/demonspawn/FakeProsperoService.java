package com.ecollege.ei.demonspawn;

import static com.strategicgains.restexpress.RestExpress.JSON_FORMAT;
import static com.strategicgains.restexpress.RestExpress.XML_FORMAT;

import java.io.File;
import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executors;

import org.apache.log4j.Logger;
import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;
import org.jboss.netty.handler.codec.http.HttpMethod;

import com.strategicgains.restexpress.domain.Link;
import com.strategicgains.restexpress.pipeline.DefaultRequestHandler;
import com.strategicgains.restexpress.pipeline.PipelineBuilder;
import com.strategicgains.restexpress.route.RouteMapping;
import com.strategicgains.restexpress.route.RouteResolver;
import com.strategicgains.restexpress.serialization.DefaultSerializationResolver;
import com.strategicgains.restexpress.serialization.SerializationProcessor;
import com.strategicgains.restexpress.serialization.json.DefaultJsonProcessor;
import com.strategicgains.restexpress.serialization.xml.DefaultXmlProcessor;
import com.strategicgains.restexpress.util.Resolver;
import com.thoughtworks.xstream.XStream;

public class FakeProsperoService {
	static Logger logger = Logger.getLogger(FakeProsperoService.class);
	
	public static String URL_PATH;
	public static String MESSAGE_STORE_PATH;

	/**
	 * @param args
	 * @throws Exception 
	 */
	public static void main(String[] args) throws Exception {
		if(args.length < 1 || args[0] == null || args[0] == "")
			throw new Exception("The HTTP Server Port must be supplied as the first argument");
		if(args.length < 2 || args[1] == null || args[1] == "")
			throw new Exception("The message URL path must be supplied as the second argument");
		if(args.length < 3 || args[2] == null || args[2] == "")
			throw new Exception("The message storage path must be supplied as the third argument");

		int port = Integer.parseInt(args[0]);
		URL_PATH = args[1];
		MESSAGE_STORE_PATH = args[2];
		
		File tmp = new File(MESSAGE_STORE_PATH);
		if( !tmp.exists() )
			throw new Exception("The message store path '" + tmp.getCanonicalPath() + "' does not exist");
		if( !tmp.isDirectory() )
			throw new Exception("The message store path '" + tmp.getCanonicalPath() + "' exists but is not a directory");
		
		// Configure the server.
		ServerBootstrap bootstrap = new ServerBootstrap
		(new NioServerSocketChannelFactory
				(Executors.newCachedThreadPool(),
						Executors.newCachedThreadPool()));

		// Set up the event pipeline factory.
		DefaultRequestHandler requestHandler = 
			new DefaultRequestHandler(new RouteResolver(createRouteMapping()), createSerializationResolver());
		// Add pre/post processors to the request handler here...
		//	    requestHandler.addPreprocessor(handler);
		//	    requestHandler.addPostprocessor(handler);

		PipelineBuilder pf = new PipelineBuilder()
		.setRequestHandler(requestHandler);
		bootstrap.setPipelineFactory(pf);

		// Bind and start to accept incoming connections.
		logger.debug("Starting event consumer service on port " + port);
		bootstrap.bind(new InetSocketAddress(port));
	}

	private static Resolver<SerializationProcessor> 
	createSerializationResolver() {
		Map<String, SerializationProcessor> serializationProcessors = 
			new HashMap<String, SerializationProcessor>();
		serializationProcessors.put(JSON_FORMAT, new DefaultJsonProcessor());

		serializationProcessors.put(XML_FORMAT, 
				new DefaultXmlProcessor(createXStream()));
		return new DefaultSerializationResolver(serializationProcessors, 
				JSON_FORMAT);
	}
	
	private static RouteMapping createRouteMapping()
	{
		return new RouteMapping() {
			MessageSink sink = new MessageSink(FakeProsperoService.MESSAGE_STORE_PATH);
			
			@Override
			protected void defineRoutes() {
				uri(FakeProsperoService.URL_PATH, sink).method(HttpMethod.POST).noSerialization();
			}
		};
	}

	private static XStream createXStream() {
		XStream xstream = new XStream();
		xstream.alias("link", Link.class);
		xstream.alias("list", Collections.EMPTY_LIST.getClass());
		return xstream;
	}
}
