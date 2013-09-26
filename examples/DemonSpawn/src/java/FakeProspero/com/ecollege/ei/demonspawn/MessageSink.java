package com.ecollege.ei.demonspawn;

import java.io.File;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.apache.log4j.Logger;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.handler.codec.http.QueryStringDecoder;
import org.jboss.netty.util.CharsetUtil;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class MessageSink {
	private String messageStorePath;
	static Logger logger = Logger.getLogger(MessageSink.class);
	
	public MessageSink(String messageStorePath) {
		this.messageStorePath = messageStorePath;
	}
	
	public Object create(Request request, Response response)
	{
		Date now = new Date();
	    ChannelBuffer content = request.getBody();
	    String postBody = content.toString(CharsetUtil.UTF_8);
	    
	    String guid = UUID.randomUUID().toString();

	    Message message = Message.fromUrlEncodedBody(postBody);
	    File file = message.writeToFile(guid);
	    
	    logger.info("Created message " + guid + " at file " + file.getCanonicalPath());

		return guid;
	}
	
	//public void writeFile(String fileName) throws FileNotFoundException
	//{
	//	File file = new File(messageStorePath + File.pathSeparator + fileName);
	//	FileChannel channel = new FileOutputStream(file).getChannel();
	//	
	//}
}
