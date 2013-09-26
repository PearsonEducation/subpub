package com.ecollege.prospero.wsdl.delivery;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Date;

import javax.jws.WebService;

import com.ecollege.prospero.wsdl.delivery.xsd.DeliveryResponse;

@WebService(targetNamespace = "http://delivery.wsdl.prospero.ecollege.com", endpointInterface = "com.ecollege.prospero.wsdl.delivery.DeliveryServicePortType", serviceName = "DeliveryService")
public class DeliveryServicePortTypeImpl implements DeliveryServicePortType {
	private final String mysqlConnString;
	private final String name;
	
	public DeliveryServicePortTypeImpl(String mysqlConnString, String name)
	{
		super();
		this.mysqlConnString = mysqlConnString;
		this.name = name;
	}
	
	@Override
	public DeliveryResponse deliver(
			String messageType, 
			String messageId,
			String attemptId, 
			String authenticationToken,
			String authenticationDelimiter, 
			byte[] payload
	) throws Exception {
		Connection conn = null;
		Statement stmt = null;
		ResultSet rs = null;
		try
		{
			String sql = "INSERT INTO delivery(" +
				"del_attempt_id, " +
				"date_delivered, " +
				"message_type, " +
				"messge_id, " +
				"authorization, " +
				"payload, " +
				"tester) " +
				"VALUES(" +
				"'" + attemptId + "'," +
				"'" + (new Date()).getTime() / 1000 + "'," +
				"'" + messageType + "'," +
				"'" + messageId + "'," +
				"'" + authenticationToken + "'," +
				"'" + new String(payload) + "'," +
				"'" + name + "')";
			conn = DriverManager.getConnection(mysqlConnString);
			stmt = conn.createStatement();
			stmt.execute(sql);
			
			int autoIncKeyFromFunc = -1;
			rs = stmt.executeQuery("SELECT LAST_INSERT_ID()");

			if (rs.next()) {
				autoIncKeyFromFunc = rs.getInt(1);
			} else {
				throw new Exception("Unable to obtain LAST_INSERT_ID");
			}
			
			DeliveryResponse result = new DeliveryResponse();
			result.setConfirmationNumber(name + "::" + Integer.toString(autoIncKeyFromFunc));
			return result;
		}
		finally
		{
			if( rs != null )
				rs.close();
			if( stmt != null )
				stmt.close();
			if( conn != null )
				conn.close();
		}
	}

}
