package com.ecollege.prospero.wsdl.delivery;


public interface DeliveryService {
	
	//DeliveryResponse deliver(Delivery delivery);
	DeliveryResponse deliver(String messageType, String messageId, String attemptId, String authenticationToken, String authenticationDelimiter, String payloadContentType, byte[] payload);
}
