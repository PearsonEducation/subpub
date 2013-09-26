
package com.ecollege.prospero.wsdl.delivery;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="messageType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="messageId" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="attemptId" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="authenticationToken" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="authenticationDelimiter" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="payload" type="{http://www.w3.org/2001/XMLSchema}base64Binary"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "messageType",
    "messageId",
    "attemptId",
    "authenticationToken",
    "authenticationDelimiter",
    "payload"
})
@XmlRootElement(name = "deliver")
public class Deliver {

    @XmlElement(required = true)
    protected String messageType;
    @XmlElement(required = true)
    protected String messageId;
    @XmlElement(required = true)
    protected String attemptId;
    @XmlElement(required = true)
    protected String authenticationToken;
    @XmlElement(required = true)
    protected String authenticationDelimiter;
    @XmlElement(required = true)
    protected byte[] payload;

    /**
     * Gets the value of the messageType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMessageType() {
        return messageType;
    }

    /**
     * Sets the value of the messageType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMessageType(String value) {
        this.messageType = value;
    }

    /**
     * Gets the value of the messageId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMessageId() {
        return messageId;
    }

    /**
     * Sets the value of the messageId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMessageId(String value) {
        this.messageId = value;
    }

    /**
     * Gets the value of the attemptId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAttemptId() {
        return attemptId;
    }

    /**
     * Sets the value of the attemptId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAttemptId(String value) {
        this.attemptId = value;
    }

    /**
     * Gets the value of the authenticationToken property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAuthenticationToken() {
        return authenticationToken;
    }

    /**
     * Sets the value of the authenticationToken property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAuthenticationToken(String value) {
        this.authenticationToken = value;
    }

    /**
     * Gets the value of the authenticationDelimiter property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAuthenticationDelimiter() {
        return authenticationDelimiter;
    }

    /**
     * Sets the value of the authenticationDelimiter property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAuthenticationDelimiter(String value) {
        this.authenticationDelimiter = value;
    }

    /**
     * Gets the value of the payload property.
     * 
     * @return
     *     possible object is
     *     byte[]
     */
    public byte[] getPayload() {
        return payload;
    }

    /**
     * Sets the value of the payload property.
     * 
     * @param value
     *     allowed object is
     *     byte[]
     */
    public void setPayload(byte[] value) {
        this.payload = ((byte[]) value);
    }

}
