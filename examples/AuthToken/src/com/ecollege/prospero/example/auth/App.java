package com.ecollege.prospero.example.auth;

import java.io.UnsupportedEncodingException;

import org.bouncycastle.crypto.engines.AESFastEngine;
import org.bouncycastle.crypto.macs.CMac;
import org.bouncycastle.crypto.params.KeyParameter;
import org.bouncycastle.util.encoders.Hex;

public class App {
	public static void main(String[] args) throws UnsupportedEncodingException
	{
        byte[] data = "2010-08-13T23:15:14+0000Hello".getBytes();
        byte[] key = "1234567890123456".getBytes();

        byte[] output = new byte[16];

        CMac macProvider = new CMac( new AESFastEngine(), 128 );
        macProvider.init( new KeyParameter(key) );
        macProvider.update( data, 0, data.length );
        macProvider.doFinal( output, 0 );
        
        String result = new String( Hex.encode( output ) );
        System.out.println("Result: " + result); //We expect the result to be "aa46c65f0b075b634f02f34c9799d55c"
	}
}
