package com.ecollege.prospero.messagegenerator;

public class Util {
	public static byte[] mergeByteArrays(byte[] one, byte[] two)
	{
		byte[] result = new byte[one.length + two.length];
		System.arraycopy(one, 0, result, 0, one.length);
		System.arraycopy(two, 0, result, one.length, two.length);
		return result;
	}

}
