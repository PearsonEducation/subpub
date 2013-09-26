package com.ecollege.prospero.messagegenerator;

import java.io.InputStream;

import org.apache.http.entity.mime.content.InputStreamBody;

class KnownSizeInputStreamBody extends InputStreamBody {
	private int length;

	public KnownSizeInputStreamBody(final InputStream in, final int length, final String mimeType, final String filename) {
		super(in, mimeType, filename);
		this.length = length;
	}

	@Override
	public long getContentLength() {
		return this.length;
	}
}
