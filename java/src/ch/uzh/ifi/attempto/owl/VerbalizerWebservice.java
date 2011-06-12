package ch.uzh.ifi.attempto.owl;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

/**
 * This is a simple interface to the OWL verbalizer.
 * OWL verbalizer is a tool that translates an OWL 2 ontology into an ACE text.
 * It expects the ontology to be represented in the OWL 2 XML serialization syntax, see
 * 
 * <blockquote>
 * <a href="http://www.w3.org/TR/owl-xml-serialization/">http://www.w3.org/TR/owl-xml-serialization/</a>
 * </blockquote>
 * 
 * The OWL verbalizer accepts one parameter, <code>xml</code>, the value of which
 * is the complete ontology as string.
 * The OWL verbalizer returns the ACE representation of the ontology as plain text (string).
 * In case the verbalizer fails to process an OWL axiom in the ontology,
 * an error message is returned between ACE comment symbols.
 * 
 * @author Kaarel Kaljurand
 *
 */
public class VerbalizerWebservice {

	private static final int MAX_HTTP_GET_LENGTH = 1000;
	private static final String ERROR_MESSAGE = "Accessing OWL->ACE webservice failed";

	private final String wsUrl;

	/**
	 * Constructs a new <code>VerbalizerWebservice</code> object
	 * on the basis of the URL of the OWL verbalizer webservice.
	 * 
	 * @param wsUrl The URL of the OWL verbalizer webservice.
	 */
	public VerbalizerWebservice(String wsUrl) {
		this.wsUrl = wsUrl;
	}

	/**
	 * Calls the OWL verbalizer webservice by giving the string representation of
	 * an XML-formatted ontology as input. Returns the corresponding ACE text
	 * with possible error messages as comments.
	 * 
	 * @param xml XML-representation of an OWL ontology.
	 * @return ACE text that corresponds to the ontology.
	 */
	public String call(String xml) {
		List <NameValuePair> nvps = new ArrayList <NameValuePair>();
		nvps.add(new BasicNameValuePair("xml", xml));
		return getResponseAsString(nvps);
	}

	private String getResponseAsString(List<NameValuePair> nvps) {
		DefaultHttpClient httpclient = new DefaultHttpClient();
		HttpUriRequest request = getHttpUriRequest(nvps);
		return getEntity(httpclient, request);
	}

	/**
	 * We create an HTTP GET query from the given parameters. If it turns out to be
	 * too long (which we expect to happen very infrequently) then we fall back to creating
	 * HTTP POST.
	 * 
	 * @param nvps List of name-value pairs
	 * @return HTTP request (either GET or POST)
	 */
	private HttpUriRequest getHttpUriRequest(List<NameValuePair> nvps) {
		String getQuery = wsUrl + "?" + URLEncodedUtils.format(nvps, HTTP.UTF_8);
		if (getQuery.length() > MAX_HTTP_GET_LENGTH) {
			HttpPost httppost = new HttpPost(wsUrl);
			try {
				httppost.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8));
			} catch (UnsupportedEncodingException e) {
				// BUG: Assuming that this cannot happen
			}
			return httppost;
		}
		return new HttpGet(getQuery);
	}

	private String getEntity(DefaultHttpClient httpclient, HttpUriRequest httpRequest) {
		try {
			HttpResponse response = httpclient.execute(httpRequest);
			HttpEntity entity = response.getEntity();

			if (entity == null) {
				throw new RuntimeException(ERROR_MESSAGE + ": " + response.getStatusLine());
			}
			int statusCode = response.getStatusLine().getStatusCode();
			if (statusCode != HttpStatus.SC_OK) {
				throw new RuntimeException(ERROR_MESSAGE + ": " + response.getStatusLine());
			}
			// The APE webservice returns the data in UTF8, even if it doesn't declare it.
			if (entity.getContentEncoding() == null) {
				return EntityUtils.toString(entity, HTTP.UTF_8);
			}
			return EntityUtils.toString(entity);

		} catch (ClientProtocolException e) {
			throw new RuntimeException(ERROR_MESSAGE + ": " + e.getMessage());
		} catch (IOException e) {
			throw new RuntimeException(ERROR_MESSAGE + ": " + e.getMessage());
		} finally {
			httpclient.getConnectionManager().shutdown();
		}
	}
}
