package ch.uzh.ifi.attempto.owl;

import static org.junit.Assert.*;

import org.junit.Test;

public class VerbalizerWebserviceTest {

	private static final String OWL_TO_ACE_WS_URL = "http://attempto.ifi.uzh.ch/service/owl_verbalizer/owl_to_ace";
	private static final String OWL_TO_ACE_WS_URL_LOCALHOST = "http://localhost:8001";

	private static final String IN = "<?xml version=\"1.0\"?>\n" +
	"<Ontology\n" +
	"	xmlns=\"http://www.w3.org/2002/07/owl#\"\n" +
	"	xml:base=\"http://org.semanticweb.ontologies/ont\"\n" +
	"	xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">\n" +
	"	<Prefix name=\"story\" IRI=\"http://www.example.org/story.owl#\"/>\n" +
	"	<SubClassOf>\n" +
	"		<Class abbreviatedIRI=\"story:man\"/>\n" +
	"		<Class abbreviatedIRI=\"story:human\"/>\n" +
	"	</SubClassOf>\n" +
	"</Ontology>\n";

	private static final String OUT = "Every man is a human.";

	@Test
	public final void testCall() {
		VerbalizerWebservice verbalizer = new VerbalizerWebservice(OWL_TO_ACE_WS_URL);
		String response = verbalizer.call(IN);
		assertEquals(OUT, response.trim());
	}

	@Test
	public final void testCall1() {
		VerbalizerWebservice verbalizer = new VerbalizerWebservice(OWL_TO_ACE_WS_URL_LOCALHOST);
		String response = verbalizer.call(IN);
		assertEquals(OUT, response.trim());
	}
}