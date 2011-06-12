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
	"	<Prefix name=\"t\" IRI=\"http://www.example.org/t.owl#\"/>\n" +
	"	<SubClassOf>\n" +
	"		<Class abbreviatedIRI=\"t:man\"/>\n" +
	"		<Class abbreviatedIRI=\"t:human\"/>\n" +
	"	</SubClassOf>\n" +
	"</Ontology>\n";

	private static final String IN_UNDEF_PREFIX = "<?xml version=\"1.0\"?>\n" +
	"<Ontology\n" +
	"	xmlns=\"http://www.w3.org/2002/07/owl#\"\n" +
	"	xml:base=\"http://org.semanticweb.ontologies/ont\"\n" +
	"	xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">\n" +
	"	<SubClassOf>\n" +
	"		<Class abbreviatedIRI=\"undef:man\"/>\n" +
	"		<Class abbreviatedIRI=\"undef:human\"/>\n" +
	"	</SubClassOf>\n" +
	"</Ontology>\n";

	private static final String OUT = "Every man is a human.";

	private static final String OUT_UNDEF_PREFIX = "<error type=\"existence_error\">existence_error(variable,undef): system:nb_getval/2: </error>";

	private static final String OUT_CSV = "" +
	"ignored\tPrefix(t,http://www.example.org/t.owl#)\n\n" +
	"f\tEvery\n" +
	"cn_sg\thttp://www.example.org/t.owl#man\n" +
	"f\tis\n" +
	"f\ta\n" +
	"cn_sg\thttp://www.example.org/t.owl#human\n" +
	"f\t.\n\n";

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

	@Test
	public final void testCall2() {
		VerbalizerWebservice verbalizer = new VerbalizerWebservice(OWL_TO_ACE_WS_URL_LOCALHOST);
		String response = verbalizer.call(IN_UNDEF_PREFIX);
		assertEquals(OUT_UNDEF_PREFIX, response.trim());
	}

	@Test
	public final void testCall3() {
		VerbalizerWebservice verbalizer = new VerbalizerWebservice(OWL_TO_ACE_WS_URL_LOCALHOST);
		String response = verbalizer.call(IN, OutputType.CSV);
		System.out.println(response);
		assertEquals(OUT_CSV, response);
	}
}