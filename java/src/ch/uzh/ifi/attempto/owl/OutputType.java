package ch.uzh.ifi.attempto.owl;

/**
 * <p>This enumeration lists all the possible output types of the OWL verbalizer.</p>
 * 
 * @author Kaarel Kaljurand
 */
public enum OutputType {

	/**
	 * <p>ACE snippets each corresponding to an input axiom, separated by
	 * an empty line. Plain text.</p>
	 */
	ACE,

	/**
	 * <p>HTML table that maps input axioms to snippets.</p>
	 */
	HTML,

	/**
	 * <p>Two columns (morph. type and ACE token), tab separated, each morph. type
	 * ACE token pair on a separate line, empty line denotes snippet border. Content
	 * words are represented by input IRIs.</p>
	 */
	CSV;
}