---+ OWL verbalizer

Author: Kaarel Kaljurand

Version: 2011-06-13


---++ Introduction

The OWL verbalizer takes its input in OWL 2 XML
and produces an output in a fragment of Attempto Controlled English (ACE).
This fragment is described in Kaarel Kaljurand's PhD thesis as ACE2
(see <http://attempto.ifi.uzh.ch/site/pubs/papers/phd_kaljurand.pdf>).

Note that the input must be in OWL 2 XML (<http://www.w3.org/TR/owl2-xml-serialization/>).
No RDF-based format is supported as input.
You can convert OWL RDF/XML into OWL 2 XML using the OWL-API v3
(<http://owlapi.sourceforge.net/>), e.g.
via the online tool <http://owl.cs.manchester.ac.uk/converter/>
or via Protege 4.1 (<http://protege.stanford.edu/>).


---++ Example

---+++ Input

==
<SubClassOf>
	<Class IRI="#man"/>
	<ObjectSomeValuesFrom>
		<ObjectProperty IRI="#know"/>
		<ObjectComplementOf>
			<ObjectOneOf>
				<NamedIndividual IRI="#Mary"/>
			</ObjectOneOf>
		</ObjectComplementOf>
	</ObjectSomeValuesFrom>
</SubClassOf>
==

---+++ Output

In ace-format:

==
Every man knows something that is not Mary.
==

In csv-format:

==
f	Every
cn_sg	#man
tv_sg	#know
f	a
f	thing
f	that
f	is
f	not
pn_sg	#Mary
f	.
==

---++ Dependencies

The OWL verbalizer depends on a reasonably recent version of SWI-Prolog
(<http://www.swi-prolog.org/>), e.g. 5.10 or higher.
SWI-Prolog must be installed together with the packages
=|clib|=, =|sgml|=, and =|http|=.

---++ Compiling the OWL verbalizer command-line client

First, make sure that the SWI-Prolog executable is on the PATH,
i.e. that you can execute `swipl' in any directory.
(Note: the executable of older versions of SWI-Prolog is called `plcon' on Windows).

In order to compile the OWL verbalizer executable, execute:

==
swipl -O -f owl_to_ace.pl -g "qsave_program('owl_to_ace.exe', [goal(main), toplevel(halt), local(25600), global(25600)])." -t halt
==

or, alternatively, just click on =|make_exe.bat|= (on Windows) or
type =|sh make_exe.sh|= (on Unix/Linux/Mac).
As a result, an EXE-file (the command-line client) is created.


---++ Using the OWL verbalizer command-line client

The command-line client takes one obligatory argument: the name
of the OWL 2 XML file to be verbalized.
In the following example, we are using
=|examples/example.owl|= as input.

==
./owl_to_ace.exe -xml examples/example.owl
==

The output (i.e. the resulting ACE text) is printed to standard output.

Note that this command can be also executed as:

==
swipl -x owl_to_ace.exe -- -xml examples/example.owl
==

This you can use on computers where the path to SWI-Prolog is different than the
one that you used during building.

If executing the EXE-file results in an "out of stack" error, then increase the stack sizes via
options local/1 and global/1, and recompile.


---++ Using the OWL verbalizer webservice

The OWL verbalizer webservice is launched on the command-line, e.g.:

==
./owl_to_ace.exe -httpserver -port 5123 -workers 2
==

A good way to start the service on a Unix command-line is:

==
nohup swipl -x owl_to_ace.exe -- -httpserver -port 5123 -workers 2 > stdout.txt 2> stderr.txt &
==

In this case, the service is started in the background,
a start up message is stored in =|stdout.txt|=, and possible error messages in =|stderr.txt|=.
On Mac OS X, one could use _|launchctl|_ instead.

As a result, a webserver (SWI HTTP server) is started on port 5123 with 2 worker threads.
The optimal number of workers depends on the number of CPUs.

In order to verbalize an ontology,
post the OWL 2 XML document to the server via the argument `xml'.
Following are some examples of using 'curl' to achieve this.

==
curl -F "xml=@examples/example.owl" http://localhost:5123

curl -F "xml=<examples/example.owl" http://localhost:5123

cat examples/example.owl | curl -F "xml=<-" http://localhost:5123

curl 'http://owl.cs.manchester.ac.uk/repository/download?ontology=http://www.co-ode.org/ontologies/pizza/pizza.owl&format=OWL/XML' | curl -F "xml=<-" http://localhost:5123
==

Posting from an HTML page: look at the source code of <docs/owl_to_ace.html>.

Posting from Java: use the OWL verbalizer Java interface (in directory java/).


---++ How are OWL entity IRIs verbalized?

---+++ Using the lexicon

It is possible to specify the mapping of IRIs to surface forms
using the following annotation properties:

==
* http://attempto.ifi.uzh.ch/ace_lexicon#PN_sg
* http://attempto.ifi.uzh.ch/ace_lexicon#CN_sg
* http://attempto.ifi.uzh.ch/ace_lexicon#CN_pl
* http://attempto.ifi.uzh.ch/ace_lexicon#TV_sg
* http://attempto.ifi.uzh.ch/ace_lexicon#TV_pl
* http://attempto.ifi.uzh.ch/ace_lexicon#TV_vbg
==

which stand for

==
* singular form of a proper name (e.g. `John')
* singular form of a common noun (`man')
* plural form of a common noun (`men')
* singular form of a transitive verb (`mans')
* plural form of a transitive verb (`man')
* past participle form a transitive verb (`manned')
==

The following axioms state that if the IRI "#man" is used as a plural common noun,
then the wordform `men' must be used by the verbalizer. If, however, it
is used as a singular transitive verb, then `mans' must be used.

==
<AnnotationAssertion>
	<AnnotationProperty IRI="http://attempto.ifi.uzh.ch/ace_lexicon#CN_pl"/>
	<IRI>#man</IRI>
	<Literal datatypeIRI="&xsd;string">men</Literal>
</AnnotationAssertion>

<AnnotationAssertion>
	<AnnotationProperty IRI="http://attempto.ifi.uzh.ch/ace_lexicon#TV_sg"/>
	<IRI>#man</IRI>
	<Literal datatypeIRI="&xsd;string">mans</Literal>
</AnnotationAssertion>
==

For example, these axioms support the generation of the sentence ``John mans at most 3 men.''
from an axiom that uses the IRI "#man" via punning once as an object property name,
and once as a class name.

If the mapping of an IRI is missing then its fragment is used in the output.
The fragment is the part that comes after '#' or the last '/'.
Note that this means that different IRIs are not necessarily verbalized as different.

---+++ Not using the lexicon (and leaving the IRIs as they are)

If the output-mode is "csv", then each ACE token is output on a separate line
and morphological mappings are left to the user to be applied. An example of
csv-output is:

==
pn_sg	http://www.example.org/story.owl#John
f	is
f	a
cn_sg	http://www.example.org/story.owl#man
f	.

f	Every
f	thing
f	that
f	is
f	a
cn_sg	http://www.example.org/story.owl#apple
f	or
f	that
f	is
f	a
cn_sg	http://www.example.org/story.owl#leaf
f	is
f	a
cn_sg	http://www.example.org/story.owl#food
f	.
==

The columns are tab-separated. The first column specifies the type of the token
(f = function word, qs = quoted string, comment = comment, cn_sg = singular common noun, ...),
and the 2nd column contains ACE tokens and OWL IRIs. Empty line denotes axiom
borders (each OWL axiom in the input ontology is verbalized by 0 or more ACE
sentences).

In this mode the AnnotationAssertions are ignored and IRI fragments are not generated.
As a result this mode is about 3x faster.


---++ API-level access to the OWL verbalizer

The API-level access to the OWL verbalizer is only documented with SWI-Prolog's PlDoc.
Provided that you have PlDoc installed (SWI-Prolog package 'pldoc'),
you can view the documentation by:

==
?- doc_server(8000).
?- [owl_to_ace].
?- doc_browser.
==

The main entry point to the verbalizer is the module axiom_sentencelist.pl
which converts an input OWL axiom
(Prolog term in OWL FSS) to a list of ACE sentences (where each sentence is a Prolog
list of ACE tokens, where each token is a Prolog atom, number, or unary ground term).
This module calls three further rules from three separate modules:
table_1/2 rewrites axioms (except for SubClassOf-, SubPropertyOf-, and DisjointProperties-axioms);
rewrite_subclassof/2 rewrites SubClassOf-axioms; and
owl_ace/2 verbalizes the rewritten axioms with a Definite Clause Grammar.


---++ Known issues

See: http://code.google.com/p/owlverbalizer/issues/list
