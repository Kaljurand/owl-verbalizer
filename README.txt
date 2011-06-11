---+ OWL verbalizer

Author: Kaarel Kaljurand

Version: 2011-06-11


---++ Introduction

The OWL verbalizer takes its input in OWL 2 XML
and produces an output in a fragment of Attempto Controlled English (ACE).
This fragment is described in Kaarel Kaljurand's PhD thesis as ACE2
(see <http://attempto.ifi.uzh.ch/site/pubs/papers/phd_kaljurand.pdf>).

Note that the input must be in OWL 2 XML (<http://www.w3.org/TR/owl2-xml-serialization/>).
No RDF-based format is supported as input.
You can convert OWL RDF/XML into OWL 2 XML using the OWL-API v3
(<http://owlapi.sourceforge.net/>), e.g.

* via the online tool <http://owl.cs.manchester.ac.uk/converter/>
* via Protege 4.1 (<http://protege.stanford.edu/>)


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
SWI-Prolog must be installed together with the following packages:

* clib
* sgml
* http

---++ Compiling the OWL verbalizer command-line client

First, make sure that the SWI-Prolog executable is on the PATH,
i.e. that you can execute `swipl' in any directory.
(Note: the executable of older versions of SWI-Prolog is called `plcon' on Windows).

In order to compile the OWL verbalizer executable, proceed as follows.

On Unix/Mac, execute

==
swipl -O -F none -g "[owl_to_ace], qsave_program('owl_to_ace.exe', [goal(main), toplevel(halt), local(25600), global(25600)])." -t halt
==

Alternatively, given that you have `bash' installed, you can just execute
the following command.

==
sh make_exe.sh
==

On Windows, execute:

==
plcon -O -f owl_to_ace.pl -g "qsave_program('owl_to_ace.exe', [goal(main), toplevel(halt), local(25600), global(25600)])." -t halt
==

or, alternatively, just click on =|make_exe.bat|=.
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


---++ Using OWL verbalizer webservice

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

Posting from Java: use the Attempto Java Packages
(see e.g. <http://attempto.ifi.uzh.ch/site/docs/java/ch/uzh/ifi/attempto/owl/VerbalizerWebservice.html>).


---++ API-level access to the OWL verbalizer

The API-level access to the OWL verbalizer is only documented with SWI-Prolog's PlDoc.
The entry point is the module =|owlfss_acetext.pl|= which exports the rule owlfss_acetext/2.
This rule expects an OWL ontology (as a Prolog term) as input and returns a (Prolog) list of
Axiom-SentenceList pairs as output, where Axiom is an OWL axiom and SentenceList is a list
of corresponding ACE sentences, where each sentence is a list of tokens.
This module calls three further rules from three separate modules.

* table_1/2: rewrites axioms (except for SubClassOf-, SubPropertyOf-, and DisjointProperties-axioms)

* rewrite_subclassof/2: rewrites SubClassOf-axioms

* owl_ace/2: verbalizes the rewritten axioms with a Definite Clause Grammar

---++ Using the lexicon

It is possible to specify the mapping of IRIs to surface forms
using the following annotation properties:

* http://attempto.ifi.uzh.ch/ace_lexicon#PN_sg
* http://attempto.ifi.uzh.ch/ace_lexicon#CN_sg
* http://attempto.ifi.uzh.ch/ace_lexicon#CN_pl
* http://attempto.ifi.uzh.ch/ace_lexicon#TV_sg
* http://attempto.ifi.uzh.ch/ace_lexicon#TV_pl
* http://attempto.ifi.uzh.ch/ace_lexicon#TV_vbg

which stand for

* singular form of a proper name (e.g. `John')
* singular form of a common noun (`man')
* plural form of a common noun (`men')
* singular form of a transitive verb (`mans')
* plural form of a transitive verb (`man')
* past participle form a transitive verb (`manned')

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

---++ Leaving the IRIs as they are

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

In this mode the AnnotationAssertions are ignored and fragments are not generated.
As a result this mode is about 3x faster.


---++ Files

---+++ Prolog modules

* owl_to_ace.pl: command-line client + HTTP-interface to the OWL verbalizer
* owlxml_owlfss.pl: converts OWL 2 XML into OWL 2 Functional-Style Syntax (in Prolog notation), which is used by all the following modules
* owlfss_acetext.pl: main interface to the verbalizer
* table_1.pl: axiom rewriting
* rewrite_subclassof.pl: SubClassOf-axiom rewriting
* owlace_dcg.pl: ACE2 grammar in DCG (used to generate ACE sentences)
* output_results.pl: different ways to output the results (ace, csv, html)
* ace_niceace.pl: some post-processing of the verbalization (not used in csv-mode)
* lexicon.pl: lexicon lookup + generation on the basis of AnnotationAssertion-axioms (not used in the csv-mode)


---+++ Other

* examples/: some examples of OWL 2 XML files
* docs/: some docs and demos


---++ Issues

See: http://code.google.com/p/owlverbalizer/issues/list

---+++ Issues not yet migrated to Google Code

* Some OWL 2 logical constructs are not supported, e.g.
some data property constructs.

* Axioms that contain annotations are rejected.

* All white-space in the input XML-file is normalized,
e.g. newlines inside annotations will be mapped to spaces.

* The OWL 2 XML processor has been tested only on the files generated by Protege 4.1.

* The input OWL 2 XML cannot have elements/attributes from other namespaces than OWL.

* Very complex class descriptions are rejected, i.e. they are not verbalized.

* DisjointClasses and other similar list/set constructs could be handled in a shorter way.

* Only the logical content of an OWL ontology is verbalized,
and not the information about annotations, versioning, import-structure, etc.

* Check if the verbalizer and its webservice are Unicode-aware.

* Rewrite: (R some C) and (R only C) --> (R some Thing) and (R only C)
	-- This should be semantics preserving
	-- This would simplify the structure of the axiom in case C is complex
	-- This seems to violate a Gricean maxim

* dog and cat = Nothing  -->  dog -> not cat

* Test ordering of OneOf

* Think: does it make sense to push negation into UnionOf or IntersectionOf

* OneOf wrapped in UnionOf creates complex class which could be simplified

* Think about the wrapping of complex classes (don't wrap OneOf, because it's just like an atomic noun?)

* Is this a problem?
==
Everything is an a.  # function word used as content word
==

* NamedIndividual should not be verbalized into `itself' or `themselves', because
in some positions it would not be an ACE proper name but an anaphoric reference.
(e.g.: Nothing sees itself.)
