OWL verbalizer
==============

Introduction
------------

OWL verbalizer is a tool that converts an OWL ontology into an Attempto Controlled English (ACE) text.

It can handle complex OWL formulas such as


	man
		and (not (own some car))
		and (own some bike)
	SubClassOf inverse (likes) some ({Mary})

by turning them into natural English sentences such as:


	Every man
		that owns a bike
		and
		that does not own a car
	is liked by Mary .

This conversion is designed to be reversible, i.e. one can convert the ACE representation back into OWL so that
no loss in meaning occurs. For more of the theory and design choices behind the verbalization read section
__5.6 Verbalizing OWL in ACE__ of

> Kaarel Kaljurand. Attempto Controlled English as a Semantic Web Language.
> PhD thesis, Faculty of Mathematics and Computer Science, University of Tartu, 2007.
> <http://attempto.ifi.uzh.ch/site/pubs/papers/phd_kaljurand.pdf>

For a demo visit the [OWL verbalizer demo page](http://attempto.ifi.uzh.ch/site/docs/owl_to_ace.html).

OWL verbalizer is implemented in SWI-Prolog.
It offers a command-line front-end and can also be run as an HTTP server.
The following example demonstrates launching the server and using it to verbalize an OWL/XML
ontology from a remote repository.


	$ ./owl_to_ace.exe -httpserver -port 5123 &

	$ curl 'http://owl.cs.manchester.ac.uk/repository/download?
		ontology=http://www.co-ode.org/ontologies/pizza/pizza.owl&format=OWL/XML'
	| curl -F "xml=<-" http://localhost:5123


The resulting ACE text will appear in STDOUT.


Example
-------

The OWL verbalizer takes its input in OWL 2 XML
and produces an output in a fragment of Attempto Controlled English (ACE).

Note that the input must be in OWL 2 XML (<http://www.w3.org/TR/owl2-xml-serialization/>).
No RDF-based format is supported as input.
You can convert OWL RDF/XML into OWL 2 XML using the OWL-API v3
(<http://owlapi.sourceforge.net/>), e.g.
via the online tool <http://owl.cs.manchester.ac.uk/converter/>
or via Protege 4.1+ (<http://protege.stanford.edu/>).

### Input

    <?xml version="1.0" encoding="UTF-8"?>
    <Ontology
        xmlns="http://www.w3.org/2002/07/owl#"
        xml:base="http://www.w3.org/2002/07/owl#"
        xmlns:xml="http://www.w3.org/XML/1998/namespace">
      <Prefix name="" IRI="http://www.example.org/test#"/>
      <SubClassOf>
        <ObjectIntersectionOf>
          <Class abbreviatedIRI=":man"/>
          <ObjectSomeValuesFrom>
	    <ObjectProperty abbreviatedIRI=":own"/>
	    <Class abbreviatedIRI=":bike"/>
          </ObjectSomeValuesFrom>
          <ObjectComplementOf>
	    <ObjectSomeValuesFrom>
	      <ObjectProperty abbreviatedIRI=":own"/>
	      <Class abbreviatedIRI=":car"/>
	    </ObjectSomeValuesFrom>
          </ObjectComplementOf>
        </ObjectIntersectionOf>
        <ObjectSomeValuesFrom>
          <ObjectInverseOf>
	    <ObjectProperty abbreviatedIRI=":like"/>
          </ObjectInverseOf>
          <ObjectOneOf>
	    <NamedIndividual abbreviatedIRI=":Mary"/>
          </ObjectOneOf>
        </ObjectSomeValuesFrom>
      </SubClassOf>
    </Ontology>

### Output

In ace-format:


	Every man that own a bike and that does not own a car is like by Mary.


In csv-format:


    ignored	Prefix(,http://www.example.org/test#)

    f	Every
    cn_sg	http://www.example.org/test#man
    f	that
    tv_sg	http://www.example.org/test#own
    f	a
    cn_sg	http://www.example.org/test#bike
    f	and
    f	that
    f	does
    f	not
    tv_pl	http://www.example.org/test#own
    f	a
    cn_sg	http://www.example.org/test#car
    f	is
    tv_vbg	http://www.example.org/test#like
    f	by
    pn_sg	http://www.example.org/test#Mary
    f	.

Dependencies
------------

The OWL verbalizer depends on a reasonably recent version of SWI-Prolog
(<http://www.swi-prolog.org/>), e.g. 5.10 or higher.
SWI-Prolog must be installed together with the packages
`clib`, `sgml`, and `http`.

See also: [docs/installing_swipl_on_linux.txt](docs/installing_swipl_on_linux.txt).

Compiling the OWL verbalizer command-line client
------------------------------------------------

First, make sure that the SWI-Prolog executable is on the PATH,
i.e. that you can execute `swipl` in any directory.

In order to compile the OWL verbalizer executable, execute:

    swipl -O -f owl_to_ace.pl -g "qsave_program('owl_to_ace.exe', [goal(main), toplevel(halt), local(25600), global(25600)])." -t halt

or, alternatively, just click on `make_exe.bat` (on Windows) or
type `sh make_exe.sh` (on Unix / Linux / MacOS X).
As a result, an EXE-file (the command-line client) is created.


Using the OWL verbalizer command-line client
--------------------------------------------

The command-line client takes one obligatory argument: the name
of the OWL 2 XML file to be verbalized.
In the following example, we are using
[examples/example.owl](examples/example.owl) as input.

	./owl_to_ace.exe -xml examples/example.owl

The output (i.e. the resulting ACE text) is printed to standard output.

Note that this command can be also executed as:

	swipl -x owl_to_ace.exe -- -xml examples/example.owl

This you can use on computers where the path to SWI-Prolog is different than the
one that you used during building.

If executing the EXE-file results in an "out of stack" error, then increase the stack sizes via
options `local/1` and `global/1`, and recompile.


Using the OWL verbalizer webservice
-----------------------------------

The OWL verbalizer webservice is launched on the command-line, e.g.:

	./owl_to_ace.exe -httpserver -port 5123 -workers 2

A good way to start the service on a Unix command-line is:

	nohup swipl -x owl_to_ace.exe -- -httpserver -port 5123 -workers 2 > stdout.txt 2> stderr.txt &

In this case, the service is started in the background,
a start up message is stored in `stdout.txt`, and possible error messages in `stderr.txt`.
On Mac OS X, one could use `launchctl` instead.

As a result, a webserver (SWI HTTP server) is started on port 5123 with 2 worker threads.
The optimal number of workers depends on the number of CPUs.

In order to verbalize an ontology,
post the OWL 2 XML document to the server via the argument `xml`.
Following are some examples of using `curl` to achieve this.

	curl -F "xml=@examples/example.owl" http://localhost:5123

	curl -F "xml=<examples/example.owl" http://localhost:5123

	cat examples/example.owl | curl -F "xml=<-" http://localhost:5123

	curl 'http://owl.cs.manchester.ac.uk/repository/download?ontology=http://www.co-ode.org/ontologies/pizza/pizza.owl&format=OWL/XML' | curl -F "xml=<-" http://localhost:5123

Posting from an HTML page: look at the source code of [docs/owl_to_ace.html](docs/owl_to_ace.html).

Posting from Java: use the OWL verbalizer Java interface, in directory [java/](java/).


How are OWL entity IRIs verbalized?
-----------------------------------

The OWL verbalizer maps OWL entity IRIs to ACE content words such that

  - OWL individuals map to ACE proper names (PN)
  - OWL classes map to ACE common nouns (CN)
  - OWL properties map to ACE transitive verbs (TV)

There are 6 morphological categories that determine the surface form of an IRI:

  - singular form of a proper name (e.g. _John_)
  - singular form of a common noun (e.g. _man_)
  - plural form of a common noun (e.g. _men_)
  - singular form of a transitive verb (e.g. _mans_)
  - plural form of a transitive verb (e.g. _man_)
  - past participle form a transitive verb (e.g. _manned_)

The user has full control over the eventual surface forms of the IRIs but
has to choose them in terms of the above categories. Furthermore,

  - the surface forms must be legal ACE content words (e.g. they should not contain punctuation symbols);
  - the mapping of IRIs to surface forms must be bidirectional within the same word class, in order to be able to (if needed) parse the verbalization back into OWL in a semantics preserving way.

### Using the lexicon

It is possible to specify the mapping of IRIs to surface forms
using the following annotation properties:

  - `http://attempto.ifi.uzh.ch/ace_lexicon#PN_sg`
  - `http://attempto.ifi.uzh.ch/ace_lexicon#CN_sg`
  - `http://attempto.ifi.uzh.ch/ace_lexicon#CN_pl`
  - `http://attempto.ifi.uzh.ch/ace_lexicon#TV_sg`
  - `http://attempto.ifi.uzh.ch/ace_lexicon#TV_pl`
  - `http://attempto.ifi.uzh.ch/ace_lexicon#TV_vbg`

For example, the following axioms state that if the IRI "#man" is used as a plural
common noun, then the wordform _men_ must be used by the verbalizer. If, however, it
is used as a singular transitive verb, then _mans_ must be used.

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

For example, these axioms support the generation of the sentence `John mans at most 3 men.`
from an axiom that uses the IRI "#man" via punning once as an object property name,
and once as a class name.

If the mapping of an IRI is missing then its fragment is used in the output.
The fragment is the part that comes after '#' or the last '/'.
Note that this means that different IRIs are not necessarily verbalized as different.

### Not using the lexicon (and leaving the IRIs as they are)

If the output-mode is "csv", then each ACE token is output on a separate line
and morphological mappings are left to the user to be applied after the verbalization.
An example of csv-output is:

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

The columns are tab-separated. The first column specifies the type of the token, e.g.

  - `f` = function word
  - `qs` = quoted string
  - `comment` = comment
  - `cn_sg` = singular common noun
  - ...

and the 2nd column contains ACE tokens and OWL IRIs. Empty line denotes axiom
borders (each OWL axiom in the input ontology is verbalized by 0 or more ACE
sentences).

In this mode the AnnotationAssertions are ignored and IRI fragments are not generated.
As a result this mode is about 3x faster.


API-level access to the OWL verbalizer
--------------------------------------

The API-level access to the OWL verbalizer is only documented with SWI-Prolog's PlDoc.
Provided that you have PlDoc installed (SWI-Prolog package `pldoc`),
you can view the documentation by:

	?- doc_server(8000).
	?- [owl_to_ace].
	?- doc_browser.

The main entry point to the verbalizer is the module
[axiom_sentencelist.pl](axiom_sentencelist.pl)
which converts an input OWL axiom
(Prolog term in OWL FSS) to a list of ACE sentences (where each sentence is a Prolog
list of ACE tokens, where each token is a Prolog atom, number, or unary ground term).

Tools that use the OWL verbalizer
---------------------------------

- [ACE compliant controlled Latvian for ontology authoring and verbalization](http://valoda.ailab.lv/cnl/)
- [ACE View — a natural language interface to knowledge engineering](http://attempto.ifi.uzh.ch/aceview/)
- [MoKi: the Enterprise Modelling WiKi](https://moki.fbk.eu) uses the OWL verbalizer for the "lightly-structured" presentation of the content

Similar projects
----------------

- [SWAT Natural Language Tools](http://swat.open.ac.uk/swat/)
