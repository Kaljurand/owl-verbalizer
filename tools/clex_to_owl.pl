#!/opt/local/bin/swipl -f none -g main -t halt -s

% Converts the clex-formatted lexicon into OWL 2 XML syntax.
% The lemmas introduce OWL entities and the surface forms introduce
% entity annotations for these entities.
%
% Note: work in progress!
%
% Author: Kaarel Kaljurand
% Version: 2008-10-01

:- set_prolog_flag(encoding, utf8).

:- [clex_lexicon].

main :-
	findall(Node, (
		noun_sg(SgForm, Lemma, _),
		lexentry_to_node(noun_sg(SgForm, Lemma, _), Node)
		),
		NodeList
	),
	XML = [element('Ontology', [
			xmlns='http://www.w3.org/2006/12/owl2-xml#',
			'URI'='http://attempto.ifi.uzh.ch/ontologies/owlswrl/test'
			], NodeList)],
	set_stream(user_output, encoding(utf8)),
	xml_write(XML, []).


lexentries_to_nodes([LexEntry | LexEntryList], [Node | NodeList]) :-
	lexentry_to_node(LexEntry, Node),
	lexentries_to_nodes(LexEntryList, NodeList).


lexentry_to_node(
	noun_sg(SgForm, Lemma, _),
	element('EntityAnnotation', [], [
		element('OWLClass', ['URI'=Lemma], []),
		element('Annotation', ['annotationURI'='http://attempto.ifi.uzh.ch/ace_lexicon#sg'], [
			element('Constant', [], [SgForm])
		])
	])
	).
