% Converts the clex-formatted lexicon into OWL 2 XML syntax.
% The lexicon entries are represented by OWL annotation assertions
% which are essentially triples of the from
%
%  (Entity IRI, Annotation IRI, Literal)
%
% e.g.
%
% the entry
%
%  noun_pl(cities, city, neutr).
%
% is converted into
%
%  <AnnotationAssertion>
%    <AnnotationProperty abbreviatedIRI="t:CN_pl"/>
%    <AbbreviatedIRI>e:city</AbbreviatedIRI>
%    <Literal>cities</Literal>
%  </AnnotationAssertion>
%
% Also generates declaration axioms, e.g.:
%
%  <Declaration>
%    <Class abbreviatedIRI="e:city"/>
%  </Declaration>
%
% 'e' and 't' are prefixes which are defined in the beginning of the resulting
% file and can be easily redefined, e.g. in Protege.
%
% The conversion is only applied to 6 lexicon categories, the rest are ignored.
% Also, the gender-information of nouns is not represented.
%
% Author: Kaarel Kaljurand
% Version: 2011-08-02

% Usage example:
% swipl -f clex_to_owl.pl -g "main('clex_lexicon.pl')" -t halt -q > clex.owl

% TODO:
% - check if the generated IRI is a legal IRI and escape if needed
% - do not generate duplicate declaration axioms
% - should we generate declaration axioms at all?

main(File) :-
	open(File, read, Fd, []),
	get_annotations(Fd, Annotations),
	close(Fd),
	wrap_annotations(Annotations, Owl),
	serialize_as_xml(Owl).


get_annotations(Fd, AnnOut) :-
	read_term(Fd, Entry, []),
	(
		Entry = end_of_file
	->
		AnnOut = []
	;
		entry_to_ann(Entry, AnnIn, AnnOut),
		get_annotations(Fd, AnnIn)
	).


entry_to_ann(Entry, AnnIn, AnnOut) :-
	ann(Entry, Ann),
	!,
	convert_ann(Ann, AnnIn, AnnOut).

entry_to_ann(_Entry, AnnIn, AnnIn).


convert_ann([X], Ann, [X | Ann]).
convert_ann([X, Y], Ann, [X, Y | Ann]).


ann(pn_sg(WordForm, Lemma, _Type), Element) :-
	get_annass('PN_sg', Lemma, WordForm, Element).
ann(noun_sg(WordForm, Lemma, _Type), Element) :-
	get_annass('CN_sg', Lemma, WordForm, Element).
ann(noun_pl(WordForm, Lemma, _Type), Element) :-
	get_annass('CN_pl', Lemma, WordForm, Element).
ann(tv_finsg(WordForm, Lemma), Element) :-
	get_annass('TV_sg', Lemma, WordForm, Element).
ann(tv_infpl(WordForm, Lemma), Element) :-
	get_annass('TV_pl', Lemma, WordForm, Element).
ann(tv_pp(WordForm, Lemma), Element) :-
	get_annass('TV_vbg', Lemma, WordForm, Element).


get_declaration(Type, Iri, element('Declaration', [], [
	element(Type, [abbreviatedIRI=Iri], [])])).


get_annass(MorphType, Lemma, WordForm, [Declaration, Annotation]) :-
	atomic_list_concat([t, ':', MorphType], MorphTypeIri),
	atomic_list_concat([e, ':', Lemma], LemmaIri),
	morphtype_to_entitytype(MorphType, EntityType),
	get_declaration(EntityType, LemmaIri, Declaration),
	Annotation = element('AnnotationAssertion', [], [
		element('AnnotationProperty', [abbreviatedIRI=MorphTypeIri], []),
		element('AbbreviatedIRI', [], [LemmaIri]),
		element('Literal', [], [WordForm])
	]).


%%
%
%
morphtype_to_entitytype('PN_sg', 'NamedIndividual').
morphtype_to_entitytype('CN_sg', 'Class').
morphtype_to_entitytype('CN_pl', 'Class').
morphtype_to_entitytype('TV_sg', 'ObjectProperty').
morphtype_to_entitytype('TV_pl', 'ObjectProperty').
morphtype_to_entitytype('TV_vbg', 'ObjectProperty').


%% wrap_annotations(+Annotations:list, -Xml:xml)
%
% Wraps the OWL annotations into the complete OWL file
% which contains also the header and the prefix declarations.
%
wrap_annotations(Annotations,
	element('Ontology', [
		xmlns='http://www.w3.org/2002/07/owl#',
		'xml:base'='http://attempto.ifi.uzh.ch/clex',
		'xmlns:xml'='http://www.w3.org/XML/1998/namespace'
	], [
		element('Prefix', [name='t', 'IRI'='http://attempto.ifi.uzh.ch/ace_lexicon#'], []),
		element('Prefix', [name='e', 'IRI'='http://attempto.ifi.uzh.ch/clex#'], [])
	| Annotations])
).


serialize_as_xml(Xml) :-
	set_stream(user_output, encoding(utf8)),
	xml_write(Xml, []).
