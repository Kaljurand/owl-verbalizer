% This file is part of the OWL verbalizer.
% Copyright 2008-2011, Kaarel Kaljurand <kaljurand@gmail.com>.
%
% The OWL verbalizer is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the
% Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%
% The OWL verbalizer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
% without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
% See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the
% OWL verbalizer. If not, see http://www.gnu.org/licenses/.

:- module(owlxml_owlfss, [
		owlxml_owlfss/3
	]).

/** <module> owlxml_owlfss

@author Kaarel Kaljurand
@version 2011-06-11

TODO:

* Make the code bidirectional,
i.e. XML<->FSS and use it in ACE->OWL to generate XML output.

*/


%% owlxml_owlfss(+Stream:stream, -Ontology:term, -ErrorList:list) is det.
%
% Translates OWL 2 XML serialization syntax
% into a Prolog term that represents this OWL ontology
% in Functional-Style Syntax. Unsupported XML elements are collected
% into ErrorList. Abbreviated IRIs are expanded into full IRIs.
%
% Closes the stream after reading it into the XML term.
%
% Note that the following are equivalent:
%
% load_xml_file(Stream, XML)
% load_structure(Stream, XML, [dialect(xml)])
%
% We use the general rule in order to be able to remove whitespace.
%
% @param Stream is a stream that contains OWL 2 XML
% @param Ontology is an OWL ontology in OWL 2 Functional-Style Syntax
% @param ErrorList is a list of encountered errors
%
% @bug Seems to normalize whitespace within PCDATA which we do not actually want.
%
owlxml_owlfss(Stream, Ontology, ErrorList) :-
	load_structure(Stream, XML, [dialect(xml), space(remove)]),
	% We sometimes got an existence error, force(true) should avoid that
	close(Stream, [force(true)]),
	ellist_termlist(XML, []-ErrorList, [Ontology]).


%% ellist_termlist(+ElementList:list, -TermList:list) is det.
%
% @param ElementList is a list of XML elements
% @param TermList is a list of Prolog terms (representing OWL expressions)
%
ellist_termlist([], E-E, []).

ellist_termlist([El | ElList], E1-E3, [Term | TermList]) :-
	el_term(El, E1-E2, Term),
	!,
	ellist_termlist(ElList, E2-E3, TermList).

% BUG: build a list of errors in el_term/2 instead
ellist_termlist([El | ElList], E1-[El | E2], TermList) :-
	ellist_termlist(ElList, E1-E2, TermList).


%% el_term(+Element:term, +OWLEntity:term) is det.
%
% @param Element is an XML element
% @param OWLEntity is an OWL entity
%
el_term(element('Ontology', AttrList, ElList), E1-E2, 'Ontology'(_, XmlBase, TermList)) :-
	!,
	get_xmlbase(AttrList, XmlBase),
	ellist_termlist(ElList, E1-E2, TermList).

el_term(element('Prefix', AttrList, _), E-E, 'Prefix'(Name, IRI)) :-
	memberchk(name = Name, AttrList),
	memberchk('IRI' = IRI, AttrList),
	% BUG: use global variables instead
	assert(owlverbalizer_Prefix(Name, IRI)),
	!.

el_term(element('AbbreviatedIRI', [], [PCDATA]), E-E, 'IRI'(Iri)) :-
	airi_name(PCDATA, Iri),
	!.

el_term(element('AnonymousIndividual', [nodeID = NodeId], _), E-E, 'AnonymousIndividual'(NodeId)) :-
	!.

el_term(element('Literal', Attrs, Data), E-E, ParsedLiteral) :-
	parse_literal(Attrs, Data, ParsedLiteral),
	!.

% @deprecated (OWL-API 2): use Literal[]
el_term(element('Constant', [], []), E-E, '^^'('', '')) :- !.

% @deprecated (OWL-API 2): use Literal[]
el_term(element('Constant', [], [PCDATA]), E-E, '^^'(PCDATA, '')) :- !.

% @deprecated (OWL-API 2): use Literal[@datatypeIRI]
% BUG: is this obsolete? Yes, but before removal, test cases must be updated.
el_term(element('Constant', ['Datatype' = Datatype], [PCDATA]), E-E, '^^'(Data, Datatype)) :-
	!,
	datatype_pcdata_data(PCDATA, Datatype, Data).

% @deprecated (OWL-API 2): use Literal[@datatypeIRI]
% BUG: is this obsolete? Yes, but before removal, test cases must be updated.
el_term(element('Constant', ['Datatype' = Datatype], []), E-E, '^^'('', Datatype)) :- !.

% @deprecated (OWL-API 2): use Literal[@datatypeIRI]
el_term(element('Constant', ['datatypeURI' = Datatype], [PCDATA]), E-E, '^^'(Data, Datatype)) :-
	!,
	datatype_pcdata_data(PCDATA, Datatype, Data).

% @deprecated (OWL-API 2): use Literal[@datatypeIRI]
el_term(element('Constant', ['datatypeURI' = Datatype], []), E-E, '^^'('', Datatype)) :- !.

el_term(element(Term, [], ElList), E1-E2, Return) :-
	is_owl_list(Term, TermList, Return),
	!,
	ellist_termlist(ElList, E1-E2, TermList).

el_term(element(Term, [cardinality = Cardinality], ElList), E1-E2, Return) :-
	is_cardinality(Cardinality, Number),
	!,
	ellist_termlist(ElList, E1-E2, TermList),
	Return =.. [Term, Number | TermList].

el_term(element(Term, [facet = Facet], ElList), E1-E2, Return) :-
	!,
	ellist_termlist(ElList, E1-E2, TermList),
	Return =.. [Term, Facet | TermList].

el_term(element(Term, ['IRI' = Iri], _), E-E, Base) :-
	is_base(Term, Iri, Base),
	!.

el_term(element(Term, ['abbreviatedIRI' = AbbreviatedIri], _), E-E, Base) :-
	is_base(Term, Name, Base),
	airi_name(AbbreviatedIri, Name),
	!.

% @deprecated (OWL-API 2): use IRI
el_term(element(Term, ['URI' = Uri], _), E-E, Base) :-
	is_base(Term, Uri, Base),
	!.

% @deprecated (OWL-API 2): use ObjectHasSelf
% (The purpose of this rule is simply to provide backwards compatibility.)
el_term(element('ObjectExistsSelf', [], ElList), E1-E2, Return) :-
	!,
	ellist_termlist(ElList, E1-E2, TermList),
	Return =.. ['ObjectHasSelf' | TermList].

% @deprecated (OWL-API 2): use ObjectInverseOf
% (The purpose of this rule is simply to provide backwards compatibility.)
el_term(element('InverseObjectProperty', [], ElList), E1-E2, Return) :-
	!,
	ellist_termlist(ElList, E1-E2, TermList),
	Return =.. ['ObjectInverseOf' | TermList].

el_term(element(Term, [], ElList), E1-E2, Return) :-
	!,
	ellist_termlist(ElList, E1-E2, TermList),
	Return =.. [Term | TermList].

el_term(element(Term, ['annotationURI' = Uri], ElList), E1-E2, Return) :-
	Return =.. [Term, Uri, Term1],
	ellist_termlist(ElList, E1-E2, [Term1]).

el_term(PCDATA, E-E, PCDATA) :-
	atom(PCDATA).


%% is_cardinality(+Atom:atom, -Integer:integer) is det.
%
%
is_cardinality(Atom, Number) :-
	(
		atom(Atom)
	;
		number(Number)
	),
	atom_number(Atom, Number).


%% is_base(+OWLEntityName:atom, +URIName:atom, -OWLEntity:term) is det.
%
%
is_base('Class', Name, 'Class'(Name)).
is_base('ObjectProperty', Name, 'ObjectProperty'(Name)).
is_base('DataProperty', Name, 'DataProperty'(Name)).
is_base('AnnotationProperty', Name, 'AnnotationProperty'(Name)).
is_base('NamedIndividual', Name, 'NamedIndividual'(Name)).
is_base('Datatype', Name, 'Datatype'(Name)).

% @deprecated, use 'Class'
is_base('OWLClass', Name, 'Class'(Name)).
% @deprecated (OWL-API 2), use 'NamedIndividual' or 'AnonymousIndividual'
is_base('Individual', Name, 'NamedIndividual'(Name)).


%% is_owl_list(+ElementName:atom) is det.
%
% Succeeds if the element denoted by the name has a set/list type
% in OWL structural specification.
%
% @param ElementName is an OWL XML element name
%
% @tbd
%
% DataAllValuesFrom
% DataSomeValuesFrom
% DisjointUnion
%
is_owl_list('DataOneOf', TermList, 'DataOneOf'(TermList)).
is_owl_list('ObjectUnionOf', TermList, 'ObjectUnionOf'(TermList)).
is_owl_list('ObjectIntersectionOf', TermList, 'ObjectIntersectionOf'(TermList)).
is_owl_list('ObjectOneOf', TermList, 'ObjectOneOf'(TermList)).
is_owl_list('EquivalentClasses', TermList, 'EquivalentClasses'(TermList)).
is_owl_list('DisjointClasses', TermList, 'DisjointClasses'(TermList)).
is_owl_list('ObjectPropertyChain', TermList, 'ObjectPropertyChain'(TermList)).
is_owl_list('EquivalentObjectProperties', TermList, 'EquivalentObjectProperties'(TermList)).
is_owl_list('DisjointObjectProperties', TermList, 'DisjointObjectProperties'(TermList)).
is_owl_list('EquivalentDataProperties', TermList, 'EquivalentDataProperties'(TermList)).
is_owl_list('DisjointDataProperties', TermList, 'DisjointDataProperties'(TermList)).
is_owl_list('SameIndividual', TermList, 'SameIndividual'(TermList)).
is_owl_list('DifferentIndividuals', TermList, 'DifferentIndividuals'(TermList)).

% @deprecated
is_owl_list('SameIndividuals', TermList, 'SameIndividuals'(TermList)).
is_owl_list('SubObjectPropertyChain', TermList, 'ObjectPropertyChain'(TermList)).

% BUG: the spec doesn't say that it is a set
is_owl_list('DatatypeRestriction', TermList, 'DatatypeRestriction'(TermList)).

/*
is_owl_ternary('ObjectPropertyAssertion').
is_owl_ternary('DataPropertyAssertion').

is_owl_binary('ClassAssertion').
is_owl_binary('SubClassOf').
is_owl_binary('SubObjectPropertyOf').
is_owl_binary('ObjectSomeValuesFrom').
is_owl_binary('ObjectAllValuesFrom').
is_owl_binary('ObjectHasValue').
is_owl_binary('ObjectMaxCardinality').
is_owl_binary('ObjectMinCardinality').
is_owl_binary('ObjectExactCardinality').
is_owl_binary('DataHasValue').
is_owl_binary('DataMaxCardinality').
is_owl_binary('DataMinCardinality').
is_owl_binary('DataExactCardinality').
is_owl_binary('ObjectPropertyDomain').
is_owl_binary('ObjectPropertyRange').
is_owl_binary('DataPropertyDomain').
is_owl_binary('DataPropertyRange').
is_owl_binary('InverseObjectProperties').
is_owl_binary('EntityAnnotation').

is_owl_unary('ObjectInverseOf').
is_owl_unary('ObjectHasSelf').
is_owl_unary('ObjectComplementOf').
is_owl_unary('TransitiveObjectProperty').
is_owl_unary('FunctionalObjectProperty').
is_owl_unary('InverseFunctionalObjectProperty').
is_owl_unary('ReflexiveObjectProperty').
is_owl_unary('IrreflexiveObjectProperty').
is_owl_unary('SymmetricObjectProperty').
is_owl_unary('AsymmetricObjectProperty').
is_owl_unary('FunctionalDataProperty').

% @deprecated (OWL-API 2): use ObjectHasSelf
is_owl_unary('ObjectExistsSelf').

% @deprecated (OWL-API 2): use ObjectInverseOf
%is_owl_unary('InverseObjectProperty').
*/


%% airi_name(+AbbreviatedIri:atom, -Iri:atom) is det.
%
% Expands abbreviated IRIs.
%
airi_name(AbbreviatedIri, Iri) :-
	concat_atom([Prefix, Name], ':', AbbreviatedIri),
	abbr_expansion(Prefix, Expansion),
	concat_atom([Expansion, Name], Iri).


%% abbr_expansion
%
% Front-end to Prefix-declarations
%
% abbr_expansion(ace_lexicon, 'http://attempto.ifi.uzh.ch/ace_lexicon') :- !.
abbr_expansion(Name, IRI) :-
	owlverbalizer_Prefix(Name, IRI),
	!.
% BUG: throw exception instead
abbr_expansion(_, 'http://attempto.ifi.uzh.ch/default').


%% parse_literal
%
%
parse_literal(Attrs, [], '^^'('', Datatype)) :-
	memberchk('datatypeIRI' = Datatype, Attrs),
	!.

parse_literal(_Attrs, [], '^^'('', '')).

parse_literal(Attrs, [PCDATA], '^^'(Data, Datatype)) :-
	memberchk('datatypeIRI' = Datatype, Attrs),
	!,
	datatype_pcdata_data(PCDATA, Datatype, Data).

parse_literal(_Attrs, [PCDATA], '^^'(PCDATA, '')).



%% datatype_pcdata_data(+PCDATA:atom, +Datatype:atom, -Data:atomic) is det.
%
% The purpose of this rule is to convert some of the PCDATA
% into Prolog numbers. In case of failure, the PCDATA is returned
% as it is, i.e. it will be represented as an atom.
%
% Note that if the PCDATA (i.e. an atom) cannot be converted
% into a number then atom_number/2 throws an exception.
% This is caught at higher level (for the time being).
%
% @param PCDATA is a data value (atom)
% @param Datatype is an XML Schema datatype
% @param Data is a data value (atom or number)
%
datatype_pcdata_data(PCDATA, Datatype, Data) :-
	is_numbertype(Datatype),
	!,
	atom_number(PCDATA, Data).

datatype_pcdata_data(PCDATA, _, PCDATA).


%% is_numbertype(+Datatype:atom) is det.
%
% @param Datatype is an XML Schema datatype
%
is_numbertype('http://www.w3.org/2001/XMLSchema#int').
is_numbertype('http://www.w3.org/2001/XMLSchema#integer').
is_numbertype('http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
is_numbertype('http://www.w3.org/2001/XMLSchema#float').
is_numbertype('http://www.w3.org/2001/XMLSchema#double').
is_numbertype('http://www.w3.org/2001/XMLSchema#short').
is_numbertype('http://www.w3.org/2001/XMLSchema#long').


%% get_xmlbase(+AttrList:list) is det.
%
get_xmlbase(AttrList, XmlBase) :-
	memberchk('xml:base' = XmlBase, AttrList),
	!.

get_xmlbase(_AttrList, '').
