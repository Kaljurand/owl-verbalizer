% This file is part of the OWL verbalizer.
% Copyright 2008-2010, Kaarel Kaljurand <kaljurand@gmail.com>.
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

:- module(lexicon, [
		set_default_ns/1,
		asserta_lexicon/1,
		pn_sg/2,
		cn_sg/2,
		cn_pl/2,
		tv_sg/2,
		tv_pl/2,
		tv_vbg/2
	]).


/** <module> Lexicon

@author Kaarel Kaljurand
@version 2010-06-15

Lemma to surface form mapping.
The mappings must be bidirectional within the same word class.

The supported surface forms are:

* singular proper name,
* singular noun,
* plural noun,
* singular verb,
* plural verb (= infinitive verb),
* past participle (passive participle) verb.

The lexicon is instantiated from a list of OWL annotation assertion axioms.

@tbd Cut when first argument is instantiated (?)
@tbd Add support for checking the bidirectionality of morphological mappings
@tbd Use compile_predicates/1 for all dynamic predicates, e.g.
in asserta_lexicon/1: compile_predicates([pn_sg_x/2, ...]). Actually this wouldn't work
in the server mode as the dynamic predicates need to be retracted and new ones created
all the time.

*/

:- dynamic(pn_sg_x/2, cn_sg_x/2, cn_pl_x/2, tv_sg_x/2, tv_pl_x/2, tv_vbg_x/2).
:- dynamic(default_ns/1).

% BUG: experimental
default_ns('').

%% asserta_lexicon(+AxiomList:list) is det.
%% asserta_lexicon_x(+AxiomList:list) is det.
%
% Instantiates the lexicon (mapping from lemmas to surface forms)
% from a given list of entity annotation axioms.
%
% Note we use asserta/1 to override any existing entries for this
% word and category.
%
% Note that the Structural Specification and Functional-Style Syntax says that:
%
%==
% ontology := 'Ontology' '(' ontologyURI { importDeclaration } { annotation } { axiom } ')'
% axiom := classAxiom | objectPropertyAxiom | dataPropertyAxiom | fact | declaration | entityAnnotation
%==
%
% i.e. entity annotations can be mixed with logical axioms. This means that in order
% to construct the lexicon we have to scan first the complete ontology.
%
% @param AxiomList is a list of OWL axioms
%
asserta_lexicon(AxiomList) :-
	retract_lexicon,
	asserta_lexicon_x(AxiomList).


asserta_lexicon_x([]).

% @deprecated (OWL-API 2 style): use AnnotationAssertion
asserta_lexicon_x(['EntityAnnotation'(Entity, 'Annotation'(Feature, '^^'(Form, _))) | AxiomList]) :-
	make_lex_entry(Entity, Feature, Form, Entry),
	!,
	%format("Adding lex entry: ~q~n", [Entry]),
	asserta(Entry),
	asserta_lexicon_x(AxiomList).

% AnnotationAssertion(AnnotationProperty(sg), IRI(#John), ^^(John, http://www.w3.org/2001/XMLSchema#string))
asserta_lexicon_x(['AnnotationAssertion'('AnnotationProperty'(Property), 'IRI'(Iri), '^^'(WordForm, _)) | AxiomList]) :-
	concat_atom([NS, Lemma], '#', Iri),
	atom_concat(NS, '#', NS1),
	make_lex_entry(NS1:Lemma, Property, WordForm, Entry),
	!,
	% format("Adding lex entry: ~q~n", [Entry]),
	asserta(Entry),
	asserta_lexicon_x(AxiomList).

asserta_lexicon_x([_Axiom | AxiomList]) :-
	% format("Ignoring: ~w~n", [Axiom]),
	asserta_lexicon_x(AxiomList).


%% retract_lexicon is det.
%
% Removes all dynamic lexicon entries.
%
retract_lexicon :-
	retractall(pn_sg_x(_, _)),
	retractall(cn_sg_x(_, _)),
	retractall(cn_pl_x(_, _)),
	retractall(tv_sg_x(_, _)),
	retractall(tv_pl_x(_, _)),
	retractall(tv_vbg_x(_, _)).


%% make_lex_entry(+OWLEntity:term, +MorphType:atom, +WordForm:atom, -LexiconEntry:term) is det.
%
% @param OWLEntity is an OWL entity, e.g. 'Class'(ClassURI)
% @param MorphType is an URI for an ACE morphological type, one of {sg, pl, vbg}
% @param WordForm is an ACE surface word-form
% @param LexiconEntry is a lexicon entry, one of {n_pl/2, v_sg/2, v_vbg/2}
%
% @deprecated
%
make_lex_entry('NamedIndividual'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#sg', Form, pn_sg_x(Lemma, Form)).
make_lex_entry('Class'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#sg', Form, cn_sg_x(Lemma, Form)).
make_lex_entry('Class'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#pl', Form, cn_pl_x(Lemma, Form)).
make_lex_entry('ObjectProperty'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#sg', Form, tv_sg_x(Lemma, Form)).
make_lex_entry('ObjectProperty'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#pl', Form, tv_pl_x(Lemma, Form)).
make_lex_entry('ObjectProperty'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#vbg', Form, tv_vbg_x(Lemma, Form)).


%% make_lex_entry(+Iri:atom, +MorphType:atom, +WordForm:atom, -LexiconEntry:term) is det.
%
% @param Iri is an IRI, e.g. ClassIRI
% @param MorphType is an IRI for an ACE morphological type, one of {PN_sg, CN_sg, CN_pl, TV_sg, TV_pl, TV_vbg}
% @param WordForm is an ACE surface word-form
% @param LexiconEntry is a lexicon entry, one of {n_pl/2, v_sg/2, v_vbg/2}
%
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'PN_sg', Form, pn_sg_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'CN_sg', Form, cn_sg_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'CN_pl', Form, cn_pl_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'TV_sg', Form, tv_sg_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'TV_pl', Form, tv_pl_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'TV_vbg', Form, tv_vbg_x(Lemma, Form)).

% BUG: just for testing, remove these at some point
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':sg, Form, tv_sg_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':pl, Form, tv_pl_x(Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':vbg, Form, tv_vbg_x(Lemma, Form)).


%% pn_sg(+Lemma, +WordForm) is det.
%% cn_sg(+Lemma, +WordForm) is det.
%% cn_pl(+Lemma, +WordForm) is det.
%% tv_sg(+Lemma, +WordForm) is det.
%% tv_pl(+Lemma, +WordForm) is det.
%% tv_vbg(+Lemma, +WordForm) is det.
%
% Interface to the dynamic lexicon.
%
pn_sg(X, Y) :- pn_sg_x(X, Y).
pn_sg(_:X, X). % BUG: fallback

% BUG experimental
/*
pn_sg(NS:X, Y) :-
	default_ns(NS),
	pn_sg_x(NS:X, Y),
	!.

pn_sg(NS:X, NSY) :-
	pn_sg_x(NS:X, Y),
	concat_atom([other, Y], NSY),
	!.

pn_sg(NS:X, X) :-
	default_ns(NS),
	!.

pn_sg(NS:X, NSX) :-
	concat_atom(['`', NS, X, '`'], NSX).
*/


cn_sg(X, Y) :- cn_sg_x(X, Y).
cn_sg(_:X, X). % BUG: fallback

cn_pl(X, Y) :- cn_pl_x(X, Y).
cn_pl(_:X, X). % BUG: fallback


tv_sg(X, Y) :- tv_sg_x(X, Y).
tv_sg(_:X, X). % BUG: fallback

tv_pl(X, Y) :- tv_pl_x(X, Y).
tv_pl(_:X, X). % BUG: fallback

tv_vbg(X, Y) :- tv_vbg_x(X, Y).
tv_vbg(_:X, X). % BUG: fallback


%% set_default_ns(+NS:atom) is det.
%
set_default_ns(NS) :-
	assert(default_ns(NS)).
