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
@version 2011-06-09

Lemma to surface form mapping.
The mappings must be bidirectional within the same word class.

The supported surface forms are:

* (singular) proper name,
* singular noun,
* plural noun,
* singular verb,
* plural verb (= infinitive verb),
* past participle (passive participle) verb.

The lexicon is instantiated from a list of OWL annotation assertion axioms.

@tbd Cut when first argument is instantiated (?)
@tbd Add support for checking the bidirectionality of morphological mappings

*/

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
	set_lexicon_entry(Entry),
	asserta_lexicon_x(AxiomList).

% AnnotationAssertion(AnnotationProperty(sg), IRI(#John), ^^(John, http://www.w3.org/2001/XMLSchema#string))
asserta_lexicon_x(['AnnotationAssertion'('AnnotationProperty'(Property), 'IRI'(Iri), '^^'(WordForm, _)) | AxiomList]) :-
	concat_atom([NS, Lemma], '#', Iri),
	atom_concat(NS, '#', NS1),
	make_lex_entry(NS1:Lemma, Property, WordForm, Entry),
	!,
	set_lexicon_entry(Entry),
	asserta_lexicon_x(AxiomList).

asserta_lexicon_x([_Axiom | AxiomList]) :-
	% format("Ignoring: ~w~n", [Axiom]),
	asserta_lexicon_x(AxiomList).


%% retract_lexicon is det.
%
% Removes all dynamic lexicon entries.
%
% TODO: not sure we need it
%
retract_lexicon :-
	nb_current(N, _), nb_delete(N), fail ; true.


%% make_lex_entry(+OWLEntity:term, +MorphType:atom, +WordForm:atom, -LexiconEntry:term) is det.
%
% @param OWLEntity is an OWL entity, e.g. 'Class'(ClassURI)
% @param MorphType is an URI for an ACE morphological type, one of {sg, pl, vbg}
% @param WordForm is an ACE surface word-form
% @param LexiconEntry is a lexicon entry, one of {n_pl/2, v_sg/2, v_vbg/2}
%
% @deprecated
%
make_lex_entry('NamedIndividual'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#sg', Form, entry('PN_sg', Lemma, Form)).
make_lex_entry('Class'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#sg', Form, entry('CN_sg', Lemma, Form)).
make_lex_entry('Class'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#pl', Form, entry('CN_pl', Lemma, Form)).
make_lex_entry('ObjectProperty'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#sg', Form, entry('TV_sg', Lemma, Form)).
make_lex_entry('ObjectProperty'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#pl', Form, entry('TV_pl', Lemma, Form)).
make_lex_entry('ObjectProperty'(Lemma), 'http://attempto.ifi.uzh.ch/ace_lexicon#vbg', Form, entry('TV_vbg', Lemma, Form)).


%% make_lex_entry(+Iri:term, +MorphType:atom, +WordForm:atom, -LexiconEntry:term) is det.
%
% @param Iri is an IRI, e.g. 'http://blah':man
% @param MorphType is an IRI for an ACE morphological type, one of {PN_sg, CN_sg, CN_pl, TV_sg, TV_pl, TV_vbg}
% @param WordForm is an ACE surface word-form
% @param LexiconEntry is a lexicon entry, one of {n_pl/2, v_sg/2, v_vbg/2}
%
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'PN_sg', Form, entry('PN_sg', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'CN_sg', Form, entry('CN_sg', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'CN_pl', Form, entry('CN_pl', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'TV_sg', Form, entry('TV_sg', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'TV_pl', Form, entry('TV_pl', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':'TV_vbg', Form, entry('TV_vbg', Lemma, Form)).

% BUG: just for testing, remove these at some point
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':sg, Form, entry('TV_sg', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':pl, Form, entry('TV_pl', Lemma, Form)).
make_lex_entry(Lemma, 'http://attempto.ifi.uzh.ch/ace_lexicon#':vbg, Form, entry('TV_vbg', Lemma, Form)).


%% pn_sg(+Lemma, +WordForm) is det.
%% cn_sg(+Lemma, +WordForm) is det.
%% cn_pl(+Lemma, +WordForm) is det.
%% tv_sg(+Lemma, +WordForm) is det.
%% tv_pl(+Lemma, +WordForm) is det.
%% tv_vbg(+Lemma, +WordForm) is det.
%
% Interface to the dynamic lexicon.
%
pn_sg(X, Y) :- get_lexicon_entry('PN_sg', X, Y), !.
pn_sg(_:X, X). % BUG: fallback

% BUG experimental
/*
pn_sg(NS:X, Y) :-
	get_default_ns(NS),
	pn_sg_x(NS:X, Y),
	!.

pn_sg(NS:X, NSY) :-
	pn_sg_x(NS:X, Y),
	concat_atom([other, Y], NSY),
	!.

pn_sg(NS:X, X) :-
	get_default_ns(NS),
	!.

pn_sg(NS:X, NSX) :-
	concat_atom(['`', NS, X, '`'], NSX).
*/


cn_sg(X, Y) :- get_lexicon_entry('CN_sg', X, Y), !.
cn_sg(_:X, X). % BUG: fallback

cn_pl(X, Y) :- get_lexicon_entry('CN_pl', X, Y), !.
cn_pl(_:X, X). % BUG: fallback


tv_sg(X, Y) :- get_lexicon_entry('TV_sg', X, Y), !.
tv_sg(_:X, X). % BUG: fallback

tv_pl(X, Y) :- get_lexicon_entry('TV_pl', X, Y), !.
tv_pl(_:X, X). % BUG: fallback

tv_vbg(X, Y) :- get_lexicon_entry('TV_vbg', X, Y), !.
tv_vbg(_:X, X). % BUG: fallback


%% set_lexicon_entry(+Type:atom, +Iri:term, +Form:atom) is det.
%
set_lexicon_entry(entry(Type, NS:Lemma, Form)) :-
	concat_atom([Type, NS, Lemma], Id),
	nb_setval(Id, Form).


%% get_lexicon_entry(+Type:atom, +Iri:term, -Form:atom) is det.
%
% TODO: with better indexing we can avoid concat_atom/2
% if it proves to be too slow.
%
get_lexicon_entry(Type, NS:Lemma, Form) :-
	concat_atom([Type, NS, Lemma], Id),
	catch(nb_getval(Id, Form), _, fail).


%% set_default_ns(+NS:atom) is det.
%
set_default_ns(NS) :-
	nb_setval(default_ns, NS).


%% get_default_ns(+NS:atom) is det.
%
% @throws error if NS is not a set variable
%
% TODO: currently not used
get_default_ns(NS) :-
	nb_getval(default_ns, NS).
