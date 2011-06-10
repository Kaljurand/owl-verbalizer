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
		init_lexicon/1,
		pn_sg/2,
		cn_sg/2,
		cn_pl/2,
		tv_sg/2,
		tv_pl/2,
		tv_vbg/2
	]).


/** <module> Lexicon

@author Kaarel Kaljurand
@version 2011-06-10

Manages the mapping of OWL IRIs to ACE common word surface forms.
The mapping is instantiated from a list of OWL annotation assertion axioms
found in the ontology.

The supported surface forms are of the following type:

* (singular) proper name,
* singular noun,
* plural noun,
* singular verb,
* plural verb (= infinitive verb),
* past participle (passive participle) verb.

This mapping must obey certain rules:

- it must be bidirectional within the same word class
- it must not generate forms which are not legal ACE tokens

We do not check if these rules are followed, it is up to the
user to make sure that they are.

*/

%% init_lexicon(+AxiomList:list) is det.
%% init_lexicon_loop(+AxiomList:list) is det.
%
% Instantiates the lexicon from a given list of entity annotation axioms.
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
init_lexicon(AxiomList) :-
	clear_lexicon,
	init_lexicon_loop(AxiomList).


init_lexicon_loop([]).

% @deprecated (OWL-API 2 style): use AnnotationAssertion
init_lexicon_loop(['EntityAnnotation'(Entity, 'Annotation'(Feature, '^^'(Form, _))) | AxiomList]) :-
	make_lex_entry(Entity, Feature, Form, Entry),
	!,
	set_lexicon_entry(Entry),
	init_lexicon_loop(AxiomList).

% AnnotationAssertion(AnnotationProperty(sg), IRI(#John), ^^(John, http://www.w3.org/2001/XMLSchema#string))
init_lexicon_loop(['AnnotationAssertion'('AnnotationProperty'(Property), 'IRI'(Iri), '^^'(WordForm, _)) | AxiomList]) :-
	make_lex_entry(Iri, Property, WordForm, Entry),
	!,
	set_lexicon_entry(Entry),
	init_lexicon_loop(AxiomList).

init_lexicon_loop([_Axiom | AxiomList]) :-
	% format("Ignoring: ~w~n", [Axiom]),
	init_lexicon_loop(AxiomList).


%% clear_lexicon is det.
%
% Removes all the lexicon entries.
%
clear_lexicon :-
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
% @param Iri is an IRI, e.g. http://example.org/test#man
% @param MorphType is an IRI for an ACE morphological type, one of {PN_sg, CN_sg, CN_pl, TV_sg, TV_pl, TV_vbg}
% @param WordForm is an ACE surface word-form
% @param LexiconEntry is a lexicon entry
%
make_lex_entry(Iri, 'http://attempto.ifi.uzh.ch/ace_lexicon#PN_sg', Form, entry('PN_sg', Iri, Form)).
make_lex_entry(Iri, 'http://attempto.ifi.uzh.ch/ace_lexicon#CN_sg', Form, entry('CN_sg', Iri, Form)).
make_lex_entry(Iri, 'http://attempto.ifi.uzh.ch/ace_lexicon#CN_pl', Form, entry('CN_pl', Iri, Form)).
make_lex_entry(Iri, 'http://attempto.ifi.uzh.ch/ace_lexicon#TV_sg', Form, entry('TV_sg', Iri, Form)).
make_lex_entry(Iri, 'http://attempto.ifi.uzh.ch/ace_lexicon#TV_pl', Form, entry('TV_pl', Iri, Form)).
make_lex_entry(Iri, 'http://attempto.ifi.uzh.ch/ace_lexicon#TV_vbg', Form, entry('TV_vbg', Iri, Form)).


%% pn_sg(+Iri, +WordForm) is det.
%% cn_sg(+Iri, +WordForm) is det.
%% cn_pl(+Iri, +WordForm) is det.
%% tv_sg(+Iri, +WordForm) is det.
%% tv_pl(+Iri, +WordForm) is det.
%% tv_vbg(+Iri, +WordForm) is det.
%
% Interface to the morph. mapping
% with a fallback to using the IRI fragment.
%
pn_sg(X, Y) :- get_lexicon_entry('PN_sg', X, Y), !.
pn_sg(Iri, F) :-
	iri_fragment(Iri, F).

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
cn_sg(Iri, F) :-
	iri_fragment(Iri, F).

cn_pl(X, Y) :- get_lexicon_entry('CN_pl', X, Y), !.
cn_pl(Iri, F) :-
	iri_fragment(Iri, F).


tv_sg(X, Y) :- get_lexicon_entry('TV_sg', X, Y), !.
tv_sg(Iri, F) :-
	iri_fragment(Iri, F).

tv_pl(X, Y) :- get_lexicon_entry('TV_pl', X, Y), !.
tv_pl(Iri, F) :-
	iri_fragment(Iri, F).

tv_vbg(X, Y) :- get_lexicon_entry('TV_vbg', X, Y), !.
tv_vbg(Iri, F) :-
	iri_fragment(Iri, F).


%% iri_fragment(+Iri:atom, -Fragment:atom)
%
iri_fragment(Iri, Fragment) :-
	concat_atom([_, Fragment], '#', Iri),
	!.

% This always succeeds
iri_fragment(Iri, Fragment) :-
	concat_atom(Parts, '/', Iri),
	last(Parts, Fragment).


%% set_lexicon_entry(+Type:atom, +Iri:atom, +Form:atom) is det.
%
set_lexicon_entry(entry(Type, Iri, Form)) :-
	concat_atom([Type, Iri], Id),
	nb_setval(Id, Form).


%% get_lexicon_entry(+Type:atom, +Iri:atom, -Form:atom) is det.
%
% TODO: with better indexing we can avoid concat_atom/2
% if it proves to be too slow.
%
get_lexicon_entry(Type, Iri, Form) :-
	concat_atom([Type, Iri], Id),
	catch(nb_getval(Id, Form), _, fail).


%% set_default_ns(+NS:atom) is det.
%
% TODO: currently not used
set_default_ns(NS) :-
	nb_setval(default_ns, NS).


%% get_default_ns(+NS:atom) is det.
%
% @throws error if NS is not a set variable
%
% TODO: currently not used
get_default_ns(NS) :-
	nb_getval(default_ns, NS).
