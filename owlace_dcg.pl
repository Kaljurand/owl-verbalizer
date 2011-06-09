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

:- module(owlace_dcg, [
		owl_ace/2
	]).

/** <module> Definite Clause Grammar for an OWL-compatible fragment of ACE

Converts a sentence in a fragment of ACE into an axiom in
a syntactic fragment of OWL 2. And vice versa.

_|Note: only the OWL->ACE direction has been tested.|_

Things to discuss:

* Ambiguity of RelCl and coordination.
* Comma-and vs and. Use comma-and only when disjunction is around.

Things to test:

* Roundtripping OWL->ACE->OWL (generate OWL with owl_generator.pl)
* Roundtripping ACE->OWL->ACE (generate ACE with this grammar)
* How many solutions from ACE->OWL? Is the first one correct?
* How many solutions from OWL->ACE? Is the first one correct?
* Compatibility with ACE semantics (i.e. treatment of relative clauses and coordinations)

TEST:

==
owl_ace(Owl, ['Every', protein, modifies, exactly, 1, protein, that, are, modified, by, a, protein, '.']), write(Owl), nl.
==

Syntactic roundtripping does not work, e.g. in the case of: 'that' vs 'and that':

==
[Met, is, a, thing, that, is, a, thing, that, does, not, modify, a, thing, .]
[Met, is, a, thing, that, is, a, thing, and, that, does, not, modify, a, thing, .]
==

@author Kaarel Kaljurand
@version 2011-06-09

*/


%% from_ace_to_owl(+ACE:atom, -OWL:term)
%
% Translates ACE to OWL. Experimental.
%
% @bug Uses a simple tokenizer to split an atom into a list.
% One would we need to convert numbers to Prolog numbers
% (with atom_number/2) afterwards.
%
/*
from_ace_to_owl(ACE, OWL) :-
	concat_atom(TokenizedACE, ' ', ACE),
	ace_merge(InternalACE, [], TokenizedACE),
	owl_ace(OWL, InternalACE).
*/



%% owl_ace(+OWL:term, -ACE:list) is nondet.
%% owl_ace(-OWL:term, +ACE:list) is nondet.
%
% Front-end to the DCG.
%
% An example of verbalizing an OWL axiom as an ACE sentence.
%
%==
% ?- owlace_dcg:owl_ace('SubClassOf'('Class'(protein),'ObjectSomeValuesFrom'('ObjectInverseOf'('ObjectProperty'(modify)),'ObjectOneOf'(['NamedIndividual'('Met')]))),ACE).
%
%ACE = ['Every', protein, is, modify, by, 'Met', '.'] ;
%==
%
% An example of using ACE to express OWL axioms.
% Due to coordination ambiguity there can be several solutions. The solution
% that is compatible with the ACE interpretation rules (i.e. the relative clause
% modifies to the closest preceding noun) is always the first solution.
%
% BUG: this comes out wrong as top-level OR is the first solution. I.e.
% don't use this grammar to do the ACE->OWL translation, but only the
% OWL->ACE translation!!!
%
%==
%
%?- owl_ace(OWL, ['Every', 'protein', 'is', 'a', 'thing', 'that', 'modifies', 'a', 'thing', 'that', 'is', 'Met', 'or', 'that', 'is', 'Met', '.']).
%
%OWL = 'SubClassOf'(protein, 'ObjectIntersectionOf'(['owl:Thing', 'ObjectSomeValuesFrom'(modify, 'ObjectIntersectionOf'('owl:Thing', 'ObjectUnionOf'('ObjectOneOf'(['Met']), 'ObjectOneOf'(['Met']))))])) ;
%
%OWL = 'SubClassOf'(protein, 'ObjectIntersectionOf'(['owl:Thing', 'ObjectUnionOf'('ObjectSomeValuesFrom'(modify, 'ObjectIntersectionOf'('owl:Thing', 'ObjectOneOf'(['Met']))), 'ObjectOneOf'(['Met']))])) ;
%No
%==
%
% @param OWL is an OWL axiom in the OWL 2 Functional-Style Syntax (Prolog notation)
% @param ACE is an ACE sentence represented as a list of atoms
%

owl_ace(OWL, ACE) :-
	ip(OWL, ACE, []).


%
% SYNTAX
%

ip(
	'SubObjectPropertyOf'('ObjectPropertyChain'(PropertyChain), 'ObjectProperty'(S)),
	TokenList,
	[]
) :-
	propertychain_verbchain(PropertyChain, [something, that | Tail]),
	TokenListBeginning = ['If', 'X' | Tail],
	append(TokenListBeginning, ['Y', then, 'X', tv_sg(S), 'Y', '.'], TokenList).

ip(
	'SubObjectPropertyOf'('ObjectPropertyChain'(PropertyChain), 'ObjectInverseOf'('ObjectProperty'(S))),
	TokenList,
	[]
) :-
	propertychain_verbchain(PropertyChain, [something, that | Tail]),
	TokenListBeginning = ['If', 'X' | Tail],
	append(TokenListBeginning, ['Y', then, 'X', is, tv_vbg(S), by, 'Y', '.'], TokenList).

% BUG: use passive to be consistent with the thesis
ip('SubObjectPropertyOf'('ObjectInverseOf'('ObjectProperty'(R)), 'ObjectInverseOf'('ObjectProperty'(S)))) -->
	['If', 'Y', tv_sg(R), 'X', then, 'Y', tv_sg(S), 'X', '.'].

ip('SubObjectPropertyOf'('ObjectInverseOf'('ObjectProperty'(R)), 'ObjectProperty'(S))) -->
	['If', 'Y', tv_sg(R), 'X', then, 'X', tv_sg(S), 'Y', '.'].

ip('SubObjectPropertyOf'('ObjectProperty'(R), 'ObjectInverseOf'('ObjectProperty'(S)))) -->
	['If', 'X', tv_sg(R), 'Y', then, 'Y', tv_sg(S), 'X', '.'].

ip('SubObjectPropertyOf'('ObjectProperty'(R), 'ObjectProperty'(S))) -->
	['If', 'X', tv_sg(R), 'Y', then, 'X', tv_sg(S), 'Y', '.'].

% BUG: use passive to be consistent with the thesis
ip('DisjointObjectProperties'(['ObjectInverseOf'('ObjectProperty'(R)), 'ObjectInverseOf'('ObjectProperty'(S))])) -->
	['If', 'Y', tv_sg(R), 'X', then, it, is, false, that, 'Y', tv_sg(S), 'X', '.'].

ip('DisjointObjectProperties'(['ObjectInverseOf'('ObjectProperty'(R)), 'ObjectProperty'(S)])) -->
	['If', 'Y', tv_sg(R), 'X', then, it, is, false, that, 'X', tv_sg(S), 'Y', '.'].

ip('DisjointObjectProperties'(['ObjectProperty'(R), 'ObjectInverseOf'('ObjectProperty'(S))])) -->
	['If', 'X', tv_sg(R), 'Y', then, it, is, false, that, 'Y', tv_sg(S), 'X', '.'].

ip('DisjointObjectProperties'(['ObjectProperty'(R), 'ObjectProperty'(S)])) -->
	['If', 'X', tv_sg(R), 'Y', then, it, is, false, that, 'X', tv_sg(S), 'Y', '.'].


ip(SubClassOf) -->
	np_subj(Y, SubClassOf),
	ibar(num=sg, Y),
	['.'].


ibar(Num, C2) -->
	auxc(Num, C1, C2),
	cop(C1).

ibar(Num, C2) -->
	auxv(Num, Neg, Vbn, C1, C2),
	vp(Num, Neg, Vbn, C1).


cop('ObjectOneOf'([Individual])) -->
	propn('ObjectOneOf'([Individual])).

cop('ObjectIntersectionOf'([C1, C2])) -->
	[a],
	n(num=sg, C1),
	relcoord(num=sg, C2).

cop(C) -->
	[a],
	n(num=sg, C).


vp(Num, Neg, Vbn, Restriction) -->
	tv(Num, Neg, Vbn, R),
	np_obj(Num, R, Restriction).


np_subj(C, 'SubClassOf'('ObjectOneOf'([Individual]), C)) -->
	propn('ObjectOneOf'([Individual])).

np_subj(Y, 'SubClassOf'(C, D)) -->
	det_subj(C, Y, 'SubClassOf'(C, D)),
	n(num=sg, C).

np_subj(Y, 'SubClassOf'(C, D)) -->
	det_subj('ObjectIntersectionOf'([X, Coord]), Y, 'SubClassOf'(C, D)),
	n(num=sg, X),
	relcoord(num=sg, Coord).


np_obj(_, R, 'ObjectSomeValuesFrom'(R, 'ObjectOneOf'([Individual]))) -->
	propn('ObjectOneOf'([Individual])).

np_obj(_, R, 'DataHasValue'(R, '^^'(Integer, 'http://www.w3.org/2001/XMLSchema#integer'))) -->
	[Integer],
	{ integer(Integer) }.

np_obj(_, R, 'DataHasValue'(R, '^^'(Double,  'http://www.w3.org/2001/XMLSchema#double'))) -->
	[Double],
	{ float(Double) }.

np_obj(_, R, 'DataHasValue'(R, '^^'(String,  'http://www.w3.org/2001/XMLSchema#string'))) -->
	[qs(String)],
	{ atom(String) }.

np_obj(num=sg, R, 'ObjectHasSelf'(R)) -->
	[itself].

np_obj(num=pl, R, 'ObjectHasSelf'(R)) -->
	[themselves].

np_obj(_, R, Restriction) -->
	det_obj(Num, R, C, Restriction),
	n(Num, C).

np_obj(_, R, Restriction) -->
	det_obj(Num, R, 'ObjectIntersectionOf'([C, Coord]), Restriction),
	n(Num, C),
	relcoord(Num, Coord).


% Relative clauses
%
% Note that only binary and/or are supported. The binding order
% is implemented by multiple levels of rules.
%
% We need to specify the cases where we need comma-and.
%
% [ A or B , and C ]
% [ A , and B or C ]
% [ A or B , and C or D ]
%
% What about:
%
% [ A and B , and C ]
% [ A , and B and C ]
%
% and other cases which a more complex.

% BUG: Here we restrict the use of comma-and only to cases where
% there is a UnionOf nearby.
relcoord(Num, 'ObjectIntersectionOf'(['ObjectUnionOf'(CL1), 'ObjectUnionOf'(CL2)])) -->
	relcoord_1(Num, 'ObjectUnionOf'(CL1)),
	[',', and],
	relcoord_1(Num, 'ObjectUnionOf'(CL2)).

relcoord(Num, 'ObjectIntersectionOf'(['ObjectUnionOf'(CL1), C2])) -->
	relcoord_1(Num, 'ObjectUnionOf'(CL1)),
	[',', and],
	relcoord_2(Num, C2).

relcoord(Num, 'ObjectIntersectionOf'([C1, 'ObjectUnionOf'(CL2)])) -->
	relcoord_2(Num, C1),
	[',', and],
	relcoord_1(Num, 'ObjectUnionOf'(CL2)).

relcoord(Num, Coord) -->
	relcoord_1(Num, Coord).


relcoord_1(Num, 'ObjectUnionOf'([C1, C2])) -->
	relcoord_2(Num, C1),
	[or],
	relcoord_1(Num, C2).

relcoord_1(Num, Coord) -->
	relcoord_2(Num, Coord).


relcoord_2(Num, 'ObjectIntersectionOf'([C1, C2])) -->
	rel(Num, C1),
	[and],
	relcoord_2(Num, C2).

relcoord_2(Num, Coord) -->
	rel(Num, Coord).


/*
relcoord(Num, 'ObjectIntersectionOf'([C1, C2])) -->
	relcoord_1(Num, C1),
	comma_and(C1, C2, 'ObjectIntersectionOf'([C1, C2])),
	relcoord(Num, C2).

relcoord(Num, Coord) -->
	relcoord_1(Num, Coord).

relcoord_1(Num, 'ObjectUnionOf'([C1, C2])) -->
	relcoord_2(Num, C1),
	or(C1, C2, 'ObjectUnionOf'([C1, C2])),
	relcoord_1(Num, C2).

relcoord_1(Num, Coord) -->
	relcoord_2(Num, Coord).

relcoord_2(Num, 'ObjectIntersectionOf'([C1, C2])) -->
	rel(Num, C1),
	and(C1, C2, 'ObjectIntersectionOf'([C1, C2])),
	relcoord_2(Num, C2).
relcoord_2(Num, Coord) -->
	rel(Num, Coord).
*/


rel(Num, X) -->
	[that],
	ibar(Num, X).


%
% FORMAL LEXICON
%

% Syntactic sugar 'no' could be implemented like this:
det_subj(C1, C2, 'SubClassOf'(C1, 'ObjectComplementOf'(C2))) -->
	['No'].

det_subj(C1, C2, 'SubClassOf'(C1, C2)) -->
	['Every'].

% General number rule. Note that we do not support the number 0.
det_obj(num=sg, R, C, 'ObjectSomeValuesFrom'(R, C)) -->
	[a].
det_obj(num=pl, R, C, 'ObjectAllValuesFrom'(R, C)) -->
	[nothing, but].
det_obj(num=sg, R, C, 'ObjectMinCardinality'(1, R, C)) -->
	[at, least, 1].
det_obj(num=sg, R, C, 'ObjectMaxCardinality'(1, R, C)) -->
	[at, most, 1].
det_obj(num=sg, R, C, 'ObjectExactCardinality'(1, R, C)) -->
	[exactly, 1].
det_obj(num=pl, R, C, 'ObjectMinCardinality'(Integer, R, C)) -->
	[at, least, Integer],
	{ between(2, infinite, Integer) }.
det_obj(num=pl, R, C, 'ObjectMaxCardinality'(Integer, R, C)) -->
	[at, most, Integer],
	{ between(2, infinite, Integer) }.
det_obj(num=pl, R, C, 'ObjectExactCardinality'(Integer, R, C)) -->
	[exactly, Integer],
	{ between(2, infinite, Integer) }.

auxc(num=sg, C, C) -->
	[is].
auxc(num=pl, C, C) -->
	[are].
auxc(num=sg, C, 'ObjectComplementOf'(C)) -->
	[is, not].
auxc(num=pl, C, 'ObjectComplementOf'(C)) -->
	[are, not].

auxv(num=sg, neg=yes, vbn=no, C, 'ObjectComplementOf'(C)) -->
	[does, not].
auxv(num=pl, neg=yes, vbn=no, C, 'ObjectComplementOf'(C)) -->
	[do, not].
auxv(num=sg, neg=yes, vbn=yes, C, 'ObjectComplementOf'(C)) -->
	[is, not].
auxv(num=pl, neg=yes, vbn=yes, C, 'ObjectComplementOf'(C)) -->
	[are, not].
auxv(num=sg, neg=no, vbn=yes, C, C) -->
	[is].
auxv(num=pl, neg=no, vbn=yes, C, C) -->
	[are].
auxv(_, neg=no, vbn=no, C, C) -->
	[].

comma_and(C1, C2, 'ObjectIntersectionOf'([C1, C2])) -->
	[',', and].
and(C1, C2, 'ObjectIntersectionOf'([C1, C2])) -->
	[and].
or(C1, C2, 'ObjectUnionOf'([C1, C2])) -->
	[or].


%
% CONTENT LEXICON: propn//1, n//2, tv//4
%

% Proper names
propn('ObjectOneOf'(['NamedIndividual'(Lemma)])) -->
	[pn_sg(Lemma)].

/*
TODO: this way of handling Anonymous individuals does not work:
1) they must be ACE variables (of the form [A-Z][0-9]*)
2) in OWL axiom ordering does not matter, but we need to make
sure that a ClassAssertion is verbalized before SubClassOf in case both
reference the same anonymous individual

propn('ObjectOneOf'(['AnonymousIndividual'(NodeId)])) -->
	[NodeId].
*/

% Nouns (including `something')
% For nouns we have to describe 2 forms: {num=sg, num=pl}
n(num=sg, 'Class'('http://www.w3.org/2002/07/owl#Thing')) -->
	[thing].
n(num=pl, 'Class'('http://www.w3.org/2002/07/owl#Thing')) -->
	[things].
n(num=sg, 'Class'(Lemma)) -->
	[cn_sg(Lemma)].
n(num=pl, 'Class'(Lemma)) -->
	[cn_pl(Lemma)].


% Transitive verbs
%
% For verbs we have to describe 8 forms:
% {neg=yes, neg=no} * {num=sg, num=pl} * {vbn=no, vbn=yes}

% modifies
% modify
% does not modify
% do not modify
% is modified by
% are modified by
% is not modified by
% are not modified by

% Note that the (external) lexicon has to provide only 3 forms:
% finite form, infinitive, and past participle (e.g. modifies, modify, modified).

tv(num=sg, neg=no, vbn=no, 'ObjectProperty'(Lemma)) -->
	[tv_sg(Lemma)].
tv(num=pl, neg=no, vbn=no, 'ObjectProperty'(Lemma)) -->
	[tv_pl(Lemma)].
tv(num=sg, neg=yes, vbn=no, 'ObjectProperty'(Lemma)) -->
	[tv_pl(Lemma)].
tv(num=pl, neg=yes, vbn=no, 'ObjectProperty'(Lemma)) -->
	[tv_pl(Lemma)].
tv(_, _, vbn=yes, 'ObjectInverseOf'('ObjectProperty'(Lemma))) -->
	[tv_vbg(Lemma), by].

% BUG: DataProperties are treated in the same was as object properties,
% i.e. with transitive verbs
% BUG: why don't we use tv_sg and tv_vbg here?
tv(num=sg, neg=no, vbn=no, 'DataProperty'(Lemma)) -->
	[tv_pl(Lemma)].
tv(num=pl, neg=no, vbn=no, 'DataProperty'(Lemma)) -->
	[tv_pl(Lemma)].
tv(num=sg, neg=yes, vbn=no, 'DataProperty'(Lemma)) -->
	[tv_pl(Lemma)].
tv(num=pl, neg=yes, vbn=no, 'DataProperty'(Lemma)) -->
	[tv_pl(Lemma)].


%% propertychain_verbchain(?PropertyChain:list, ?VerbChain:list) is det.
%
% Example:
%
%==
% propertychain_verbchain(['ObjectProperty'(know), 'ObjectInverseOf'('ObjectProperty'(eat))], VerbChain).
%
% VerbChain = [something, that, knows, something, that, is, eaten, by] 
%==
%
% @param PropertyChain is a list of OWL property expressions
% @param VerbChain is a list of ACE tokens containing `something', `that', and the verb
%
propertychain_verbchain([], []).

propertychain_verbchain([Property | PropertyChainTail], VerbChain) :-
	property_verb(Property, VerbChainTail, VerbChain),
	propertychain_verbchain(PropertyChainTail, VerbChainTail).


%% property_verb(+Property:term, ?VerbChainTail:list, -VerbChain:list) is det.
%
% @param Property is an OWL property expression
% @param VerbChainTail is a list of ACE tokens containing `something', `that', and the verb
% @param VerbChain is a list of ACE tokens containing `something', `that', and the verb
%
property_verb('ObjectProperty'(R), VerbChainTail, [something, that, tv_sg(R) | VerbChainTail]).

property_verb('ObjectInverseOf'('ObjectProperty'(R)), VerbChainTail, [something, that, is, tv_vbg(R), by | VerbChainTail]).
