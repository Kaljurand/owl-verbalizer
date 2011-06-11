:- module(simple_axiom, [
	simple_axiom/1
	]).

/** <module> Simple OWL Axiom Generator

@author Kaarel Kaljurand
@version 2007-06-18

*/

%:- use_module(owlace_dcg).

%% simple_axiom(+OWLAxiom:term) is det.
%% simple_axiom(-OWLAxiom:term) is nondet.
%
% @param OWLAxiom is an OWL axiom in OWL Functional-Style Syntax (Prolog notation)
%
% Here we have a grammar for a fragment of OWL. In this fragment
% we can express OWL ontologies which can entail any OWL ontology
% that uses only SubClassOf-axioms and does not use any data properties.
%
% Provided that we have a finite number of names and numbers,
% the number of possible axioms in this language is finite. If we have only
% one class name (e.g. `protein'), one property name (e.g. `modify'),
% one individual name (e.g. `Met'), and one number (e.g. `2') then we
% have altogether 12 axioms.
%
%==
%findall(OwlAxiom, simple_axiom(OwlAxiom), List), length(List, 12).
%==
%
% BUG: experimentally, there are examples of the possible plural formulation of those axioms.
% The benefit of using plurals is that we do not need to have singular nouns and verbs
% in the lexicon. Also, we don't need to make the `a' vs `an' distinction.
% But, we would still need the participles.
% The requirement of 'are + some' is strange,
% maybe 'are + some' can be shortened to be 'are'. See also the "capers example".

% Every protein is a molecule.
% (All proteins are some molecules.)
simple_axiom('SubClassOf'(C, D)) :- owl_class(C), owl_class(D).

% No protein is a molecule.
% (No proteins are some molecules.)
simple_axiom('SubClassOf'(C, 'ObjectComplementOf'(D))) :- owl_class(C), owl_class(D).

% Everything that is not a protein is a molecule.
% (All things that are not some proteins are some molecules.)
simple_axiom('SubClassOf'('ObjectIntersectionOf'(['owl:Thing', 'ObjectComplementOf'(C)]), D)) :- owl_class(C), owl_class(D).

% Every protein is Met.
% (All proteins are Met.)
simple_axiom('SubClassOf'(C, 'ObjectOneOf'([I]))) :- owl_class(C), owl_individual(I).

% Everything that is Met is a protein.
% (All things that are Met are some proteins.)
% short form: Met is a protein. (??? Met are some proteins.)
simple_axiom('SubClassOf'('ObjectOneOf'([I]), C)) :- owl_individual(I), owl_class(C).

% Every protein modifies itself.
% (All proteins modify themselves.)
simple_axiom('SubClassOf'(C, 'ObjectExistsSelf'(R))) :- owl_class(C), owl_property(R).

% Everything that modifies itself is a protein.
% (All things that modify themselves are some proteins.)
simple_axiom('SubClassOf'('ObjectIntersectionOf'(['owl:Thing', 'ObjectExistsSelf'(R)]), C)) :- owl_property(R), owl_class(C).

% Every protein modifies at most 2 molecules.
% (All proteins modify at most 2 molecules.)
simple_axiom('SubClassOf'(C, 'ObjectMinCardinality'(Number, R, D))) :-
	owl_class(C), owl_number(Number), owl_property(R), owl_class(D).

% Everything that modifies at most 2 proteins is a molecule.
% (All things that modify at most 2 proteins are some molecules.)
simple_axiom('SubClassOf'('ObjectIntersectionOf'(['owl:Thing', 'ObjectMinCardinality'(Number, R, C)]), D)) :-
	owl_number(Number), owl_property(R), owl_class(C), owl_class(D).

% Every protein is modified by at most 2 molecules.
% (All proteins are modified by at most 2 molecules.)
simple_axiom('SubClassOf'(C, 'ObjectMinCardinality'(Number, 'InverseObjectProperty'(R), D))) :-
	owl_class(C), owl_number(Number), owl_property(R), owl_class(D).

% Everything that is modified by at most 2 proteins is a molecule.
% (All things that are modified by at most 2 proteins are some molecules.)
simple_axiom('SubClassOf'('ObjectIntersectionOf'(['owl:Thing', 'ObjectMinCardinality'(Number, 'InverseObjectProperty'(R), C)]), D)) :-
	owl_number(Number), owl_property(R), owl_class(C), owl_class(D).

% Everything that is a protein and that is a molecule is an amino-acid.
% (All things that are some proteins and that are some molecules are some amino-acids.)
simple_axiom('SubClassOf'('ObjectIntersectionOf'([C1, C2]), C3)) :-
	owl_class(C1), owl_class(C2), owl_class(C3).


owl_class(protein).
owl_property(modify).
owl_individual('Met').
owl_number(2).
