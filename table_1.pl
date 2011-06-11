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


:- module(table_1, [
		table_1/2
	]).


/** <module> Table 1

@author Kaarel Kaljurand
@version 2011-06-11
@license LGPLv3

TODO:

* Handle inv(inv(...))
* Handle different orderings of arguments in some axioms, e.g. ClassAssertion,
Domain, Range, property assertions
* Apply list_to_set/2 where appropriate
* check the cases where a set/list must have more than 1 element according to the spec
*/


%% table_1(+Axiom:term, -Axioms:list) is semidet.
%
% This module rewrites axioms into a syntactically different form
% while preserving their meaning. For example
%
%==
% EquivalentClasses'(['Class'('http://www.w3.org/2002/07/owl#Thing'), C])
%==
%
% is rewritten into
%
%==
% SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), C)
%==
%
% Note that sometimes we need to make several axioms from one,
% that is why a list of axioms is returned. Some of those cases where
% an axiom maps to a list of axioms are not implemented though.
%
% Also, there is no implementation for most data property axioms, the
% mapping simply fails in those cases.
%
% Note that SubClassOf-axioms are further rewritten in rewrite_subclassof.pl.
%
% @param Axiom is an OWL axiom
% @param Axioms is a list of OWL axioms
%

table_1(AxiomWithList, Axioms) :-
	axiom_with_list_to_axiom_with_set(AxiomWithList, AxiomWithSet),
	!,
	table_1_x(AxiomWithSet, Axioms).

% SubClassOf will not be changed here.
table_1('SubClassOf'(C, D), [
	'SubClassOf'(C, D)
	]).

table_1('DisjointClasses'([C, D]), [
	'SubClassOf'(C, 'ObjectComplementOf'(D))
	]).

% BUG: implement also the general case
% table_1('DisjointClasses'(Classes), [ ]).

% table_1('DisjointUnion'(Classes), [ ]).

% SubPropertyOf will not be changed here.
table_1('SubObjectPropertyOf'('ObjectPropertyChain'(ObjectProperties), S), [
	'SubObjectPropertyOf'('ObjectPropertyChain'(ObjectProperties), S)
	]) :- !.

table_1('SubObjectPropertyOf'(R, S), [
	'SubObjectPropertyOf'(R, S)
	]).

table_1('EquivalentObjectProperties'([R1, R2]), [
	'SubObjectPropertyOf'(R1, R2),
	'SubObjectPropertyOf'(R2, R1)
	]).

% BUG: implement also the general case
% table_1('EquivalentObjectProperties'(ObjectPropertyList), [ ]).

table_1('DisjointObjectProperties'([R, S]), [
	'DisjointObjectProperties'([R, S])
	]).

% BUG: implement also the general case
% table_1('DisjointObjectProperties'(ObjectPropertyList), [ ]).

table_1('ObjectPropertyDomain'(R, C), [
	'SubClassOf'('ObjectSomeValuesFrom'(R, 'Class'('http://www.w3.org/2002/07/owl#Thing')), C)
	]).

table_1('ObjectPropertyRange'('ObjectInverseOf'('ObjectProperty'(R)), C), [
	'SubClassOf'('ObjectSomeValuesFrom'('ObjectProperty'(R), 'Class'('http://www.w3.org/2002/07/owl#Thing')), C)
	]).

table_1('ObjectPropertyRange'('ObjectProperty'(R), C), [
	'SubClassOf'('ObjectSomeValuesFrom'('ObjectInverseOf'('ObjectProperty'(R)), 'Class'('http://www.w3.org/2002/07/owl#Thing')), C)
	]).

% BUG: doesn't support ObjectInverseOf as argument
table_1('InverseObjectProperties'('ObjectProperty'(R), 'ObjectProperty'(S)), [
	'SubObjectPropertyOf'('ObjectProperty'(R), 'ObjectInverseOf'('ObjectProperty'(S))),
	'SubObjectPropertyOf'('ObjectProperty'(S), 'ObjectInverseOf'('ObjectProperty'(R)))
	]).

table_1('FunctionalObjectProperty'(R), [
	'SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), 'ObjectMaxCardinality'(1, R, 'Class'('http://www.w3.org/2002/07/owl#Thing')))
	]).

table_1('InverseFunctionalObjectProperty'('ObjectInverseOf'('ObjectProperty'(R))), [
	'SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), 'ObjectMaxCardinality'(1, 'ObjectProperty'(R), 'Class'('http://www.w3.org/2002/07/owl#Thing')))
	]).

table_1('InverseFunctionalObjectProperty'('ObjectProperty'(R)), [
	'SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), 'ObjectMaxCardinality'(1, 'ObjectInverseOf'('ObjectProperty'(R)), 'Class'('http://www.w3.org/2002/07/owl#Thing')))
	]).

table_1('ReflexiveObjectProperty'(R), [
	'SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), 'ObjectHasSelf'(R))
	]).

table_1('IrreflexiveObjectProperty'(R), [
	'SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), 'ObjectComplementOf'('ObjectHasSelf'(R)))
	]).

table_1('SymmetricObjectProperty'('ObjectInverseOf'('ObjectProperty'(R))), [
	'SubObjectPropertyOf'('ObjectInverseOf'('ObjectProperty'(R)), 'ObjectProperty'(R))
	]).

table_1('SymmetricObjectProperty'('ObjectProperty'(R)), [
	'SubObjectPropertyOf'('ObjectProperty'(R), 'ObjectInverseOf'('ObjectProperty'(R)))
	]).

table_1('AsymmetricObjectProperty'('ObjectInverseOf'('ObjectProperty'(R))), [
	'DisjointObjectProperties'(['ObjectInverseOf'('ObjectProperty'(R)), 'ObjectProperty'(R)])
	]).

table_1('AsymmetricObjectProperty'('ObjectProperty'(R)), [
	'DisjointObjectProperties'(['ObjectProperty'(R), 'ObjectInverseOf'('ObjectProperty'(R))])
	]).

table_1('TransitiveObjectProperty'(R), [
	'SubObjectPropertyOf'('ObjectPropertyChain'([R, R]), R)
	]).

% BUG: future work
% table_1('SubDataPropertyOf'(U V), [ ]).

% BUG: future work
% table_1('EquivalentDataProperties'(DataProperties), [ ]).

% BUG: future work
% table_1('DisjointDataProperties'(DataProperties), [ ]).

% BUG: future work
% table_1('DataPropertyDomain'(U, C), [ ]).

% BUG: future work
% table_1('DataPropertyRange'(U, DR), [ ]).

% BUG: future work
% table_1('FunctionalDataProperty'(U), [ ]).



% BUG: We assume that individual comes first.
% Spec is ambiguous currently.
table_1('ClassAssertion'('NamedIndividual'(A), C), [
	'SubClassOf'('ObjectOneOf'(['NamedIndividual'(A)]), C)
	]).

% BUG: We also support the other order.
table_1('ClassAssertion'(C, 'NamedIndividual'(A)), [
	'SubClassOf'('ObjectOneOf'(['NamedIndividual'(A)]), C)
	]).

table_1('ClassAssertion'('AnonymousIndividual'(A), C), [
	'SubClassOf'('ObjectOneOf'(['AnonymousIndividual'(A)]), C)
	]).

% BUG: We also support the other order.
table_1('ClassAssertion'(C, 'AnonymousIndividual'(A)), [
	'SubClassOf'('ObjectOneOf'(['AnonymousIndividual'(A)]), C)
	]).

table_1('ObjectPropertyAssertion'(R, A, B), [
	'SubClassOf'(
		'ObjectOneOf'([A]),
		'ObjectSomeValuesFrom'(R, 'ObjectOneOf'([B]))
	)
	]).

table_1('NegativeObjectPropertyAssertion'(R, A, B), [
	'SubClassOf'(
		'ObjectOneOf'([A]),
		'ObjectComplementOf'('ObjectSomeValuesFrom'(R, 'ObjectOneOf'([B])))
	)
	]).

table_1('DataPropertyAssertion'(DataProperty, Individual, DataValue), [
	'SubClassOf'(
		'ObjectOneOf'([Individual]),
		'DataHasValue'(DataProperty, DataValue)
	)
	]).

table_1('NegativeDataPropertyAssertion'(DataProperty, Individual, DataValue), [
	'SubClassOf'(
		'ObjectOneOf'([Individual]),
		'ObjectComplementOf'('DataHasValue'(DataProperty, DataValue))
	)
	]).

% Classes that are equivalent to owl:Nothing
table_1_x('EquivalentClasses'(ClassSet), SubClassOfList) :-
	ClassSet = [_, _ | _],
	select('Class'('http://www.w3.org/2002/07/owl#Nothing'), ClassSet, ClassSetWithoutNothing),
	!,
	findall(
		'SubClassOf'(Class, 'Class'('http://www.w3.org/2002/07/owl#Nothing')),
		member(Class, ClassSetWithoutNothing),
		SubClassOfList
	).

% Classes that are equivalent to owl:Thing
table_1_x('EquivalentClasses'(ClassSet), SubClassOfList) :-
	ClassSet = [_, _ | _],
	select('Class'('http://www.w3.org/2002/07/owl#Thing'), ClassSet, ClassSetWithoutThing),
	!,
	findall(
		'SubClassOf'('Class'('http://www.w3.org/2002/07/owl#Thing'), Class),
		member(Class, ClassSetWithoutThing),
		SubClassOfList
	).

table_1_x('EquivalentClasses'(ClassSet), SubClassOfList) :-
	ClassSet = [_, _ | _],
	findall(
		'SubClassOf'(Class, 'ObjectIntersectionOf'(RemainingClassSet)),
		select(Class, ClassSet, RemainingClassSet),
		SubClassOfList
	).

% BUG: maybe we should not do this
table_1_x('SameIndividual'([A]), [
		'SubClassOf'('ObjectOneOf'([A]), 'ObjectOneOf'([A]))
		]) :- !.

table_1_x('SameIndividual'([A1, A2]), [
		'SubClassOf'('ObjectOneOf'([A1]), 'ObjectOneOf'([A2]))
		]) :- !.

table_1_x('SameIndividual'(IndividualSet), SubClassOfList) :-
	IndividualSet = [_, _ | _],
	findall(
		'SubClassOf'('ObjectOneOf'([Individual]), 'ObjectOneOf'(RemainingIndividualSet)),
		select(Individual, IndividualSet, RemainingIndividualSet),
		SubClassOfList
	).

table_1_x('DifferentIndividuals'(IndividualSet), AxiomList) :-
	IndividualSet = [_, _ | _],
	findall(
		'ObjectOneOf'([Individual]),
		member(Individual, IndividualSet),
		OneOfList
	),
	table_1('DisjointClasses'(OneOfList), AxiomList).


%% axiom_with_list_to_axiom_with_set(+AxiomWithList:term, -AxiomWithSet:term) is det.
%
% Transforms an axiom that contains a list into the same axiom but the list is replaced
% by a set. This transformation is applied only to certain axioms.
%
% @param AxiomWithList is one of {EquivalentClasses, SameIndividual, DifferentIndividuals}
% @param AxiomWithSet is the given axiom with its argument list converted into set
%
axiom_with_list_to_axiom_with_set('EquivalentClasses'(ClassList), 'EquivalentClasses'(ClassSet)) :-
	list_to_set(ClassList, ClassSet).

axiom_with_list_to_axiom_with_set('SameIndividual'(IndividualList), 'SameIndividual'(IndividualSet)) :-
	list_to_set(IndividualList, IndividualSet).

axiom_with_list_to_axiom_with_set('DifferentIndividuals'(IndividualList), 'DifferentIndividuals'(IndividualSet)) :-
	list_to_set(IndividualList, IndividualSet).

% @deprecated: use: SameIndividual
axiom_with_list_to_axiom_with_set('SameIndividuals'(IndividualList), 'SameIndividual'(IndividualSet)) :-
	list_to_set(IndividualList, IndividualSet).
