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

:- module(rewrite_subclassof, [
		rewrite_subclassof/2
	]).


/** <module> Rewrite SubClassOf

Rewrites OWL SubClassOf-axioms into a form that is
more suitable for direct verbalization.

Checks:

1. Can all OWL axioms be rewritten? (Check by testing.)
2. Is rewriting correct? (This is checked by proof.)
3. Can all OWL rewritten axioms be verbalized? (Check by testing.)

@author Kaarel Kaljurand
@version 2011-06-06

*/

%% rewrite_subclassof(+Axiom1:term, -Axiom2:term) is det.
%
% Rewriting the SubClassOf axioms in a semantics-preserving way.
% Changes are needed to handle complex structures which without
% changes could not be handled. Changes are also needed to be able
% to verbalize the axioms later into a more natural ACE sentences.
%
% @param Axiom1 is an OWL axiom
% @param Axiom2 is an OWL axiom
%
% @bug we should maybe remove owl:Thing from the right side (if it is in coordination,
% there is a named class (but not ObjectOneOf) as well).
%
% @bug we handle property axioms here as well, therefore: rename rewrite_subclassof/2

rewrite_subclassof('SubObjectPropertyOf'(R, S), 'SubObjectPropertyOf'(R, S)) :- !.

rewrite_subclassof('DisjointObjectProperties'([R, S]), 'DisjointObjectProperties'([R, S])) :- !.

% We add owl:Thing to be potentially next to a complex class.
% This makes the verbalization more direct.
% BUG: do we need this? maybe it's taken care of later anyway?
% BUG: do later
rewrite_subclassof('SubClassOf'(C1, C2), SubClassOf) :-
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C1]), NewC1),
	rewrite_class(C2, NewC2),
	rewrite_subclassof_x('SubClassOf'(NewC1, NewC2), SubClassOf).


%% rewrite_subclassof_x
%
% Note: these rewritings are done at the very end, after class expressions have
% already been rewritten. So, here we can make some assumptions about the
% structure of the class expressions (e.g. that the property expressions have
% been flattened as much as possible.
rewrite_subclassof_x(
	'SubClassOf'('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectComplementOf'(C1)]), 'ObjectComplementOf'(C2)),
	'SubClassOf'(C2, C1)
) :- !.

% No dog-cat is something. --> Nothing is a dog-cat.
rewrite_subclassof_x(
	'SubClassOf'(Class, 'ObjectComplementOf'('Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')))),
	'SubClassOf'('Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectComplementOf'(Class))
) :- !.

rewrite_subclassof_x(
	'SubClassOf'(C1, 'ObjectAllValuesFrom'('ObjectInverseOf'(R), C2)),
	'SubClassOf'('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectSomeValuesFrom'(R, C1)]), C2)
) :- !.

rewrite_subclassof_x(
	'SubClassOf'(C1, 'ObjectAllValuesFrom'(R, C2)),
	'SubClassOf'('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectSomeValuesFrom'('ObjectInverseOf'(R), C1)]), C2)
) :- !.

rewrite_subclassof_x(X, X).


% BUG: Possible rewriting:
% Motivation:
% No man that does not own a car is a truck-driver.
% No truck-driver is a man that does not own a car.
% BUG: implement: is_more_complex_than on the basis of natural_order/3
% BUG: this is not safe, we cannot assume that ObjectComplementOf will be preserved
%rewrite_subclassof(
%	'SubClassOf'(C1, 'ObjectComplementOf'(C2)),
%	'SubClassOf'(C2, 'ObjectComplementOf'(C1))
%) :- C1 is more complex than C2.


%% rewrite_classlist(+ClassList:list, -RewrittenClassList:list) is det.
%
% @param ClassList is a list of OWL classes
% @param RewrittenClassList is a list of OWL classes after being rewritten
%
rewrite_classlist([], []).

rewrite_classlist([Class | Classes], [NewClass | NewClasses]) :-
	rewrite_class(Class, NewClass),
	rewrite_classlist(Classes, NewClasses).


%% rewrite_class(+Class1:term, -Class2:term) is det.
%
% Note: We wrap all complex classes (apart from ObjectOneOf) into ObjectIntersectionOf with owl:Thing.
% We must think carefully where can we add the thing. We should try to keep the rules in
% such a way that they always make the class expression simpler. Otherwise the process does not
% terminate. Maybe we need to add it only to the left side of SubClassOf (in case there are no
% named classes). We also need it if the argument of a complex class expression is not a named class.
%
% Add: ordering in UnionOf + making it binary
% Add: ordering in IntersectionOf + making it binary
% Preserve: ObjectAllValuesFrom next to a negation
% Preserve: ObjectSomeValuesFrom next to a negation
% Think about it: ExactCardinality next to a negation (we can remove the negation then)
%
% @param Class1 is an OWL class expression
% @param Class2 is an OWL class expression 
%
% @bug add placeholders for data properties.
%
% @bug How can we rewrite: 'ObjectSomeValuesFrom'(modify, 'ObjectComplementOf'(':'('http://www.w3.org/2002/07/owl#', 'Thing')))
%
% @bug Should we remove the duplicates from ObjectOneOf (or even sort the individuals)? Yes.

rewrite_class(
	'Class'('http://www.w3.org/2002/07/owl#':'Nothing'),
	'ObjectComplementOf'('Class'('http://www.w3.org/2002/07/owl#':'Thing'))
) :- !.

% Removing double negation
% BUG: why don't we do atomic check here?
% BUG: not complete: the argument can be: and(Thing, not(_)) (fixed: but is it complete?)
rewrite_class('ObjectComplementOf'(C), Return) :-
	!,
	rewrite_class(C, NewC),
	rewrite_class_complement(NewC, Return).

rewrite_class('ObjectSomeValuesFrom'(R, 'ObjectOneOf'([Name])), 'ObjectSomeValuesFrom'(RR, 'ObjectOneOf'([Name]))) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectSomeValuesFrom'(R, C), 'ObjectSomeValuesFrom'(RR, NewC)) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).

rewrite_class(
	'ObjectAllValuesFrom'(R, 'ObjectOneOf'([Name])),
	'ObjectAllValuesFrom'(RR, 'ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectOneOf'([Name])]))
	) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectAllValuesFrom'(R, 'ObjectComplementOf'(C)), RewrittenClass) :-
	!,
	rewrite_class('ObjectComplementOf'('ObjectSomeValuesFrom'(R, C)), RewrittenClass).


% BUG: we should check whether the NewC ends up being a negation,
% then we could remove it.
rewrite_class('ObjectAllValuesFrom'(R, C), 'ObjectAllValuesFrom'(RR, NewC)) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).


rewrite_class('ObjectHasValue'(R, I), 'ObjectSomeValuesFrom'(RR, 'ObjectOneOf'([I]))) :-
	!,
	rewrite_property(R, RR).


% BUG: this is maybe confusing because it deletes what the user said about R and C.
rewrite_class('ObjectMinCardinality'(0, _R), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.
rewrite_class('ObjectMinCardinality'(0, _R, _C), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.


rewrite_class('ObjectMaxCardinality'(0, R), 'ObjectComplementOf'('ObjectSomeValuesFrom'(RR, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))))) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectMaxCardinality'(0, R, C), 'ObjectComplementOf'('ObjectSomeValuesFrom'(RR, NewC))) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).

rewrite_class('ObjectExactCardinality'(0, R), 'ObjectComplementOf'('ObjectSomeValuesFrom'(RR, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))))) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectExactCardinality'(0, R, C), 'ObjectComplementOf'('ObjectSomeValuesFrom'(RR, NewC))) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).

rewrite_class('ObjectHasSelf'(R), 'ObjectHasSelf'(R3)) :-
	!,
	rewrite_property(R, R1),
	(
		R1 = 'ObjectInverseOf'(R2)
	->
		R3 = R2
	;
		R3 = R1
	).

rewrite_class('ObjectMinCardinality'(N, R), 'ObjectMinCardinality'(N, RR, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')))) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectMinCardinality'(N, R, C), 'ObjectMinCardinality'(N, RR, NewC)) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).

rewrite_class('ObjectMaxCardinality'(N, R), 'ObjectMaxCardinality'(N, RR, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')))) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectMaxCardinality'(N, R, C), 'ObjectMaxCardinality'(N, RR, NewC)) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).

rewrite_class('ObjectExactCardinality'(N, R), 'ObjectExactCardinality'(N, RR, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')))) :-
	!,
	rewrite_property(R, RR).

rewrite_class('ObjectExactCardinality'(N, R, C), 'ObjectExactCardinality'(N, RR, NewC)) :-
	!,
	rewrite_property(R, RR),
	rewrite_class('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), C]), NewC).

% Note: OWL 2 spec allows only 1..*
rewrite_class('ObjectOneOf'([]), 'ObjectComplementOf'('Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')))) :- !.

rewrite_class('ObjectOneOf'([I]), 'ObjectOneOf'([I])) :- !.

rewrite_class('ObjectOneOf'(IndividualList), RewrittenClass) :-
	!,
	findall('ObjectOneOf'([Individual]), member(Individual, IndividualList), ClassList),
	rewrite_class('ObjectUnionOf'(ClassList), RewrittenClass).

% Note: OWL 2 spec allows only 2..*
rewrite_class('ObjectIntersectionOf'([]), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.

% Note: OWL 2 spec allows only 2..*
rewrite_class('ObjectIntersectionOf'([C]), NewC) :-
	!,
	rewrite_class(C, NewC).

% During rewriting we should treat the intersection as a list of
% 0 or more elements. Each element in this list should be rewritten
% and the resulting elements should be reordered.
% BUG: In the end we should pack the list into a binary representation, e.g.
% and(x, and(y, and(z))) to be compatible with the current OWLACE->OWL DCG.
% (Or maybe we should change the DCG to support lists...)

rewrite_class('ObjectIntersectionOf'(Classes), Return) :-
	!,
	% BUG: Do we need the first flattening?
	make_flat('ObjectIntersectionOf'(_), Classes, FlatClasses),
	rewrite_classlist(FlatClasses, RewrittenClasses),
	% BUG: new flatting in the right position
	make_flat('ObjectIntersectionOf'(_), RewrittenClasses, FlatRewrittenClasses),
	order_classes(FlatRewrittenClasses, OrderedClasses),
	prune_intersection('ObjectIntersectionOf'(OrderedClasses), FinalClass),
	(
		FinalClass = 'ObjectIntersectionOf'(FinalClassList)
	->
		check_classlist(FinalClassList),
		intersection_binary(FinalClass, Return)
	;
		Return = FinalClass
	).


% Note: OWL 2 spec allows only 2..*
rewrite_class('ObjectUnionOf'([]), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.

% Note: OWL 2 spec allows only 2..*
rewrite_class('ObjectUnionOf'([C]), NewC) :-
	!,
	rewrite_class(C, NewC).

rewrite_class('ObjectUnionOf'(Classes), Return) :-
	!,
	make_flat('ObjectUnionOf'(_), Classes, FlatClasses),
	rewrite_classlist(FlatClasses, RewrittenClasses),
	make_flat('ObjectUnionOf'(_), RewrittenClasses, FlatRewrittenClasses),
	order_classes(FlatRewrittenClasses, OrderedClasses),
	prune_union('ObjectUnionOf'(OrderedClasses), FinalClass),
	%format("union: ~w~n", [FinalClass]),
	(
		FinalClass = 'ObjectUnionOf'(FinalClassList)
	->
		check_classlist(FinalClassList),
		union_binary(FinalClass, ReturnTmp),
		Return = 'ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), ReturnTmp])
	;
		Return = FinalClass
	).

rewrite_class('DataHasValue'(DataProperty, '^^'(DataValue, DataType)), 'DataHasValue'(DataProperty, '^^'(DataValue, CastDataType))) :-
	!,
	datatype_castdatatype(DataType, CastDataType).

rewrite_class('Class'(C), 'Class'(C)).


%% rewrite_class_complement(+Class1:term, -Class2:term) is det.
%
% @param Class1 is a class expression embedded into ComplementOf
% @param Class2 is a rewritten class expression 
%
rewrite_class_complement('ObjectComplementOf'(Class), Class) :- !.

rewrite_class_complement('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectComplementOf'(Class)]), Class) :- !.

/* BUG: We should also handle ObjectAllValuesFrom wrapped in ObjectIntersectionOf. :*/

rewrite_class_complement('ObjectAllValuesFrom'(R, Class), RewrittenClass) :-
	!,
	rewrite_class('ObjectSomeValuesFrom'(R, 'ObjectComplementOf'(Class)), RewrittenClass).

rewrite_class_complement('ObjectMinCardinality'(N, R, C), NewC) :-
	!,
	NewN is N - 1,
	rewrite_class('ObjectMaxCardinality'(NewN, R, C), NewC).

rewrite_class_complement('ObjectMaxCardinality'(N, R, C), RewrittenClass) :-
	!,
	NewN is N + 1,
	rewrite_class('ObjectMinCardinality'(NewN, R, C), RewrittenClass).

% BUG: just testing
% maybe we could run always some sort of simplify/2 to remove owl:Thing etc.
rewrite_class_complement('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectUnionOf'(UnionClassList)]), RewrittenClass) :-
	!,
	findall('ObjectComplementOf'(UnionClass), member(UnionClass, UnionClassList), ClassList),
	rewrite_class('ObjectIntersectionOf'(ClassList), RewrittenClass).

rewrite_class_complement('ObjectUnionOf'(UnionClassList), RewrittenClass) :-
	!,
	findall('ObjectComplementOf'(UnionClass), member(UnionClass, UnionClassList), ClassList),
	rewrite_class('ObjectIntersectionOf'(ClassList), RewrittenClass).

rewrite_class_complement(Class, 'ObjectComplementOf'(Class)).


%% prune_intersection
%
% Note that we insert ':'('http://www.w3.org/2002/07/owl#', 'Thing') as the first element in case the
% ClassList starts with a complex class. This is to make the verbalization
% more direct.
%

% Note: OWL 2 spec allows only 2..*
prune_intersection('ObjectIntersectionOf'([C]), C) :- !.

prune_intersection('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'ObjectOneOf'([A])]), 'ObjectOneOf'([A])) :- !.

prune_intersection('ObjectIntersectionOf'([ComplexClass | Rest]), 'ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), ComplexClass | Rest])) :-
	ComplexClass \= 'Class'(_),
	!.

% Note: OWL 2 spec allows only 2..*
% BUG: if Rest = [] then a syntactically wrong class expression is generated
prune_intersection('ObjectIntersectionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'Class'(NamedClass) | Rest]), Pruned) :-
	!,
	prune_intersection('ObjectIntersectionOf'(['Class'(NamedClass) | Rest]), Pruned).

prune_intersection(And, And).


%% prune_union
%
% This is to make the verbalization more direct.
%

% Note: OWL 2 spec allows only 2..*
prune_union('ObjectUnionOf'([]), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.

% Note: OWL 2 spec allows only 2..*
prune_union('ObjectUnionOf'([C]), C) :- !.

% Note: OWL 2 spec allows only 2..*
prune_union('ObjectUnionOf'(['Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')) | _]), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.

prune_union(ObjectUnionOf, ObjectUnionOf).



%% intersection_binary(+ObjectIntersectionOf:term, -BinaryObjectIntersectionOf:term) is det.
%% intersection_binary(-ObjectIntersectionOf:term, +BinaryObjectIntersectionOf:term) is nondet.
%
% Makes a binary intersection. E.g.
%
% and([a, b, c, d]) -> and([a, and([b, and([c, d])))
%

% Note: OWL 2 spec allows only 2..*
intersection_binary('ObjectIntersectionOf'([]), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))).

% Note: OWL 2 spec allows only 2..*
intersection_binary('ObjectIntersectionOf'([Class]), Class).
intersection_binary('ObjectIntersectionOf'([C1, C2 | ClassList]), 'ObjectIntersectionOf'([C1, IntersectionBinary])) :-
	intersection_binary('ObjectIntersectionOf'([C2 | ClassList]), IntersectionBinary).


%% union_binary(+ObjectUnionOf:term, -BinaryObjectUnionOf:term) is det.
%% union_binary(-ObjectUnionOf:term, +BinaryObjectUnionOf:term) is nondet.
%
% Makes a binary union. E.g.
%
% or([a, b, c, d]) -> or([a, or([b, or([c, d])))

% Note: OWL 2 spec allows only 2..*
union_binary('ObjectUnionOf'([]), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))).

% Note: OWL 2 spec allows only 2..*
union_binary('ObjectUnionOf'([Class]), Class).

union_binary('ObjectUnionOf'([C1, C2 | ClassList]), 'ObjectUnionOf'([C1, UnionBinary])) :-
	union_binary('ObjectUnionOf'([C2 | ClassList]), UnionBinary).


%% make_flat(+CoordinationTemplate:term, +ClassList:list, -FlatClassList:list) is det.
%
% E.g. an intersection might contain other intersections. We will flat
% such intersection structures.
%
%==
% before: and(a, b, and(c, d), e, and(f, and(g, h)))
% after:  and(a, b, c, d, e, f, g, h)
%==
%
% @param CoordinationTemplate is either ObjectIntersectionOf(_) or ObjectUnionOf(_)
% @param ClassList is a list of class expressions 
% @param FlatClassList is a list of class expressions with embedded coordination removed
%
make_flat(_, [], []).

make_flat(CoordinationTemplate, [CoordinationTemplate | ClassList2], FlatClassList) :-
	arg(1, CoordinationTemplate, ClassList1),
	!,
	make_flat(CoordinationTemplate, ClassList1, FlatClassList1),
	make_flat(CoordinationTemplate, ClassList2, FlatClassList2),
	append(FlatClassList1, FlatClassList2, FlatClassList).

make_flat(CoordinationTemplate, [Class | ClassList], [Class | FlatClassList]) :-
	make_flat(CoordinationTemplate, ClassList, FlatClassList).



%% order_classes(+Classes:list, -OrderedClasses:list) is det.
%
% @param Classes is a list of OWL classes
% @param OrderedClasses is a list of OWL classes after being ordered
%
order_classes(Classes, OrderedClasses) :-
	predsort(natural_order, Classes, OrderedClasses).


%% natural_order(-Delta:atom, +E1:term, +E2:term) is det.
%
% A custom comparison of two terms, to be applied to sorting of the Classes to
% get a more readable order.
% Note that =|predsort/3|= which calls =|natural_order/3|= will remove the
% dublicates. But this is OK, since we only apply it to =|ObjectIntersectionOf|= and
% =|ObjectUnionOf|= for which it can be considered a form of semantics preserving
% rewriting, i.e.
%
% * X and X = X
% * X or X = X
%
% Other laws:
%
% * Thing and X = X
% * Thing or X = Thing

% owl:Thing is equal to itself but otherwise smaller than everything
natural_order(=, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.
natural_order(<, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing')), _) :- !.
natural_order(>, _, 'Class'(':'('http://www.w3.org/2002/07/owl#', 'Thing'))) :- !.

% BUG: discuss the arity-1 classes:
% UnionOf, IntersectionOf, ComplementOf, HasSelf, OneOf
%
% The rough ordering should be:
%
% OneOf < HasSelf < ComplementOf < UnionOf/IntersectionOf
%
% Union/Intersection should probably come last, even after
% property restrictions.
%
% Note, since ObjectOneOf and ObjectIntersectionOf (ObjectUnionOf)
% have always the same arity (i.e. 1) they are compared by functor-name
% and OneOf loses to IntersectionOf. This is not good, that's why we SHOULD
% override their order here.

natural_order(<, 'ObjectOneOf'(_), 'ObjectComplementOf'(_)) :- !.
natural_order(<, 'ObjectOneOf'(_), 'ObjectHasSelf'(_)) :- !.
natural_order(<, 'ObjectOneOf'(_), 'ObjectIntersectionOf'(_)) :- !.
natural_order(<, 'ObjectOneOf'(_), 'ObjectUnionOf'(_)) :- !.

natural_order(<, 'ObjectHasSelf'(_), 'ObjectComplementOf'(_)) :- !.
natural_order(>, 'ObjectComplementOf'(_), 'ObjectOneOf'(_)) :- !.
natural_order(>, 'ObjectComplementOf'(_), 'ObjectHasSelf'(_)) :- !.
natural_order(>, 'ObjectHasSelf'(_), 'ObjectOneOf'(_)) :- !.
natural_order(>, 'ObjectIntersectionOf'(_), 'ObjectOneOf'(_)) :- !.
natural_order(>, 'ObjectUnionOf'(_), 'ObjectOneOf'(_)) :- !.

% All atomic terms (i.e. atoms, strings, integers, and floating-point numbers) are equal.
/*
% BUG: We cannot define it like that because predsort removes all duplicates, i.e.
% this definition would remove all but one named class.
% BUG: This is unfortunate because the original order of named classes
% would be changed. E.g. "Every man is woman that is a gene." would be
% verbalized as "Every man is a gene that is a woman." because `gene'
% beats `woman' in the ordering.
natural_order(=, Term1, Term2) :-
	atomic(Term1),
	atomic(Term2),
	!.
*/

% We override the way compound terms are compared.
% We first compare the embedded classes.
% If those are equal then we fall back to the standard comparison.
% If those are not equal then we return their Delta.
natural_order(FinalDelta, Class1, Class2) :-
	embedded_class(Class1, EClass1),
	embedded_class(Class2, EClass2),
	!,
	natural_order(Delta, EClass1, EClass2),
	(Delta = '=' -> compare(FinalDelta, Class1, Class2) ; FinalDelta = Delta).


% 'ObjectComplementOf'(C) is equal to itself but otherwise larger than everything
% BUG: test without this redefinition
%
% This definition must not be here, consider e.g.:
% ssyn(sube(p, and([some(some(p)), not(p)])), Ax1)
% which is verbalized incorrectly.
% ssyn(sube(p, and([some(some(p)), not(p)])), Ax1), rewrite_subclassof(Ax1, Ax2), owlace_dcg:from_owl_to_ace(Ax2, Ace), write(Ace), nl.
%natural_order(=, 'ObjectComplementOf'(C), 'ObjectComplementOf'(C)) :- !.
%natural_order(<, _, 'ObjectComplementOf'(_)) :- !.
%natural_order(>, 'ObjectComplementOf'(_), _) :- !.

natural_order(Delta, E1, E2) :-
	compare(Delta, E1, E2).


%% embedded_class(+Class:term, -EmbeddedClass:term) is det.
%
% Extracts the embedded class expression (or a list of
% expressions) from certain class expressions,
% i.e. ObjectSomeValuesFrom.
%
embedded_class('ObjectIntersectionOf'(ClassList), ClassList).
embedded_class('ObjectUnionOf'(ClassList), ClassList).
embedded_class('ObjectComplementOf'(Class), Class).
embedded_class('ObjectSomeValuesFrom'(_, Class), Class).
embedded_class('ObjectAllValuesFrom'(_, Class), Class).
embedded_class('ObjectMinCardinality'(_, _, Class), Class).
embedded_class('ObjectMaxCardinality'(_, _, Class), Class).
embedded_class('ObjectExactCardinality'(_, _, Class), Class).


%% check_classlist(+ClassList:list) is det.
%
% A class list is complicated iff
% it contains at least 2 classes that are complicated.
%
% This is the simplest complicated formula that I can think of:
%
%==
% sube(p, and([some(some(p)), some(some(g))]))
%==
%
% Note that this axiom is 27 tokens long (in OWL functional syntax).
% So it is pretty hard to exhaustively search for such axioms.
%
% Test:
%
%==
% ssyn(sube(p, and([some(some(p)), some(some(g))])), Ax1), rewrite_subclassof(Ax1, Ax2).
%==
%
% Unfortunately, XOR is also complicated:
%
%==
% sube(p, and([or([a, b]), not(and([c, d]))]))
%==
%
% as is this one:
%
%==
% sube(p, and([or([a, b]), not(some(x, y))]))
%==
%
check_classlist(ClassList) :-
	classlist_complicationdegree(ClassList, Length),
	Length =< 1,
	!.


%% classlist_complicationdegree(+ClassList:list, -Length:integer) is det.
%
%
classlist_complicationdegree(ClassList, Length) :-
	findall(Class, (member(Class, ClassList), is_complicated(Class)), ComplicatedClassList),
	length(ComplicatedClassList, Length).


%% is_complicated(+Class) is det.
%
% Decides if a given class is structurally too complicated for the verbalization.
% Basically, complicated classes are intersections and unions, and everything
% else that has a complicated class as its argument.
%
% @param Class is a class expression
%
is_complicated('ObjectIntersectionOf'(_)).

is_complicated('ObjectUnionOf'(_)).

is_complicated('ObjectSomeValuesFrom'(_, 'ObjectIntersectionOf'(_))).

is_complicated('ObjectAllValuesFrom'(_, 'ObjectIntersectionOf'(_))).

is_complicated('ObjectMinCardinality'(_, _, 'ObjectIntersectionOf'(_))).

is_complicated('ObjectMaxCardinality'(_, _, 'ObjectIntersectionOf'(_))).

is_complicated('ObjectExactCardinality'(_, _, 'ObjectIntersectionOf'(_))).

is_complicated('ObjectComplementOf'(Class)) :-
	is_complicated(Class).


%% datatype_castdatatype(+DataType:atom, -CastDataType:atom) is det.
%
% Some type conversion.
%
% @bug Incomplete, see: http://www.w3.org/TR/xmlschema-2/
%
datatype_castdatatype('http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#string').

datatype_castdatatype('http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#integer').
datatype_castdatatype('http://www.w3.org/2001/XMLSchema#int', 'http://www.w3.org/2001/XMLSchema#integer').
datatype_castdatatype('http://www.w3.org/2001/XMLSchema#long', 'http://www.w3.org/2001/XMLSchema#integer').
datatype_castdatatype('http://www.w3.org/2001/XMLSchema#nonNegativeInteger', 'http://www.w3.org/2001/XMLSchema#integer').

datatype_castdatatype('http://www.w3.org/2001/XMLSchema#double', 'http://www.w3.org/2001/XMLSchema#double').
datatype_castdatatype('http://www.w3.org/2001/XMLSchema#float', 'http://www.w3.org/2001/XMLSchema#double').


%% rewrite_property(+Property:term, -RewrittenProperty:term) is det.
%
% Rewrites property expressions to remove extra inverse properties.
%
% @param Property is an OWL property expression in the form i(i(i(...i(PropertyName)...)))
% @param RewrittenProperty is an OWL property expression where the number of "inverse levels" is max 1
%
rewrite_property('ObjectInverseOf'('ObjectInverseOf'(Property)), RewrittenProperty) :-
	!,
	rewrite_property(Property, RewrittenProperty).

rewrite_property('ObjectInverseOf'(Property), 'ObjectInverseOf'(Property)) :-
	!.

rewrite_property(Property, Property).
