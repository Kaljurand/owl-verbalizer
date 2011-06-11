% PlUnit tests for OWL verbalizer.
%
% Load this file and run it by:
% :- run_tests.

:- begin_tests(owl_verbalizer).

:- use_module(ace_niceace).
:- use_module(owlace_dcg).

test(good_an_letters, [nondet]) :-
	ace_niceace:good_an_letters([a]).

test(good_an_letters, [fail]) :-
	ace_niceace:good_an_letters([b]).

test(n, [nondet]) :-
	owlace_dcg:n(num=pl, 'Class'('owl:Thing'), [things | C], C).

test(owl_ace) :-
	owlace_dcg:from_owl_to_ace(
		'SubClassOf'('Class'(protein), 'Class'(molecule)),
		['Every', protein, is, a, 'molecule.']
	).

test(owl_ace) :-
	from_owl_to_ace(
		'SubClassOf'(
			'Class'(protein),
			'ObjectIntersectionOf'([
				'Class'('owl:Thing'),
				'ObjectSomeValuesFrom'(
					'ObjectProperty'(modify),
					'ObjectIntersectionOf'([
						'Class'('owl:Thing'),
						'ObjectUnionOf'([
							'ObjectOneOf'(['Individual'('Met')]),
							'ObjectOneOf'(['Individual'('Met')])
						])
					])
				)
			])
		),
		['Every', protein, is, something, that, modifies, something, that, is, 'Met', or, that, is, 'Met.']).

:- end_tests(owl_verbalizer).
