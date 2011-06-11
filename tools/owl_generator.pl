:- module(owl_generator, [
	]).


/** <module> OWL Generator

@author Kaarel Kaljurand
@version 2007-06-18


Roundtripping must be tested in 2 ways:
ACE->OWL->ACE and OWL->ACE->OWL. In the first case
we generate all possible ACE sentences with the DCG (because the DCG
is in essence the formal specification of OWL ACE) in the second
case we generate all possible OWL axioms based on the formal specification
of OWL).

As a result of this work, we should be able to generate
a large file which for every OWL axiom (up to certain length) contains:

1. Its rewritten form
2. Its verbalization OR "fail" (in case the axiom was too complex)

Proofs:

1. Show that 'rewrite' applied to rewritten formulas is an identity.

2. Show that 'rewrite' applied to DCG-generated formulas is an identity.
(No, this won't work, e.g. the DCG generates negations next to numbers,
the rewriting removes them.)

Discuss the equivalence of and and comma-and. And the
effect on bidirectionality.


Priorities:

1. DONE: simple_axiom/1.
2. DONE: Generation of all classes.
3. DONE???: Rewriting of all classes.
4. DONE???: Testing the DCG on the rewritten SubClassOf axioms.
5. DONE???: Rewriting with complexity check.
6. Table 1.
7. Generation of all axioms, incl data axioms.
8. Support for property axioms in the DCG.
9. Support for data-valued properties (at least partially).
10. Parsing the complete ontology in functional syntax (without annotations)
11. Tokenization of the whole ontology
12. Support for annotations at the tokenization level (i.e. tokenization would remove them)
13. Support for annotations at the parsing level (i.e. we could return the annotation
in case the class was too complex)

TEST: (that's ok) ssyn(sube(p, and([some(k, p), some(b, g)])), Ax1), rewrite_subclassof(Ax1, Ax2).
TEST: ssyn(sube(p, and([some(a, min(p)), or([x,y]), some(a, some(p))])), Ax1), rewrite_subclassof(Ax1, Ax2), write(Ax2), nl.

TEST: This should be rewritten to be simpler:

'SubClassOf'('owl:Thing', 'ObjectSomeValuesFrom'(modify, 'ObjectIntersectionOf'(['owl:Thing', 'ObjectIntersectionOf'(['ObjectOneOf'(['Met']), protein])])))

'SubClassOf'('owl:Thing', 'ObjectSomeValuesFrom'(modify, 'ObjectIntersectionOf'([protein, 'ObjectOneOf'(['Met'])])))
*/

:- use_module(rewrite_subclassof).
:- use_module(owlace_dcg).


%% axiom_prolog_lisp(-Prolog:term, +Lisp:list, []) is det.
%% axiom_prolog_lisp(+Prolog:term, -Lisp:list, []) is det.
%% axiom_prolog_lisp(-Prolog:term, -Lisp:list, []) is nondet.
%
% BUG: maybe change '-' to '?'
%
% The following code can either:
%
% 1. Convert the OWL functional-style syntax into Prolog notation,
% given that there is a tokenization step before which normalizes spaces,
% converts character sequences into single tokens, etc.
%
% 2. Generate all possible OWL axioms up to certain length (in tokens) in both
% the Lisp and the Prolog notation. The length of Lisp can be set by =|make_list/3|=.
%
% Using this code, we can show that:
%
% 1. certain OWL axioms cannot be verbalized.
%
% 2. certain OWL axioms that are the result of the OWLED paper's table 1 cannot be verbalized.
% (E.g. we need a named class as the first conjunct in the left side of SubClassOf)
%
% 3. certain OWL axioms that are the result of table 1 and that have undergone further
% semantics-preserving changes (let's call it table 1.5 which is unfortunately not in the OWLED paper)
% cannot be verbalized. (Those changes add e.g. owl:Thing as the first conjunct, etc).
% The remaining axioms must be detected and reported to the user.

% BUG: currently we generate only a few essential axioms, but in the end this code will
% cover all of OWL 2.

axiom_prolog_lisp('SubClassOf'(C1, C2)) -->
	['SubClassOf', '('], class_prolog_lisp(C1), class_prolog_lisp(C2), [')'].


%% class_prolog_lisp(-Prolog:term, -Lisp:list, -RestLisp:list) is nondet.
%
% @param Prolog OWL class description in Prolog notation
% @param Lisp OWL class description in Lisp notation (as a list of tokens)
% @param RestLisp OWL class description in Lisp notation (usually shoould be empty)
%
% Converts a list of tokens into an OWL class description in Prolog notation.
% The list of tokens corresponds to the functional-style (Lisp-like) syntax of OWL.
% This predicate can also be used to generate all OWL class descriptions (up
% to a certain length).

class_prolog_lisp('owl:Thing') --> ['owl:Thing'].

class_prolog_lisp('owl:Nothing') --> ['owl:Nothing'].

class_prolog_lisp('ObjectComplementOf'(C)) -->
	['ObjectComplementOf', '('], class_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectIntersectionOf'(C)) -->
	['ObjectIntersectionOf', '('], classes_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectUnionOf'(C)) -->
	['ObjectUnionOf', '('], classes_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectOneOf'(IndividualList)) -->
	['ObjectOneOf', '('], individuallist_prolog_lisp(IndividualList), [')'].

class_prolog_lisp('ObjectSomeValuesFrom'(R, C)) -->
	['ObjectSomeValuesFrom', '('], property_prolog_lisp(R), class_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectAllValuesFrom'(R, C)) -->
	['ObjectAllValuesFrom', '('], property_prolog_lisp(R), class_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectHasValue'(R, I)) -->
	['ObjectHasValue', '('], property_prolog_lisp(R), individual_prolog_lisp(I), [')'].

class_prolog_lisp('ObjectExistsSelf'(R)) -->
	['ObjectExistsSelf', '('], property_prolog_lisp(R), [')'].

class_prolog_lisp('ObjectMinCardinality'(N, R, C)) -->
	['ObjectMinCardinality', '('], number_prolog_lisp(N), property_prolog_lisp(R), class_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectMaxCardinality'(N, R, C)) -->
	['ObjectMaxCardinality', '('], number_prolog_lisp(N), property_prolog_lisp(R), class_prolog_lisp(C), [')'].

class_prolog_lisp('ObjectExactCardinality'(N, R, C)) -->
	['ObjectExactCardinality', '('], number_prolog_lisp(N), property_prolog_lisp(R), class_prolog_lisp(C), [')'].


class_prolog_lisp(protein) --> [protein].

% BUG: temporary, to generate rewrite2-n.txt (2 means that there are 2 named classes)
% class_prolog_lisp(gene) --> [gene].


classes_prolog_lisp([]) --> [].

classes_prolog_lisp([Class | Classes]) --> class_prolog_lisp(Class), classes_prolog_lisp(Classes).


property_prolog_lisp(modify) --> [modify].
property_prolog_lisp('InverseObjectProperty'(modify)) -->
	['InverseObjectProperty', '('], [modify], [')'].


individual_prolog_lisp('Met') --> ['Met'].

individuallist_prolog_lisp([]) --> [].
individuallist_prolog_lisp([Individual | IndividualList]) -->
	individual_prolog_lisp(Individual),
	individuallist_prolog_lisp(IndividualList).


number_prolog_lisp(0) --> [0].
number_prolog_lisp(1) --> [1].

%
% Some helpful utilities
%

%% ssyn
%
% Simple syntax helps to input some OWL axioms.

ssyn(sube(SC1, SC2), 'SubClassOf'(C1, C2)) :-
	ssyn(SC1, C1),
	ssyn(SC2, C2).

ssyn(and(SClassList), 'ObjectIntersectionOf'(ClassList)) :-
	ssyn_list(SClassList, ClassList).

ssyn(or(SClassList), 'ObjectUnionOf'(ClassList)) :-
	ssyn_list(SClassList, ClassList).

ssyn(some(SC), 'ObjectSomeValuesFrom'(modify, C)) :-
	ssyn(SC, C).

ssyn(some(i(Property), SC), 'ObjectSomeValuesFrom'('InverseObjectProperty'(Property), C)) :-
	atomic(Property),
	ssyn(SC, C).

ssyn(some(Property, SC), 'ObjectSomeValuesFrom'(Property, C)) :-
	atomic(Property),
	ssyn(SC, C).

ssyn(min(SC), 'ObjectMinCardinality'(2, modify, C)) :-
	ssyn(SC, C).

ssyn(not(SC), 'ObjectComplementOf'(C)) :-
	ssyn(SC, C).

ssyn(one(I), 'ObjectOneOf'([I])).

ssyn(p, protein).
ssyn(g, gene).


ssyn_list([], []).
ssyn_list([SC | SClassList], [C | ClassList]) :-
	ssyn(SC, C),
	ssyn_list(SClassList, ClassList).

ssyn_ace(SSyn) :-
	ssyn(SSyn, Owl),
	rewrite_subclassof:rewrite_subclassof(Owl, RewrittenOwl),
	owlace_dcg:from_owl_to_ace(RewrittenOwl, Ace),
	format("~w~n~n", [Ace]),
	fail.




% Convert OWL in Lisp notation into Prolog notation.
o2o(AxiomLisp, AxiomProlog) :-
	atom_chars(AxiomLisp, AxiomLispChars),
	% BUG: here we need tokenization
	axiom_prolog_lisp(AxiomProlog, AxiomLispChars, []).

% Generate all possible OWL axioms
make_axiom(Axiom, High) :-
	make_axiom(Axiom, 1, High).

make_axiom(Axiom, Low, High) :-
	make_list(Low, High, List),
	axiom_prolog_lisp(Axiom, List, []).

make_list(High, Low, List) :-
	between(High, Low, Int),
	length(List, Int).

%
% All kinds of tests.
%

% echo "[owl_generator]. test_rewrite." | swipl > jama
test_rewrite :-
	forall(
		(make_axiom(Ax, 14, 16), format("~nAx1: ~w~n", [Ax]) ),
		test_rewrite_subclassof(Ax)
	).

test_rewrite_subclassof(Ax) :-
	rewrite_subclassof:rewrite_subclassof(Ax, NewAx),
	!,
	format("Ax2: ~w~n", [NewAx]).

test_rewrite_subclassof(_Ax) :-
	format("--- FAIL ---~n", []).

% echo "[owl_generator]. test_and, fail." | swipl
%Ax1 = 'SubClassOf'('ObjectIntersectionOf'(_), protein),
test_and :-
	make_axiom(Ax1, 8),
	rewrite_subclassof:rewrite_subclassof(Ax1, Ax2),
	format("Ax1 = ~w~nAx2 = ~w~n~n", [Ax1, Ax2]).


%% main_test is det.
%
% echo "[owl_generator]. main_test." | swipl
%
% Status: all possible axioms up to length 15 are rewritten
% and verbalized without a failure. The output in this case
% is 138MB.
% The correctness has to be still proven, e.g. by testing roundtripping.
%
% BUG: rewrite in such a way that it will always succeed and print
% out FAIL is there was a failure.

main_test :-
	forall(
		(make_axiom(Ax1, 15), format("Ax1: ~w~n", [Ax1])),
		main_test_x(Ax1)
	).

main_test_x(Ax1) :-
	rewrite_subclassof:rewrite_subclassof(Ax1, Ax2),
	format("Ax2: ~W~n", [Ax2, [quoted(true)]]),
	translate_to_ace(Ax2, Ace),
	!,
	format("ACE: ~w~n~n", [Ace]).

translate_to_ace(Owl, Ace) :-
	catch(
		call_with_time_limit(1, run_owl_ace(Owl, Ace)),
		CatchType,
		Ace = CatchType
	).


run_owl_ace(Axiom, Ace) :-
	%owlace_dcg:owl_ace(Axiom, Ace),
	owlace_dcg:from_owl_to_ace(Axiom, Ace),
	!.

run_owl_ace(_, '--- FAIL ---').


%
% First roundtrip test.
%
% echo "[owl_generator]. roundtrip." | swipl
%

roundtrip :-
	forall(
		make_axiom(Ax, 10),
		(roundtrip(Ax, Result), format("~w: ~w~n", [Result, Ax]))
	).


roundtrip(Ax, Result) :-
	catch(
		call_with_time_limit(0.1, roundtrip_x(Ax, Result)),
		CatchType,
		Result = CatchType
	).


% We must accept only the first solution.
roundtrip_x(Ax1, 'ok') :-
	rewrite_subclassof:rewrite_subclassof(Ax1, Ax2),
	owlace_dcg:owl_ace(Ax2, Ace),
	owlace_dcg:owl_ace(Ax3, Ace),
	Ax2 = Ax3,
	!.

roundtrip_x(_, 'FAIL').

%% roundtrip_from_ace is det.
%
% Roundtrip test: ACE -> OWL -> ACE
%
% echo "[owl_generator]. roundtrip_from_ace." | swipl
%
roundtrip_from_ace :-
	forall(
		(
			make_ace(Ace),
			owlace_dcg:owl_ace(Owl, Ace)
		),
		(
			roundtrip_from_ace(Owl, Ace2),
			!,
			(Ace = Ace2 -> Result = ok; Result = 'FAIL'),
			format("~w: ~w~n~w: ~w~n~w: ~w~n~n", [Result, Ace, Result, Owl, Result, Ace2])
		)
	).

roundtrip_from_ace(Owl, Ace2) :-
	catch(
		call_with_time_limit(0.1, roundtrip_from_ace_x(Owl, Ace2)),
		CatchType,
		Ace2 = CatchType
	).

% Note: we get only the first solution.
roundtrip_from_ace_x(Owl, Ace2) :-
	owlace_dcg:owl_ace(Owl, Ace2),
	!.

roundtrip_from_ace_x(_, 'FAIL').


% BUG: we should test with sentences of length 50. Those
% can occur quite frequently.
make_ace(Ace) :-
	between(15, 17, Int),
	length(Ace, Int),
	owlace_dcg:owl_ace(_, Ace).
