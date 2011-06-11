
:- use_module(elplusplus_generator, [
	make_axiom/2
	]).

:- use_module(rewrite_subclassof, [
	rewrite_subclassof/2
	]).

:- use_module(owlace_dcg).


%%
%
% Generate all possible axioms and try to verbalize
% them. If verbalization succeeds then the verbalization
% is output, otherwise "FAILED" is output.
%
generate_axiom_and_verbalize(TokenLength) :-
	make_axiom(TokenLength, Owl),
	% BUG: here we could plug in rewriting to handle more axioms
	(
		owlace_dcg:owl_ace(Owl, Ace)
	->
		format("OWL: ~w~nACE: ~w~n~n", [Owl, Ace])
	;
		format("OWL: ~w~nFAILED~n~n", [Owl])
	),
	fail.


% roundtrip(+TokenLength) is det.
%
% roundtrip: OWL -> ACE -> OWL
%
% We test the roundtrip in the following way.
% For all possible OWL axioms up to a certain tokenlength, we:
% 1. Test if it can be verbalized, if not then we print a failure message
% 2. Test is the verbalization can be converted back to OWL, if not then we print a failure message
% (Note that the same predicate is used to do OWL->ACE and ACE->OWL.)
% 3. Test is the original and the new axioms are lexically equivalent,
% if not then we print a failure message.
% If yes then we print "ok".
%
% TODO:
%
% * What if there are many solutions?
% * What if the DCG loops? Add try-catch.
%
roundtrip(TokenLength) :-
	forall(
		(
		make_axiom(TokenLength, Axiom),
		rewrite_subclassof(Axiom, Axiom1)
		),
		(
			(
				owlace_dcg:owl_ace(Axiom1, Ace)
			->
				(
					owlace_dcg:owl_ace(Axiom2, Ace)
				->
					(
						Axiom1 = Axiom2
					->
						format("ok: ~w <-> ~w~n", [Axiom1, Ace])
					;
						format("NO: ~w -> ~w -> ~w~n", [Axiom1, Ace, Axiom2])
					)
				;
					format("FAILED: OWL->ACE->OWL: ~w -> ~w -> ???~n", [Axiom1, Ace])
				)

			;
				format("FAILED: OWL->ACE: ~w -> ???~n", [Axiom1])
			)
		)
	).
