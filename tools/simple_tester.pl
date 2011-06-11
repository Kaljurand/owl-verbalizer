
:- use_module(simple_axiom).
:- use_module(owlace_dcg).

generate_axiom_and_verbalize :-
	simple_axiom:simple_axiom(Owl),
	owlace_dcg:owl_ace(Owl, Ace),
	format("OWL: ~w~nACE: ~w~n~n", [Owl, Ace]),
	fail.

% roundtrip: OWL -> ACE -> OWL
roundtrip :-
	forall(
		simple_axiom:simple_axiom(Axiom1),
		(
			owlace_dcg:owl_ace(Axiom1, Ace),
			owlace_dcg:owl_ace(Axiom2, Ace),
			(
				Axiom1 = Axiom2
			->
				format("ok: ~w <-> ~w~n", [Axiom1, Ace])
			;
				format("NO: ~w -> ~w -> ~w~n", [Axiom1, Ace, Axiom2])
			)
		)
	).
