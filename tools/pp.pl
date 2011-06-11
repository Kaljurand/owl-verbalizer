%% pp
%
% Pritty printer. Generates all sentences allowed in the language.
% There is a restriction on the sentence length to get a finite output.
%
% Usage on the Unix commandline (where 'swipl' is a SWI-Prolog executable):
%
%==
% echo "[owlace_dcg]. owlace_dcg:pp." | swipl
%==

pp :-
	forall(
		( init_sentence(ACE), owl_ace(OWL, ACE) ),
		( concat_atom(ACE, ' ', ACEAtom), format('~w :: ~w~n', [ACEAtom, OWL]) )
	).

init_sentence(ACE) :-
	between(4, 12, Int),
	length(ACE, Int).
