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

:- module(owlfss_acetext, [
		axiomlist_sentencelist/2,
		axiom_sentencelist/2
	]).


/** <module> OWL 2 verbalizer

@author Kaarel Kaljurand
@version 2011-06-13

*/

:- use_module(table_1, [
		table_1/2
	]).

:- use_module(rewrite_subclassof, [
		rewrite_subclassof/2
	]).

:- use_module(owlace_dcg, [
		owl_ace/2
	]).


%% axiomlist_sentencelist(+AxiomList:list, -AxiomSentenceList:list) is det.
%
% @param AxiomList is a list of OWL axioms
% @param AxiomSentenceList is a list of Axiom-SentenceList pairs
%
axiomlist_sentencelist([], []).

axiomlist_sentencelist([Axiom | AxiomList], [Axiom-SentenceList | AxiomSentenceList]) :-
	axiom_sentencelist(Axiom, SentenceList),
	axiomlist_sentencelist(AxiomList, AxiomSentenceList).


%% axiom_sentencelist(+Axiom:term, -SentenceList:list) is det.
%
% Maps the given axiom to its verbalization (list of ACE sentences).
% If the axiom is non-logical then returns 'ignored', if the verbalization
% fails then returns 'unsupported'.
%
% @param Axiom is an OWL axiom
% @param SentenceList is a list of ACE sentences
%
% @bug instead of returning atoms throw exceptions instead
%
axiom_sentencelist(Axiom, ignored) :-
	is_ignore(Axiom),
	!.

axiom_sentencelist(Axiom, SentenceList) :-
	table_1(Axiom, EquivalentAxioms),
	!,
	axiomlist_sentencelist_x(EquivalentAxioms, SentenceList).

axiom_sentencelist(_Axiom, unsupported).


%% axiomlist_sentencelist_x(+AxiomList:list, -SentenceList:list) is det.
%
% @param AxiomList is a list of OWL axioms
% @param SentenceList is a list of ACE sentences
%
axiomlist_sentencelist_x([], []).

axiomlist_sentencelist_x([Axiom | AxiomList], [Sentence | SentenceList]) :-
	rewrite_subclassof(Axiom, RewrittenAxiom),
	owl_ace(RewrittenAxiom, Sentence),
	!,
	axiomlist_sentencelist_x(AxiomList, SentenceList).

axiomlist_sentencelist_x([UnsupportedAxiom | AxiomList], [ErrorMessage | SentenceList]) :-
	with_output_to(atom(ErrorMessage), format("/* BUG: axiom too complex: ~w */", [UnsupportedAxiom])),
	axiomlist_sentencelist_x(AxiomList, SentenceList).


%% is_ignore(+Axiom:term) is det.
%
% Certain non-logical axioms are ignored.
%
% Note that we experimentally ignored the SubClassOf-axioms
% where the super class is owl:Thing as this is the case without saying.
% However, this does not seem to be a good idea in the context of synchronizing in ACE View.
%
% @param Axiom is an OWL axiom
%
is_ignore('Declaration'(_)).
is_ignore('Prefix'(_, _)).
is_ignore('AnnotationAssertion'(_, _, _)).
is_ignore('Import'(_)).
%is_ignore('SubClassOf'(_, 'Class'('owl:Thing'))).

% @deprecated
is_ignore('EntityAnnotation'(_, _)).
is_ignore('Annotation'(_, _)).
is_ignore('Imports'(_)).
