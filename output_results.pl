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

:- module(output_results, [
		output_results/2
	]).


/** <module> Output results

This module defines various ways of presenting the results.
Currently defined output formats are:

* output_sentencelist/1: list of ACE sentences, each on a separate line
* output_mapping/1: HTML-table that maps an OWL axiom to a list of ACE sentences

@author Kaarel Kaljurand
@version 2011-06-09

*/

:- use_module(ace_niceace, [
		ace_niceace/2
	]).


%% output_results(+Format:atom, +Results:list)
%
% Outputs the given ACE sentences in the given format.
% that is given as input is a list of Axiom-Sentence pairs.
%
% @param Format One of {ace, csv, html}
% @param Results List of pairs (OWL axiom, List of ACE sentences)
%
output_results(Format, Results) :-
	output_header(Format, Results),
	output_results_loop(Format, Results),
	output_footer(Format, Results).


output_results_loop(_Format, []).

output_results_loop(Format, [Axiom-Message | Results]) :-
	atom(Message),
	!,
	format_message(Format, Message, Axiom),
	output_results_loop(Format, Results).

output_results_loop(Format, [Axiom-SentenceList | Results]) :-
	format_sentences(Format, Axiom, SentenceList),
	format_axiom_separator(Format),
	output_results_loop(Format, Results).


%% format_axiom_separator(+Format:atom)
%
format_axiom_separator(ace) :- nl.
format_axiom_separator(csv) :- nl.
format_axiom_separator(html).


%% format_message(+Format:atom, +Message:term, +Axiom:term)
%
% Outputs the message (e.g. error, warning, comment).
% If some output is generated then we also output the
% axiom separator symbol (newline).
%
format_message(ace, ignored, _Axiom) :- !.
format_message(html, ignored, _Axiom) :- !.

format_message(ace, Message, Axiom) :-
	format("/* ~w: ~w */~n", [Message, Axiom]),
	format_axiom_separator(ace).

format_message(csv, Message, Axiom) :-
	format("~w\t~w~n", [Message, Axiom]),
	format_axiom_separator(csv).

format_message(html, Message, Axiom) :-
	format("<tr style='color: red'><td>~w</td><td>~w</td></tr>~n", [Axiom, Message]).


%% format_sentences(+Format:atom, +Axiom:term, +Sentences:list)
%
format_sentences(Format, Axiom, Sentences) :-
	with_output_to(atom(AceText), format_sentences(Format, Sentences)),
	print_axiom_sentences(Format, Axiom, AceText).


%% format_sentences(+Format:atom, +Sentences:list)
%
format_sentences(_, []).

format_sentences(Format, [Sentence | Sentences]) :-
	format_sentence(Format, Sentence),
	format_sentences(Format, Sentences).


%% print_axiom_sentences(+Format:atom, +Axiom:term, +Sentences:atom)
%
print_axiom_sentences(html, Axiom, Sentences) :-
	!,
	format("<tr><td>~w</td><td>~w</td></tr>~n", [Axiom, Sentences]).

print_axiom_sentences(_, _Axiom, Sentences) :-
	write(Sentences).



%% format_sentence(+Format:atom, +Sentence:term)
%
% @param Format is one of {ace, csv, html}
% @param Sentence is an ACE sentence (list) or Comment (atom)
%
format_sentence(csv, Comment) :-
	atom(Comment),
	!,
	format("comment\t~w~n", [Comment]).

format_sentence(csv, RawSentence) :-
	!,
	maplist(format_as_csv, RawSentence).

format_sentence(_, Comment) :-
	atom(Comment),
	!,
	format("~w~n", [Comment]).

format_sentence(_, RawSentence) :-
	ace_niceace(RawSentence, Sentence),
	concat_atom(Sentence, ' ', SentenceAtom),
	format("~w~n", [SentenceAtom]).


format_as_csv(Term) :-
	functor(Term, Name, Arity),
	(
		Arity =:= 0
	->
		format("f\t~w~n", [Term])
	;
		arg(1, Term, Arg1),
		format("~w\t~w~n", [Name, Arg1])
	).


%% output_header(+Format:atom, +Results:list)
%
output_header(ace, _).
output_header(csv, _).
output_header(html, SentenceList) :-
	length(SentenceList, Length),
	format("<html><head><title>ACE verbalization of ~w OWL axioms</title><style type='text/css'>td { border: 1px solid black; padding: 0.3em 0.3em 0.3em 0.3em } thead { font-weight: bold; background-color: #4b4; font-variant: small-caps } table { font-size: 90%; empty-cells: show; border-collapse: collapse }</style></head><body><table>", [Length]).


%% output_footer(+Format:atom, +Results:list)
%
output_footer(ace, _).
output_footer(csv, _).
output_footer(html, _) :-
	format("</table></body></html>~n", []).
