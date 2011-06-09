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
		output_results/2,
		output_mapping/1
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


%% output_results(+Format:atom, +SentenceList:list)
%
% Outputs the given ACE sentences. The SentenceList
% that is given as input is a list of Axiom-Sentence pairs.
%
% @param SentenceList is a list of ACE sentences
%
output_results(_Format, []).

output_results(Format, [_-[] | SentenceList]) :-
	!,
	output_results(Format, SentenceList).

output_results(Format, [Axiom-Message | SentenceList]) :-
	atom(Message),
	!,
	format("/* ~w: ~w */~n", [Message, Axiom]),
	output_results(Format, SentenceList).

output_results(Format, [_Axiom-SentenceList | AxiomSentenceListList]) :-
	format_sentences(Format, SentenceList),
	nl,
	output_results(Format, AxiomSentenceListList).


format_sentences(ace, Sentences) :-
	format_sentencelist(Sentences).


%% format_sentencelist(+SentenceList:list)
%
% @param SentenceList is a list of ACE sentences
%
format_sentencelist([]).

format_sentencelist([Comment | SentenceList]) :-
	atom(Comment),
	!,
	format("~w~n", [Comment]),
	format_sentencelist(SentenceList).

format_sentencelist([RawSentence | SentenceList]) :-
	ace_niceace(RawSentence, Sentence),
	concat_atom(Sentence, ' ', SentenceAtom),
	format("~w~n", [SentenceAtom]),
	format_sentencelist(SentenceList).


%% output_mapping(+SentenceList:list)
%
% @param SentenceList is a list of ACE sentences
%
output_mapping(SentenceList) :-
	length(SentenceList, Length),
	format("<html><head><title>ACE verbalization of ~w OWL axioms</title><style type='text/css'>td { border: 1px solid black; padding: 0.3em 0.3em 0.3em 0.3em } thead { font-weight: bold; background-color: #4b4; font-variant: small-caps } table { font-size: 90%; empty-cells: show; border-collapse: collapse }</style></head><body><table>", [Length]),
	output_mapping_x(SentenceList),
	format("</table></body></html>~n", []).


%% output_mapping_x(+SentenceList:list)
%
% @param SentenceList is a list of ACE sentences
%
output_mapping_x([]).

output_mapping_x([_-[] | SentenceList]) :-
	!,
	output_mapping_x(SentenceList).

output_mapping_x([Axiom-Message | SentenceList]) :-
	atom(Message),
	!,
	format("<tr style='color: red'><td>~w</td><td>~w</td></tr>~n", [Axiom, Message]),
	output_mapping_x(SentenceList).

output_mapping_x([Axiom-SentenceList | AxiomSentenceListList]) :-
	with_output_to(atom(AceText), format_sentencelist(SentenceList)),
	format("<tr><td>~w</td><td>~w</td></tr>~n", [Axiom, AceText]),
	output_mapping_x(AxiomSentenceListList).
