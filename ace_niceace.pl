% This file is part of the OWL verbalizer.
% Copyright 2008-2013, Kaarel Kaljurand <kaljurand@gmail.com>.
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

:- module(ace_niceace, [
		ace_niceace/2
	]).

/** <module> ACE beautifier

The ACE beautifier does the following modifications to a list
of ACE tokens.

==
* a -> an (if appropriate)
* a thing -> something
* every thing -> everything
* no thing -> nothing
* connect every comma and a period to the preceding word
* glue the quotes to the quoted strings
==

This code is not called in the csv-mode, i.e. it is left to the user
to perform these beautification transformations.

@author Kaarel Kaljurand
@version 2013-09-18

@bug this module calls the lexicon-module but does not explicitly import it.

*/

:- use_module(lexicon, [
		iri_fragment/2,
		get_lexicon_entry/3
	]).

%% ace_niceace(+TokenListIn:list, -TokenListOut:list) is det.
%
% @param TokenListIn is a list of ACE tokens
% @param TokenListOut is a list of ACE tokens
%
ace_niceace([], []) :-
	!.

ace_niceace(In, Out) :-
	ace_merge(In, Prefix, Rest),
	ace_niceace(Rest, RestOut),
	append(Prefix, RestOut, Out).


%% ace_merge(+TokenList:list, -Prefix:list, -NewTokenList:list) is nondet.
%
% @param TokenList is a list of ACE tokens
% @param Prefix is a list of ACE tokens
% @param NewTokenList is a list of ACE tokens
%
ace_merge(['Every', thing | Rest], [], ['Everything' | Rest]) :-
	!.

ace_merge(['No', thing | Rest], [], ['Nothing' | Rest]) :-
	!.

ace_merge([a, thing | Rest], [], [something | Rest]) :-
	!.

ace_merge([a, cn_sg(Iri) | Rest], [Article], [SurfaceForm | Rest]) :-
	!,
	(
		lexicon:call(cn_sg(Iri), SurfaceForm)
	->
		true
	;
		lexicon:iri_fragment(Iri, SurfaceForm)
	),
	word_article(SurfaceForm, Article).

ace_merge([qs(Token) | Rest], [], [TokenQuotes | Rest]) :-
	atom(Token),
	!,
	concat_atom(['"', Token, '"'], TokenQuotes).

ace_merge([Token, '.' | Rest], [TokenPeriod], Rest) :-
	!,
	my_concat_atom([Token, '.'], TokenPeriod).

ace_merge([Token, ',' | Rest], [TokenComma], Rest) :-
	!,
	my_concat_atom([Token, ','], TokenComma).

ace_merge([does, not, tv_pl(Iri) | Rest], [is, not, Article, SurfaceForm, of], Rest) :-
	verb_as_noun(Iri, Article, SurfaceForm),
	!.

ace_merge([do, not, tv_pl(Iri) | Rest], [are, not, Article, SurfaceForm, of], Rest) :-
	verb_as_noun(Iri, Article, SurfaceForm),
	!.

ace_merge([tv_sg(Iri) | Rest], [is, Article, SurfaceForm, of], Rest) :-
	verb_as_noun(Iri, Article, SurfaceForm),
	!.

ace_merge([tv_pl(Iri) | Rest], [are, Article, SurfaceForm, of], Rest) :-
	verb_as_noun(Iri, Article, SurfaceForm),
	!.

ace_merge([that, _, not, tv_vbg(Iri), by | Rest], [whose, SurfaceForm, is, not], Rest) :-
	verb_as_noun(Iri, _Article, SurfaceForm),
	!.

ace_merge([_, not, tv_vbg(Iri), by | Rest], ['\'s', SurfaceForm, is, not], Rest) :-
	verb_as_noun(Iri, _Article, SurfaceForm),
	!.

ace_merge([that, _, tv_vbg(Iri), by | Rest], [whose, SurfaceForm, is], Rest) :-
	verb_as_noun(Iri, _Article, SurfaceForm),
	!.

ace_merge([_, tv_vbg(Iri), by | Rest], ['\'s', SurfaceForm, is], Rest) :-
	verb_as_noun(Iri, _Article, SurfaceForm),
	!.

ace_merge([Token | Rest], [Token], Rest) :-
	atomic(Token),
	!.

ace_merge([Token | Rest], [SurfaceForm], Rest) :-
	functor(Token, _, 1),
	lexicon:call(Token, SurfaceForm),
	!.

ace_merge([Token | Rest], [SurfaceForm], Rest) :-
	functor(Token, _, 1),
	arg(1, Token, Iri),
	lexicon:iri_fragment(Iri, SurfaceForm).


%%
%
%
verb_as_noun(Iri, Article, SurfaceForm) :-
	get_lexicon_entry('CN_sg', Iri, SurfaceForm),
	!,
	word_article(SurfaceForm, Article).

%% word_article(+Word:atom, -Article:atom) is det.
%
% This code decides on the article (of the noun phrase)
% on the basis of a word (either adjective or noun).
%
% See also: <http://en.wikipedia.org/wiki/A_and_an>
%
% @param Word is an ACE token
% @param Article is an ACE indefinite article, one of {a, an}
%
word_article(Word, an) :-
	downcase_atom(Word, DowncaseWord),
	atom_chars(DowncaseWord, WordChars),
	good_an_letters(WordChars),
	\+ bad_an_letters(WordChars),
	!.

word_article(_, a).


%% good_an_letters(?LetterList:list) is nondet.
%
% @param LetterList is a list of letters that a word following 'an' can consist of
%
good_an_letters([a | _]).
good_an_letters([e | _]).
good_an_letters([i | _]).
good_an_letters([o | _]).
good_an_letters([u | _]).
good_an_letters([h, o, n, o, r, a, b, l, e | _]).
good_an_letters([h, e, i, r | _]).
good_an_letters([h, o, u, r | _]).

good_an_letters([f]).
good_an_letters([h]).
good_an_letters([l]).
good_an_letters([m]).
good_an_letters([n]).
good_an_letters([r]).
good_an_letters([s]).
good_an_letters([x]).

good_an_letters([f, '-' | _]).
good_an_letters([h, '-' | _]).
good_an_letters([l, '-' | _]).
good_an_letters([m, '-' | _]).
good_an_letters([n, '-' | _]).
good_an_letters([r, '-' | _]).
good_an_letters([s, '-' | _]).
good_an_letters([x, '-' | _]).


%% bad_an_letters(?LetterList:list) is nondet.
%
% @param LetterList is a list of letters that a word following 'an' cannot consist of
%
bad_an_letters([u]).
bad_an_letters([u, '-' | _]).
bad_an_letters([u, r, i | _]).
bad_an_letters([u, t, i | _]).
bad_an_letters([u, n, i | _]).
bad_an_letters([u, s, a | _]).
bad_an_letters([u, s, e | _]).
%bad_an_letters([u, k, '-' | _]).
bad_an_letters([u, k | _]).
bad_an_letters([o, n, e | _]).

% TODO: temporary hack
my_concat_atom(List, Atom) :-
	maplist(my_term_to_atom, List, AtomList),
	concat_atom(AtomList, Atom).

% TODO: temporary hack
my_term_to_atom(Term, Atom) :-
	functor(Term, _, 1),
	!,
	lexicon:call(Term, Atom).
my_term_to_atom(Atom, Atom).
