:- module(elplusplus_generator, [
	axiom/3,
	class/3,
	make_axiom/1,
	make_axiom/2,
	make_axiom/3
	]).


/** <module> EL++ Generator

@author Kaarel Kaljurand
@version 2007-06-24

*/

%% axiom(-Prolog:term, +TokenList:list, []) is det.
%% axiom(+Prolog:term, -TokenList:list, []) is det.
%% axiom(-Prolog:term, -TokenList:list, []) is nondet.
%
% @param Prolog OWL axiom in Prolog notation
% @param TokenList OWL axioms as a list of tokens in Lisp notation
%
% BUG: maybe change '-' to '?'
%
% The following code can either:
%
% 1. Convert the OWL 2 Functional-Style Syntax of EL++ fragment into Prolog notation,
% given that there is a tokenization step before which normalizes spaces,
% converts character sequences into single tokens, etc.
%
% 2. Generate all possible EL++ axioms up to certain length (in tokens) in both
% the Lisp and the Prolog notation. The length of Lisp can be set by =|make_list/3|=.

axiom('SubClassOf'(C1, C2)) --> ['SubClassOf', '('], class(C1), class(C2), [')'].

% BUG: Add: EquivalentClasses

% BUG: Add: DisjointClasses


%% class(-Prolog:term, -TokenList:list, -RestTokenList:list) is nondet.
%
% @param Prolog OWL class description in Prolog notation
% @param TokenList OWL class description in Lisp notation (as a list of tokens)
% @param RestTokenList OWL class description in Lisp notation (usually should be empty)
%
% Converts a list of tokens into an OWL class description (EL++ fragment) in Prolog notation.
% The list of tokens corresponds to the OWL 2 Functional-Style Syntax.
% This predicate can also be used to generate all OWL class descriptions (up
% to a certain length).

class('ObjectIntersectionOf'(ClassList)) -->
	['ObjectIntersectionOf', '('], class_list(ClassList), [')'].

class('ObjectOneOf'([Individual])) -->
	['ObjectOneOf', '('], individual(Individual), [')'].

class('ObjectSomeValuesFrom'(Property, Class)) -->
	['ObjectSomeValuesFrom', '('], property(Property), class(Class), [')'].

class('ObjectHasValue'(Property, Individual)) -->
	['ObjectHasValue', '('], property(Property), individual(Individual), [')'].

class(protein) --> [protein].
class(gene) --> [gene].


%% class_list/3
%
%
class_list([Class1, Class2]) --> class(Class1), class(Class2).
class_list([Class | ClassList]) --> class(Class), class_list(ClassList).


% property/3
%
%
property(modify) --> [modify].


%% individual/3
%
%
individual('Met') --> ['Met'].


%% make_axiom/1
%% make_axiom/2
%% make_axiom(+Low:number, +High:number, -Axiom:term) is nodet.
%
% @param Low minimal number of tokens in the Axiom
% @param High maximum number of tokens in the Axiom
% @param Axiom OWL axiom
%
% Generate all possible El++ axioms up to a certain length.
%
make_axiom(Axiom) :-
	make_axiom(1, 10, Axiom).
	                        
make_axiom(High, Axiom) :-
	make_axiom(1, High, Axiom).
	                        
make_axiom(Low, High, Axiom) :-
	make_list(Low, High, List),
	axiom(Axiom, List, []).


%% make_list(+Low:number, +High:number, -List:list) is nodet.
%
% @param Low minimal number of elements in the list
% @param High maximum number of elements in the list
% @param List a finite list where all elements are (different) variables
%
make_list(Low, High, List) :-
	between(Low, High, Int),
	length(List, Int).
