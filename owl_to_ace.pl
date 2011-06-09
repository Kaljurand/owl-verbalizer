% OWL 2 XML verbalizer command-line client
%
% Author: Kaarel Kaljurand
% Version: 2011-06-06
%
% Building the command-line client:
%
% swipl -O -F none -t halt -g "[owl_to_ace], qsave_program('owl_to_ace.exe', [goal(owl_to_ace), local(25000), global(50000)])."
%
% Using the command-line interface:
%
% ./owl_to_ace.exe -owlfile file.owx
%
% Starting the verbalizer HTTP service:
%
% ./owl_to_ace.exe -httpserver -port 5123
%
% Using the verbalizer service by loading e.g. the URL:
%
% http://localhost:5123/?xml=...
%

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).

:- use_module(owlxml_owlfss, [
		ellist_termlist/3,
		owlxml_owlfss/3
	]).

:- use_module(owlfss_acetext, [
		owlfss_acetext/2
	]).

:- use_module(output_results, [
		output_sentencelist/1,
		output_mapping/1
	]).

%% http_port(?PortNumber:integer)
%
% Port number of the webservice.
% This can be overridden on the command-line.
%
http_port(8000).


%
% Location of the webservice.
%
:- http_handler('/', owl_to_ace_handler, []).


%% cli_time_limit(?TimeLimit:integer)
%
% Command-line interface time limit.
% The default timelimit is 40 seconds,
% that should be enough even for large ontologies like GALEN.
%
cli_time_limit(40).


%% argument(?Arg, -Value, -Desc)
%
% Command-line arguments.
%
argument('-owlfile', 'FILENAME', 'The name of a file that contains an OWL ontology in OWL 2 XML format.').
argument('-httpserver', '', 'Launch an HTTP interface to OWL verbalizer at port 8000.').
argument('-port', 'NUMBER', 'Override the default port of the HTTP interface.').
argument('-html', '', 'Output an HTML table with the OWL->ACE mapping.').
argument('-version', '', 'Show version information.').
argument('-help', '', 'Show this help page.').




%% owl_to_ace
%
%
owl_to_ace :-
	current_prolog_flag(argv, RawArgList),
	get_arglist(RawArgList, ArgList),
	catch(
		( arglist_namevaluelist(ArgList, InputList), process_input(InputList) ),
		Exception,
		format_error_for_terminal(Exception)
	).


%% arglist_namevaluelist(+ArgList:list, -NameValueList:list)
%
% @param ArgList is a list of arguments
% @param NameValueList is a list of ArgumentName=ArgumentValue pairs
%
arglist_namevaluelist([], []).

% If argument comes with no value
arglist_namevaluelist([Arg | Tail1], [Name=on | Tail2]) :-
	argument(Arg, '', _),
	!,
	atom_concat('-', Name, Arg),
	arglist_namevaluelist(Tail1, Tail2).

% Else: argument must have a value
arglist_namevaluelist([Arg, ValueAtom | Tail1], [Name=Value | Tail2]) :-
	argument(Arg, _, _),
	\+ argument(ValueAtom, _, _),
	!,
	atom_concat('-', Name, Arg),
	(
		catch(atom_number(ValueAtom, ValueNumber), _, fail)
	->
		Value = ValueNumber
	;
		Value = ValueAtom
	),
	arglist_namevaluelist(Tail1, Tail2).

arglist_namevaluelist([Arg | _], _) :-
	argument(Arg, _, _),
	!,
	throw(error('Missing value for argument', context(arglist_namevaluelist/2, Arg))).

arglist_namevaluelist([Arg | _], _) :-
	throw(error('Illegal argument', context(arglist_namevaluelist/2, Arg))).


%% process_input(+InputList:list)
%
% @param InputList is a list of input parameters
%
process_input(InputList) :-
	( InputList = [] ; memberchk(help=on, InputList) ),
	!,
	show_help.

process_input(InputList) :-
	memberchk(version=on, InputList),
	!,
	show_version.

process_input([httpserver=on]) :-
	!,
	http_server.

process_input([httpserver=on, port=Port]) :-
	!,
	http_server(Port).

process_input([port=Port, httpserver=on]) :-
	!,
	http_server(Port).

process_input(InputList) :-
	memberchk(owlfile=FileName, InputList),
	cli_time_limit(TimeLimit),
	(
		memberchk(html=on, InputList)
	->
		owl_to_ace(FileName, TimeLimit, html(on))
	;
		owl_to_ace(FileName, TimeLimit, html(off))
	).


%% show_help
%
% Prints help.
%
show_help :-
	show_version,
	write('Copyright 2008-2011 Kaarel Kaljurand <kaljurand@gmail.com>\n'),
	write('This program comes with ABSOLUTELY NO WARRANTY.\n'),
	write('This is free software, and you are welcome to redistribute it under certain conditions.\n'),
	write('Please visit http://code.google.com/p/owlverbalizer/ for details.\n'),
	nl,
	write('Command-line arguments:\n'),
	argument(Arg, Value, Desc),
	\+ Desc = hidden,
	format('~w ~w~20|~w~n', [Arg, Value, Desc]),
	fail ; true.


%% show_version
%
% Prints the version information.
%
show_version :-
	format("OWL verbalizer, ver ~w~n", ['0.9.1']).


%% get_arglist(+RawArgList, -ArgList)
%
%
get_arglist(RawArgList, ArgList) :-
	append(_, ['--'|ArgList], RawArgList),
	!.

get_arglist([_|ArgList], ArgList).


% This interface is for the commandline.
owl_to_ace(FileName, TimeLimit, html(HtmlOnOff)) :-
	call_with_time_limit(
		TimeLimit,
		(
			owlxml_owlfss(FileName, Ontology, _ErrorList),
			owlfss_acetext(Ontology, SentenceList),
			current_stream(1, write, Stream),
			set_stream(Stream, encoding(utf8)),
			(
				HtmlOnOff = on
			->
				output_mapping(SentenceList)
			;
				output_sentencelist(SentenceList)
			)
		)
	).


%% http_server
%
%
http_server :-
	http_port(Port),
	http_server(Port).


%% http_server(+PortNumber:integer)
%
% Note: we use just one worker, because the OWL verbalizer is not thread-safe
% (lexicon.pl uses assert/1).
%
% @bug Make sure I understand what thread_get_message/1 does.
%
http_server(Port) :-
	format("Starting owl_to_ace at port ~w ...~n", [Port]),
	http_server(http_dispatch, [port(Port), workers(4)]),
	thread_get_message(_),
	format("Stopping owl_to_ace ...~n", [Port]),
	halt.

% @bug It would be better to read the input directly from a stream,
% not through http_parameters/2.
owl_to_ace_handler(Request) :-
	catch(
		call_with_time_limit(
			20,
			(
				http_parameters(Request, [xml(XmlAtom, [])]),
				atom_to_memory_file(XmlAtom, Handle),
				open_memory_file(Handle, read, InStream),
				load_structure(InStream, XML, [dialect(xml), space(remove)]),
				ellist_termlist(XML, []-_ErrorList, [Ontology]),
				owlfss_acetext(Ontology, SentenceList),
				current_stream(1, write, Stream),
				set_stream(Stream, encoding(utf8)),
				format('Content-type: ~w\r\n\r\n', ['text/plain']),
				output_sentencelist(SentenceList)
			)
		),
		Exception,
		(
			format_error_for_http(Exception, ContentType, Content),
			format('Content-type: ~w\r\n\r\n~w', [ContentType, Content])
		)
	).


%% format_error_for_terminal(+Exception:term)
%
% Pretty-prints the exception term for the terminal.
%
% @param Exception is the exception term in the form error(Formal, context(Name/Arity, Message))
%
format_error_for_terminal(error(Formal, context(_Name/_Arity, Message))) :-
	format(user_error, "ERROR: ~w: ~w~n", [Formal, Message]).


%% format_error_for_http(+Exception:term, -ContentType:atom, -Content:term)
%
% Generates an error message from the exception term.
%
% @param Exception is the exception term in the form error(Formal, context(Name/Arity, Message))
% @param ContentType is the content type that message is formated into, e.g. text/xml
% @param Content is the actual error message
%
format_error_for_http(error(type_error(_, WrongType), context(_Name/_Arity, Message)), 'text/xml', Xml) :-
	!,
	with_output_to(atom(Xml), format("<error type=\"Wrong input type\">~w: ~w</error>", [Message, WrongType])).

format_error_for_http(error(Formal, context(_Name/_Arity, Message)), 'text/xml', Xml) :-
	with_output_to(atom(Xml), format("<error type=\"~w\">~w</error>", [Formal, Message])).
