% OWL 2 XML verbalizer command-line client
% (See README.txt for installation instructions and usage examples.)
%
% Author: Kaarel Kaljurand
% Version: 2011-06-11
%

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).

:- use_module(owlxml_owlfss, [
		owlxml_owlfss/3
	]).

:- use_module(owlfss_acetext, [
		owlfss_acetext/2
	]).

:- use_module(output_results, [
		output_results/2
	]).

:- use_module(lexicon, [
		init_lexicon/1
	]).

%
% Location of the webservice.
%
:- http_handler('/', owl_to_ace_handler, []).

:- dynamic(http_server_time_limit/1).

%% default_value(+Key:atom, -Value:atomic)
%
% Specifies the default values of the commandline parameters.
% 
default_value(format, ace).
default_value(workers, 4).
default_value(port, 8000).
% The default timelimit is 30 seconds,
% which should be enough even for large ontologies like GALEN.
default_value(timelimit, 30).


%% argument(?Arg, -Value, -Desc)
%
% Command-line arguments.
%
argument('-xml', 'FILENAME', 'Ontology (in the OWL/XML format) to be verbalized.').
argument('-format', 'STRING', 'Specify the output format, one of {ace, html, csv}.').
argument('-httpserver', '', 'Launch an HTTP interface to OWL verbalizer.').
argument('-port', 'NUMBER', 'Override the default port (8000) of the HTTP interface.').
argument('-workers', 'NUMBER', 'Override the default number of workers (4) on the HTTP server.').
argument('-timelimit', 'NUMBER', 'Time out after this number of seconds.').
argument('-version', '', 'Show version information.').
argument('-help', '', 'Show this help page.').


%% main
%
main :-
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

process_input(InputList) :-
	memberchk(xml=FileName, InputList),
	!,
	get_arg(format, InputList, Format),
	get_arg(timelimit, InputList, TimeLimit),
	cli(FileName, TimeLimit, Format).

process_input(InputList) :-
	memberchk(httpserver=on, InputList),
	!,
	get_arg(port, InputList, Port),
	get_arg(workers, InputList, Workers),
	get_arg(timelimit, InputList, TimeLimit),
	start_http_server(Port, Workers, TimeLimit).


%% get_arg(+Key:atom, +InputList:list, -Value:atomic)
%
% Gets the value of the input parameter if set,
% otherwise uses the default value.
%
get_arg(Key, InputList, Value) :-
	memberchk(Key=Value, InputList),
	!.

get_arg(Key, _, Value) :-
	default_value(Key, Value).


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
	format("OWL verbalizer, ver ~w~n", ['0.9.3']).


%% get_arglist(+RawArgList, -ArgList)
%
%
get_arglist(RawArgList, ArgList) :-
	append(_, ['--'|ArgList], RawArgList),
	!.

get_arglist([_|ArgList], ArgList).


% cli(+Filename:atom, +Timelimit:number, +Format:atom)
%
% This interface is for the commandline.
%
cli(Filename, TimeLimit, Format) :-
	call_with_time_limit(TimeLimit, owl_to_ace_cli(Filename, Format)).


owl_to_ace_cli(FileName, Format) :-
	open(FileName, read, Stream),
	% this closes the stream as well
	owlxml_owlfss(Stream, Ontology, _ErrorList),
	owl_to_ace(Ontology, cli, Format).

owl_to_ace('Ontology'(_Name, _NS, AxiomList), Mode, Format) :-
	owlfss_acetext(AxiomList, Results),
	current_stream(1, write, Stream),
	set_stream(Stream, encoding(utf8)),
	output_header(Mode, Format),
	output_results(Format, AxiomList, Results).


%% output_results(+Format:atom, +Results:list)
%
% In case of csv-output we do not use the lexicon.
%
output_results(csv, _, Results) :-
	!,
	output_results(csv, Results).

output_results(Format, AxiomList, Results) :-
	init_lexicon(AxiomList),
	output_results(Format, Results).


%% http_server(+PortNumber:integer, +WorkerCount:integer)
%
% @bug Make sure I understand what thread_get_message/1 does.
%
start_http_server(Port, Workers, TimeLimit) :-
	format("Starting owl_to_ace at port ~w with ~w workers and ~w sec time limit ...~n", [Port, Workers, TimeLimit]),
	assert(http_server_time_limit(TimeLimit)),
	http_server(http_dispatch, [port(Port), workers(Workers)]),
	thread_get_message(_),
	format("Stopping owl_to_ace ...~n"),
	halt.

owl_to_ace_handler(Request) :-
	default_value(format, DefaultFormat),
	http_server_time_limit(TimeLimit),
	catch(
		call_with_time_limit(
			TimeLimit,
			(
				http_parameters(Request, [
					xml(XmlAtom, []),
					format(Format, [oneof([ace, csv, html]), default(DefaultFormat)])
				]),
				atom_to_memory_file(XmlAtom, Handle),
				open_memory_file(Handle, read, Stream),
				% this closes the stream as well
				owlxml_owlfss(Stream, Ontology, _Errors),
				owl_to_ace(Ontology, http, Format)
			)
		),
		Exception,
		(
			format_error_for_http(Exception, ContentType, Content),
			format('Content-type: ~w\r\n\r\n~w', [ContentType, Content])
		)
	).


%% output_header(+Mode:atom, +Format:atom)
%
output_header(cli, _Format).

output_header(http, Format) :-
	format_to_mime(Format, Mime),
	format('Content-type: ~w\r\n\r\n', [Mime]).


%% format_to_mime(+Format:atom, -Mime:atom)
%
format_to_mime(ace, 'text/plain').
format_to_mime(csv, 'text/plain').
format_to_mime(html, 'text/html').


%% format_error_for_terminal(+Exception:term)
%
% Pretty-prints the exception term for the terminal.
%
% @param Exception is the exception term in the form
%        error(Formal, context(Name/Arity, Message))
%
format_error_for_terminal(error(Formal, context(_Name/_Arity, Message))) :-
	!,
	format(user_error, "ERROR: ~w: ~w~n", [Formal, Message]).

format_error_for_terminal(Error) :-
	format(user_error, "ERROR: ~w~n", [Error]).


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
	!,
	with_output_to(atom(Xml), format("<error type=\"~w\">~w</error>", [Formal, Message])).

format_error_for_http(Error, 'text/xml', Xml) :-
	with_output_to(atom(Xml), format("<error>~w</error>", [Error])).
