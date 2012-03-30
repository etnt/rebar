%% ---------------------------------------------------------------------
%% Created: 6 Mar 2012 by etnt@redhoterlang.com
%%          Based on the surefire EUnit reporter by
%%          Mickaël Rémond, Paul Guyot
%%
%% @doc Eunit reporter, produces HTML.
%%
%% The HTML report produced also links into the code coverage rersult.
%% Hence, the following rebar.config is recommended:
%%
%% <pre>
%%   {eunit_report_format,    html}.
%%   {cover_enabled,          true}.
%%   {cover_print_enabled,    true}.
%% </pre>
%%
%% The Eterm output is experimental. The intention is to use it for
%% gathering the output in a structured manner for other kinds of
%% post-processing.
%% @end
%% ---------------------------------------------------------------------
-module(rebar_eunit_report).

-behaviour(eunit_listener).

-include("rebar.hrl").

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
	 terminate/2]).

%% ============================================================================
%% MACROS
%% ============================================================================
-define(XMLDIR, ".").
-define(INDENT, <<"  ">>).
-define(NEWLINE, <<"\n">>).

%% ============================================================================
%% TYPES
%% ============================================================================
-type(chars() :: [char() | any()]). % chars()

%% ============================================================================
%% RECORDS
%% ============================================================================
-record(testcase,
	{
	  name :: chars(),
	  description :: chars(),
	  result :: ok | {failed, tuple()} | {aborted, tuple()} | {skipped, tuple()},
	  time :: integer(),
	  output :: binary()
	 }).

-record(testsuite,
	{
	  name = <<>> :: binary(),
	  time = 0 :: integer(),
	  output = <<>> :: binary(),
	  succeeded = 0 :: integer(),
	  failed = 0 :: integer(),
	  aborted = 0 :: integer(),
	  skipped = 0 :: integer(),
	  testcases = [] :: [#testcase{}]
    }).

-record(index,
        {
          application = "" :: string(),
          result = [] :: [tuple()], % [{pass,27},{fail,0},{skip,0},{cancel,0}]
          files  = [] :: [tuple()]  % {Module,Filename}
        }).

-record(state, {verbose     = false,
		indent      = 0,
		xmldir      = ".",
                format      = html,  % html | eterm | all
                options     = [],
                application = "",
		testsuite   = #testsuite{},
                testsuites  = [], % processed #testsuite{} records - [#testcases{}]
                id_info     = []     % [{Id,Desc}]
	       }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    XMLDir = proplists:get_value(dir, Options, ?XMLDIR),
    os:cmd("rm -f EUNIT-*"),
    St = #state{verbose     = proplists:get_bool(verbose, Options),
		xmldir      = XMLDir,
                format      = proplists:get_value(format, Options, html),
                application = proplists:get_value(application, Options, ""),
                options     = Options,
		testsuite   = #testsuite{}},
    receive
	{start, _Reference} ->
	    St
    end.

%% _Data = [{pass,27},{fail,0},{skip,0},{cancel,0}]
terminate({ok, Data}, St) ->
    kwrite_report_index(St, Data),
    ok;
terminate({error, Reason}, _St) ->
    io:fwrite("Internal error: ~P.\n", [Reason, 25]),
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

handle_begin(group, Data, St) ->
    NewId = proplists:get_value(id, Data),
    case NewId of
	[] ->
	    St;

	[GroupId] ->
	    Desc = proplists:get_value(desc, Data),
	    TestSuite = St#state.testsuite,
	    NewTestSuite = TestSuite#testsuite{name = Desc},
	    St#state{testsuite=NewTestSuite, 
                     id_info = [{GroupId,Desc}|St#state.id_info]};

	[GroupId|_] ->
	    Desc = proplists:get_value(GroupId, St#state.id_info),
	    TestSuite = St#state.testsuite,
	    NewTestSuite = TestSuite#testsuite{name = Desc},
	    St#state{testsuite=NewTestSuite}

    end;
handle_begin(test, _Data, St) ->
    St.

handle_end(group, Data, St) ->
    %% Retrieve existing test suite:
    case proplists:get_value(id, Data) of
	[] ->
	    St#state{testsuite=#testsuite{}};
	[_GroupId] ->
	    TestSuite = St#state.testsuite,

	    %% Update TestSuite data:
	    Time = proplists:get_value(time, Data),
	    Output = proplists:get_value(output, Data),
	    NewTestSuite = TestSuite#testsuite{ time = Time, output = Output },
            XmlDir = St#state.xmldir,
            kwrite_report(NewTestSuite, XmlDir, St#state.format),
            TSs = [NewTestSuite#testsuite{testcases=[]} | St#state.testsuites],
	    St#state{testsuite=#testsuite{}, testsuites=TSs};

	_ ->
	    St
    end;
handle_end(test, Data, St) ->
    %% Retrieve existing test suite:
    TestSuite = St#state.testsuite,

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
		       proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Result = proplists:get_value(status, Data),
    Time = proplists:get_value(time, Data),
    Output = proplists:get_value(output, Data),
    TestCase = #testcase{name = Name, description = Desc,
			 time = Time,output = Output},
    NewTestSuite = add_testcase_to_testsuite(Result, TestCase, TestSuite),
    St#state{testsuite=NewTestSuite}.

%% Cancel group does not give information on the individual cancelled test case
%% We ignore this event
handle_cancel(group, _Data, St) ->
    St;
handle_cancel(test, Data, St) ->
    %% Retrieve existing test suite:
    TestSuite = St#state.testsuite,

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
		       proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Reason = proplists:get_value(reason, Data),
    TestCase = #testcase{
      name = Name, description = Desc,
      result = {skipped, Reason}, time = 0,
      output = <<>>},
    NewTestSuite = TestSuite#testsuite{
		     skipped = TestSuite#testsuite.skipped+1,
		     testcases=[TestCase|TestSuite#testsuite.testcases] },
    St#state{testsuite=NewTestSuite}.

format_name({Module, Function, Arity}, Line) ->
    lists:flatten([atom_to_list(Module), ":", atom_to_list(Function), "/",
		   integer_to_list(Arity), "_", integer_to_list(Line)]).
format_desc(undefined) ->
    "";
format_desc(Desc) when is_binary(Desc) ->
    binary_to_list(Desc);
format_desc(Desc) when is_list(Desc) ->
    Desc.

%% Add testcase to testsuite depending on the result of the test.
add_testcase_to_testsuite(ok, TestCaseTmp, TestSuite) ->
    TestCase = TestCaseTmp#testcase{ result = ok },
    TestSuite#testsuite{
      succeeded = TestSuite#testsuite.succeeded+1,
      testcases=[TestCase|TestSuite#testsuite.testcases] };
add_testcase_to_testsuite({error, Exception}, TestCaseTmp, TestSuite) ->
    case Exception of
	{error,{AssertionException,_},_} when
	AssertionException == assertion_failed;
	AssertionException == assertMatch_failed;
	AssertionException == assertEqual_failed;
	AssertionException == assertException_failed;
	AssertionException == assertCmd_failed;
	AssertionException == assertCmdOutput_failed
	->
	    TestCase = TestCaseTmp#testcase{ result = {failed, Exception} },
	    TestSuite#testsuite{
	      failed = TestSuite#testsuite.failed+1,
	      testcases = [TestCase|TestSuite#testsuite.testcases] };
	_ ->
	    TestCase = TestCaseTmp#testcase{ result = {aborted, Exception} },
	    TestSuite#testsuite{
	      aborted = TestSuite#testsuite.aborted+1,
	      testcases = [TestCase|TestSuite#testsuite.testcases] }
    end.

%% ----------------------------------------------------------------------------
%% Write a report as an Erlang term
%% ----------------------------------------------------------------------------

kwrite_report_index(#state{xmldir      = XmlDir,
                           format      = Format,
                           application = App,
                           testsuites  = TSs},   Result) ->
    case Format of
        eterm ->
            ewrite_report_index(TSs, Result, App, XmlDir);
        html ->
            hwrite_report_index(TSs, Result, App, XmlDir);
        _ ->
            ewrite_report_index(TSs, Result, App, XmlDir),
            hwrite_report_index(TSs, Result, App, XmlDir)
    end.

hwrite_report_index(TSs, Result, App, XmlDir) ->
    Filename = result_filename("-index", XmlDir, ".html"),

    KV2Str  = fun(Key,L) -> to_str(proplists:get_value(Key,L)) end,
    Summary =
        {table,[],
         [{tr,[],[{td,[],["Passed:"]},  {td,[],[KV2Str(pass,Result)]}]},
          {tr,[],[{td,[],["Failed:"]},  {td,[],[KV2Str(fail,Result)]}]},
          {tr,[],[{td,[],["Canceled:"]},{td,[],[KV2Str(cancel,Result)]}]},
          {tr,[],[{td,[],["Skipped:"]}, {td,[],[KV2Str(skip,Result)]}]}
         ]},

    Rows = lists:map(
             fun(#testsuite{name      = Name,
                            succeeded = Succeeded,
                            failed    = Failed,
                            aborted   = Aborted,
                            skipped   = Skipped} ) ->
                     Fname = result_filename(escape_suitename(Name), XmlDir, ".html"),
                     {tr,[],
                      [{td,[],[{a,[{href,Fname}],[Fname]}]}|
                       [{td,[],[to_str(X)]} || X <- [Succeeded,Failed,Aborted,Skipped]]]}
             end, lists:keysort(#testsuite.name, TSs)),

    Table = {table,[],
             [{tr,[],[{th,[],["Testsuite"]},{th,[],["Succeded"]},
                      {th,[],["Failed"]},{th,[],["Aborted"]},
                      {th,[],["Skipped"]}]}
              | Rows]},

    Ehtml = {html,[],
             [{head,[],[css()]},
              {body,[],
               [{h2,[],["Test suite for the "++to_str(App)++" application"]},
                {'div',[{class,"ts_summary"}],[Summary]},
                {'div',[],
                 [{a,[{href,"index.html"}],
                   ["Coverage Analysis Overview"]}]},
                {'div',[{class,"tc_files"}],[Table]}]}]},

    %%error_logger:info_msg("+++++ EHTML: ~p~n",[Ehtml]),
    Html = xmerl:export_simple([Ehtml], xmerl_html),
    write_string(Filename, Html),
    ?CONSOLE("EUnit report: ~s\n", 
             [filename:join([rebar_utils:get_cwd(), Filename])]).


kwrite_report(TestSuite, XmlDir, Format) ->
    case Format of
        eterm ->
            ewrite_report(TestSuite, XmlDir);
        html ->
            hwrite_report(TestSuite, XmlDir);
        _ ->
            ewrite_report(TestSuite, XmlDir),
            hwrite_report(TestSuite, XmlDir)
    end.


hwrite_report(#testsuite{testcases = TCs,
                         name      = TSname,
                         succeeded = Succeeded,
                         failed    = Failed,
                         aborted   = Aborted,
                         skipped   = Skipped} = TestSuite,
              XmlDir) ->
    Filename = result_filename(TestSuite, XmlDir, ".html"),

    Summary = 
        {table,[],
         [{tr,[],[{td,[],["Passed:"]}, {td,[],[to_str(Succeeded)]}]},
          {tr,[],[{td,[],["Failed:"]}, {td,[],[to_str(Failed)]}]},
          {tr,[],[{td,[],["Aborted:"]},{td,[],[to_str(Aborted)]}]},
          {tr,[],[{td,[],["Skipped:"]},{td,[],[to_str(Skipped)]}]}
         ]},

    Rows = lists:map(
             fun(#testcase{name        = Name,
                           time        = Time,
                           result      = Result,
                           description = Desc}) ->
                     {tr,[],
                      [{td,[],[Name]},
                       {td,[],[to_str(Time)]},
                       {td,[],[term2str(Result)]},
                       {td,[],[Desc]}]}
             end, TCs),

    Table = {table,[],
             [{tr,[],[{th,[],["Mod:Fun/Arity_Line"]},{th,[],["Time"]},
                      {th,[],["Result"]},{th,[],["Description"]}]}
              | Rows]},

    Ehtml = {html,[],
             [{head,[],[css()]},
              {body,[],
               [{h2,[],["Test suite for the "++escape_suitename(TSname)++" module"]},
                {'div',[{class,"tc_summary"}],[Summary]},
                {'div',[],
                 [{a,[{href,escape_suitename(TSname)++".COVER.html"}],
                   ["Coverage Analysis"]}]},
                {'div',[{class,"testcases"}],[Table]}]}]},

    %%error_logger:info_msg("+++++ EHTML: ~p~n",[Ehtml]),
    Html = xmerl:export_simple([Ehtml], xmerl_html),
    write_string(Filename, Html).

css() ->
    {style, [{type,"text/css"}],
     ["body {padding: 1em;}\n"
      ".ts_summary td {align: right;}\n"
      ".tc_files {margin: 1em 0;}\n"
      ".tc_files th {padding-right: 1em;align: left;}\n"
      ".tc_files td {padding-right: 1em;}\n"
      ".tc_summary td {align: right;}\n"
      ".testcases {margin: 1em 0;}\n"
      ".testcases th {padding-right: 1em;align: left;}\n"
      ".testcases td {padding-right: 1em;}\n"
     ]}.

ewrite_report(TestSuite, XmlDir) ->
    Filename = result_filename(TestSuite, XmlDir, ".eterm"),
    write_term(Filename, TestSuite).

ewrite_report_index(TSs, Result, App, XmlDir) ->
    Filename = result_filename("-index", XmlDir, ".eterm"),
    X = #index{application = App,
               result      = Result,
               files       = TSs},
    write_term(Filename, X),
    ?CONSOLE("EUnit report: ~s\n", 
             [filename:join([rebar_utils:get_cwd(), Filename])]).


write_term(Filename, Term) ->
    write(Filename, "~p.~n", [Term]).

write_string(Filename, String) ->
    write(Filename, "~s~n", [String]).

write(Filename, FmtStr, Data) ->
    case file:open(Filename, [write]) of
        {ok, FileDescriptor} ->
            try
                io:fwrite(FileDescriptor, FmtStr, Data)
            after
                file:close(FileDescriptor)
            end;
        {error, _Reason} = Error -> throw(Error)
    end.




result_filename(#testsuite{name = Name}, XmlDir, Suffix) ->
    filename:join(XmlDir, lists:flatten(["EUNIT-", escape_suitename(Name)], Suffix));
result_filename(Name, XmlDir, Suffix) when is_list(Name) ->
    filename:join(XmlDir, "EUNIT-"++Name++Suffix).

term2str(Term) ->
    lists:flatten(io_lib:format("~p",[Term])).

%% ----------------------------------------------------------------------------
%% Escape a suite's name to generate the filename.
%% Remark: we might overwrite another testsuite's file.
%% ----------------------------------------------------------------------------
escape_suitename([Head | _T] = List) when is_list(Head) ->
    escape_suitename(lists:flatten(List));
escape_suitename(Binary) when is_binary(Binary) ->
    escape_suitename(binary_to_list(Binary));
escape_suitename("module '" ++ String) ->
    escape_suitename(String);
escape_suitename(String) ->
    escape_suitename(String, []).

escape_suitename(Binary, Acc) when is_binary(Binary) ->
    escape_suitename(binary_to_list(Binary), Acc);
escape_suitename([], Acc) ->
    lists:reverse(Acc);
escape_suitename([$  | Tail], Acc) ->
    escape_suitename(Tail, [$_ | Acc]);
escape_suitename([$' | Tail], Acc) ->
    escape_suitename(Tail, Acc);
escape_suitename([$/ | Tail], Acc) ->
    escape_suitename(Tail, [$: | Acc]);
escape_suitename([$\\ | Tail], Acc) ->
    escape_suitename(Tail, [$: | Acc]);
escape_suitename([Char | Tail], Acc) when Char < $! ->
    escape_suitename(Tail, Acc);
escape_suitename([Char | Tail], Acc) when Char > $~ ->
    escape_suitename(Tail, Acc);
escape_suitename([Char | Tail], Acc) ->
    escape_suitename(Tail, [Char | Acc]).


to_str(I) when is_integer(I) -> integer_to_list(I);
to_str(A) when is_atom(A)    -> atom_to_list(A);
to_str(L) when is_list(L)    -> L.
