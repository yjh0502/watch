-module(watch_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case os:find_executable("watchman") of
        false ->
            {stop, not_found};
        ExecPath ->
            P = open_port({spawn_executable, ExecPath},
                [{args, ["-p", "-j", "--server-encoding=json", "--output-encoding=bser"]},
                    binary, eof, use_stdio]),
            port_command(P, cmd()),
            {ok, #{
                port => P,
                data => <<>>,
                len => 0
            }}
    end.

handle_call(_Msg, _From, S) -> {reply, ok, S}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({P, eof}, #{port := P} = State) ->
    {stop, eof, State};
handle_info({P, {data, Data}}, #{data := Head, port := P} = State) ->
    {ok, State2} = handle_data(State#{data := <<Head/binary, Data/binary>>}),
    {noreply, State2}.

terminate(_Reason, #{port := P} = _State) ->
    port_close(P),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal functions
cmd() ->
    {ok, Pwd} = file:get_cwd(),
    iolist_to_binary(io_lib:format(
        "[\"subscribe\",\"~s\",\"mysub\",{\"fields\":[\"name\"],\"expression\":[\"allof\",[\"type\",\"f\"],[\"not\",\"empty\"],[\"suffix\",\"erl\"]]}]",
        [Pwd])).

handle_data(#{data := <<0, 1, Data/binary>>, len := 0} = State) ->
    {Tail, Len} = decode(Data),
    handle_data(State#{data := Tail, len := Len});
handle_data(#{data := Data, len := Len} = State) when Len > 0 andalso byte_size(Data) >= Len ->
    <<MsgBin:Len/binary, Tail/binary>> = Data,
    {<<>>, Ev} = decode(MsgBin),
    {ok, State2} = handle_watchman_event(Ev, State),
    handle_data(State2#{data := Tail, len := 0});

handle_data(State) ->
    {ok, State}.

handle_watchman_event(#{<<"subscribe">> := _Subscribe}, State) ->
    {ok, State};

handle_watchman_event(#{<<"root">> := Root, <<"files">> := Files} = _Ev, #{optmap := OptMap0} = State) ->
    NextOptMap = lists:foldl(fun(Filename, OptMap) ->
        {ok, Opts, OptMap2} = get_opt(Root, Filename, OptMap),
        NameStr = binary_to_list(filename:join(Root, Filename)),
        case filelib:last_modified(NameStr) of
            0 ->
                ok;
            _ ->
                case compile:file(NameStr, [binary, return | Opts]) of
                    {ok, Mod, ModBin, Warnings} ->
                        case code:get_object_code(Mod) of
                            {_, ModBin, _} ->
                                % skip reloading
                                ok;
                            _ ->
                                print_results(NameStr, [], Warnings),
                                case code:load_binary(Mod, NameStr, ModBin) of
                                    {module, Mod} ->
                                        compile:file(NameStr, Opts),
                                        ok;
                                    {error, Reason} ->
                                        error_logger:info_msg(io_lib:format("~s:0: Failed to load file: ~s.~n", [NameStr, Reason]))
                                end
                        end;
                    {error, Errors, Warnings} ->
                        print_results(NameStr, Errors, Warnings),
                        ok;
                    {error, Reason} ->
                        error_logger:info_msg(io_lib:format("~s:0: Failed to load file: ~s.~n", [NameStr, Reason])),
                        ok
                end
        end,
        OptMap2
    end, OptMap0, Files),
    {ok, State#{optmap := NextOptMap}};

handle_watchman_event(#{<<"root">> := Root, <<"files">> := Files}, State) ->
    % initial event
    OptMap = lists:foldl(fun(Filename, Acc) ->
        case get_opt(Root, Filename) of
            {true, FullDir, Opts} ->
                Acc#{FullDir => Opts};
            false ->
                Acc
        end
    end, #{}, Files),
    {ok, State#{
        optmap => OptMap
    }}.

get_opt(Root, Filename, OptMap) ->
    FullDir = dir(Root, Filename),
    case maps:get(FullDir, OptMap, undefined) of
        undefined ->
            case get_opt(Root, Filename) of
                {true, FullDir, Opts} ->
                    {ok, Opts, OptMap#{FullDir=>Opts}};
                false ->
                    % fallback
                    BaseDir = to_list(filename:dirname(FullDir)),
                    OutDir = filename:join(BaseDir, "ebin"),
                    IncludeDir = filename:join(BaseDir, "include"),
                    % make output directory
                    file:make_dir(OutDir),
                    Opts = [{outdir, OutDir}, {i, BaseDir}, {i, IncludeDir}],
                    {ok, Opts, OptMap}
            end;
        Opts ->
            {ok, Opts, OptMap}
    end.

get_opt(Root, Filename) ->
    ModName = filename:basename(Filename, <<".erl">>),
    try binary_to_existing_atom(ModName, utf8) of
        Mod ->
            case erlang:function_exported(Mod, module_info, 1) of
                true ->
                    Opts = proplists:get_value(options, Mod:module_info(compile), []),
                    FullDir = dir(Root, Filename),
                    {true, FullDir, Opts};
                false ->
                    false
            end
    catch
        error:_ -> false
    end.

dir(Root, Filename) ->
    Dir = filename:dirname(Filename),
    to_list(filename:join(Root, Dir)).

to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_list(List) when is_list(List) -> List.

print_results(SrcFile, [], []) ->
    Msg = io_lib:format("~s:0: Recompiled.~n", [SrcFile]),
    error_logger:info_msg(Msg);

print_results(SrcFile, [], Warnings) ->
    Msg = [
        format_errors(SrcFile, [], Warnings),
        io_lib:format("~s:0: Recompiled with ~p warnings~n", [SrcFile, length(Warnings)])
    ],
    error_logger:warning_msg(Msg);

print_results(SrcFile, Errors, Warnings) ->
    Msg = [
        format_errors(SrcFile, Errors, Warnings)
    ],
    error_logger:error_msg(Msg).

%% Print error messages in a pretty and user readable way.
format_errors(File, Errors, Warnings) ->
    AllErrors1 = lists:sort(lists:flatten([X || {_, X} <- Errors])),
    AllErrors2 = [{Line, "Error", Module, Description} || {Line, Module, Description} <- AllErrors1],
    AllWarnings1 = lists:sort(lists:flatten([X || {_, X} <- Warnings])),
    AllWarnings2 = [{Line, "Warning", Module, Description} || {Line, Module, Description} <- AllWarnings1],
    Everything = lists:sort(AllErrors2 ++ AllWarnings2),
    F = fun({Line, Prefix, Module, ErrorDescription}) ->
        Msg = format_error(Module, ErrorDescription),
        io_lib:format("~s:~p: ~s: ~s~n", [File, Line, Prefix, Msg])
    end,
    [F(X) || X <- Everything].

format_error(Module, ErrorDescription) ->
    case erlang:function_exported(Module, format_error, 1) of
        true -> Module:format_error(ErrorDescription);
        false -> io_lib:format("~s", [ErrorDescription])
    end.

%% bser decoding
decode(<<0, Tail/binary>>) ->
    {Tail2, Len} = decode(Tail),
    decode_list(Len, Tail2, []);
decode(<<1, Tail/binary>>) ->
    {Tail2, Len} = decode(Tail),
    decode_map(Len, Tail2, []);
decode(<<2, Tail/binary>>) ->
    {Tail2, Len} = decode(Tail),
    <<Str:Len/binary, Tail3/binary>> = Tail2,
    {Tail3, Str};

decode(<<3, Val:8/integer, Tail/binary>>) -> {Tail, Val};
decode(<<4, Val:16/little-integer, Tail/binary>>) -> {Tail, Val};
decode(<<5, Val:32/little-integer, Tail/binary>>) -> {Tail, Val};
decode(<<6, Val:64/little-integer, Tail/binary>>) -> {Tail, Val};

decode(<<7, Val:64/float, Tail/binary>>) -> {Tail, Val};
decode(<<8, Tail/binary>>) -> {Tail, false};
decode(<<9, Tail/binary>>) -> {Tail, true};
decode(<<10, Tail/binary>>) -> {Tail, null};
decode(<<11, Tail/binary>>) ->
    {Tail2, Props} = decode(Tail),
    {Tail3, N} = decode(Tail2),
    {Tail4, Values} = decode_list(N*length(Props), Tail3, []),
    {Tail4, decode_compact(N, Props, Values, [])};

decode(<<12, Tail/binary>>) -> {Tail, missing}.

decode_list(0, Bin, Acc) -> {Bin, lists:reverse(Acc)};
decode_list(N, Bin, Acc) ->
    {Tail, Val} = decode(Bin),
    decode_list(N-1, Tail, [Val | Acc]).

decode_map(0, Bin, Acc) -> {Bin, maps:from_list(Acc)};
decode_map(N, Bin, Acc) ->
    {Tail0, K} = decode(Bin),
    {Tail1, V} = decode(Tail0),
    decode_map(N-1, Tail1, [{K, V} | Acc]).

decode_compact(0, _Props, [], Acc) ->
    lists:reverse(Acc);
decode_compact(N, Props, Values, Acc) ->
    {Head, Tail} = lists:split(length(Props), Values),
    Map = maps:from_list(lists:zip(Props, Head)),
    decode_compact(N-1, Props, Tail, [Map | Acc]).

