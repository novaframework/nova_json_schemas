-module(nova_json_schemas).
-behaviour(nova_plugin).

-export([
    init/0,
    load_local_schemas/0,
    pre_request/4,
    post_request/4,
    plugin_info/0
]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-export([
    render_errors/1,
    render_one_error/1,
    build_problem_details/3,
    group_errors_by_field/1,
    to_json_pointer/1,
    format_error_message/3,
    safe_format/1,
    safe_value/1,
    validate_json/3
]).
-endif.

init() ->
    jesse_database:load_all(),
    load_local_schemas(),
    #{}.

-spec load_local_schemas() -> ok.
load_local_schemas() ->
    {ok, MainApp} = nova:get_main_app(),
    PrivDir = code:priv_dir(MainApp),
    SchemasDir = filename:join([PrivDir, "schemas"]),
    case filelib:is_dir(SchemasDir) of
        true ->
            load_schemas_from_dir(SchemasDir, "schemas");
        false ->
            ?LOG_WARNING("Schemas directory not found: ~s", [SchemasDir])
    end,
    ok.

-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}
    | {stop, Req0 :: cowboy_req:req(), NewState :: any()}
    | {error, Reason :: term()}.
pre_request(
    Req = #{extra_state := #{json_schema := SchemaLocation} = Extra, json := JSON},
    _Env,
    Options,
    State
) ->
    JesseOpts = maps:get(jesse_options, Extra, []),
    case validate_json(SchemaLocation, JSON, JesseOpts) of
        ok ->
            ?LOG_DEBUG("Schema validation on JSON body successful"),
            {ok, Req, State};
        {error, Errors} ->
            ?LOG_DEBUG("Got validation-errors on JSON body. Errors: ~p", [Errors]),
            StatusCode = maps:get(status_code, Options, 422),
            case maps:get(render_errors, Options, false) of
                true ->
                    GroupByField = maps:get(group_by_field, Options, false),
                    ErrorList = render_errors(Errors),
                    ErrorBody = build_problem_details(StatusCode, ErrorList, GroupByField),
                    ErrorJson = json:encode(ErrorBody),
                    Req0 = cowboy_req:set_resp_headers(
                        #{<<"content-type">> => <<"application/problem+json">>}, Req
                    ),
                    Req1 = cowboy_req:set_resp_body(ErrorJson, Req0),
                    Req2 = cowboy_req:reply(StatusCode, Req1),
                    {stop, Req2, State};
                _ ->
                    Req0 = cowboy_req:reply(StatusCode, Req),
                    {stop, Req0, State}
            end
    end;
pre_request(#{extra_state := #{json_schema := _SchemaLocation}}, _Env, _Options, _State) ->
    ?LOG_ERROR(
        "JSON Schema is set in 'extra_state' but body have not yet been parsed - rearrange your plugins so that JSON plugin is ran before this.."
    ),
    {error, body_not_parsed};
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-spec plugin_info() ->
    #{
        title := binary(),
        version := binary(),
        url := binary(),
        authors := [binary()],
        description := binary(),
        options := [{Key :: atom(), OptionDescription :: binary()}]
    }.
plugin_info() ->
    #{
        title => <<"Nova JSON Schema plugin">>,
        version => <<"0.3.0">>,
        url => <<"https://github.com/novaframework/nova_json_schemas">>,
        authors => [<<"Niclas Axelsson <niclas@burbas.se>">>],
        description => <<"Validates JSON request bodies against JSON schemas using jesse">>,
        options => [
            {render_errors,
                <<"If true, validation errors are returned as RFC 9457 problem+json to the requester">>},
            {status_code, <<"HTTP status code for validation errors (default: 422)">>},
            {group_by_field,
                <<"If true, errors are grouped by field name in the response (default: false)">>}
        ]
    }.

%%% Internal functions

validate_json(SchemaLocation, Json, JesseOpts) ->
    Key = ensure_list(SchemaLocation),
    case jesse:validate(Key, Json, JesseOpts) of
        {error, {database_error, _, schema_not_found}} ->
            {ok, MainApp} = nova:get_main_app(),
            PrivDir = code:priv_dir(MainApp),
            SchemaLocation0 = filename:join([PrivDir, SchemaLocation]),
            case file:read_file(SchemaLocation0) of
                {ok, Filecontent} ->
                    Schema = json:decode(Filecontent),
                    jesse:add_schema(SchemaLocation, Schema),
                    validate_json(SchemaLocation, Json, JesseOpts);
                {error, Reason} ->
                    ?LOG_ERROR("Failed to read schema file ~s: ~p", [SchemaLocation0, Reason]),
                    {error, [{schema_invalid, SchemaLocation, {file_error, Reason}}]}
            end;
        {error, ValidationError} ->
            {error, ValidationError};
        {ok, _} ->
            ok
    end.

build_problem_details(StatusCode, ErrorList, true) ->
    #{
        type => <<"about:blank">>,
        title => <<"Validation Error">>,
        status => StatusCode,
        detail => <<"Request body failed JSON schema validation">>,
        errors => group_errors_by_field(ErrorList)
    };
build_problem_details(StatusCode, ErrorList, false) ->
    #{
        type => <<"about:blank">>,
        title => <<"Validation Error">>,
        status => StatusCode,
        detail => <<"Request body failed JSON schema validation">>,
        errors => ErrorList
    }.

group_errors_by_field(Errors) ->
    lists:foldl(
        fun(#{path := Path, message := Msg}, Acc) ->
            Existing = maps:get(Path, Acc, []),
            Acc#{Path => Existing ++ [Msg]}
        end,
        #{},
        Errors
    ).

render_errors([]) ->
    [];
render_errors([Error | Tl]) ->
    [render_one_error(Error) | render_errors(Tl)].

render_one_error({data_invalid, _Schema, {ErrorType, Details}, Value, Path}) ->
    #{
        path => to_json_pointer(Path),
        type => ErrorType,
        message => format_error_message(ErrorType, Details, Value),
        actual_value => safe_value(Value),
        expected => safe_value(Details)
    };
render_one_error({data_invalid, _Schema, ErrorType, Value, Path}) ->
    #{
        path => to_json_pointer(Path),
        type => ErrorType,
        message => format_error_message(ErrorType, undefined, Value),
        actual_value => safe_value(Value)
    };
render_one_error({schema_invalid, _Schema, {ErrorType, Details}}) ->
    #{
        path => <<"">>,
        type => ErrorType,
        message => format_error_message(ErrorType, Details, undefined)
    };
render_one_error({schema_invalid, _Schema, ErrorType}) ->
    #{
        path => <<"">>,
        type => ErrorType,
        message => format_error_message(ErrorType, undefined, undefined)
    };
render_one_error({data_error, {parse_error, Details}}) ->
    #{
        path => <<"">>,
        type => parse_error,
        message => iolist_to_binary(io_lib:format("Parse error: ~p", [Details]))
    };
render_one_error({schema_error, {parse_error, Details}}) ->
    #{
        path => <<"">>,
        type => schema_parse_error,
        message => iolist_to_binary(io_lib:format("Schema parse error: ~p", [Details]))
    };
render_one_error(Unknown) ->
    ?LOG_WARNING("Unhandled validation error format: ~p", [Unknown]),
    #{
        path => <<"">>,
        type => unknown_error,
        message => iolist_to_binary(io_lib:format("~p", [Unknown]))
    }.

to_json_pointer([]) ->
    <<"/">>;
to_json_pointer(Path) when is_list(Path) ->
    Segments = [escape_json_pointer(segment_to_binary(S)) || S <- Path],
    iolist_to_binary([<<"/">>, lists:join(<<"/">>, Segments)]).

segment_to_binary(S) when is_binary(S) -> S;
segment_to_binary(S) when is_integer(S) -> integer_to_binary(S);
segment_to_binary(S) when is_atom(S) -> atom_to_binary(S);
segment_to_binary(S) -> iolist_to_binary(io_lib:format("~p", [S])).

escape_json_pointer(Bin) ->
    binary:replace(binary:replace(Bin, <<"~">>, <<"~0">>, [global]), <<"/">>, <<"~1">>, [global]).

format_error_message(wrong_type, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Must be of type ~s", [safe_format(Expected)]));
format_error_message(not_in_enum, _Expected, _Value) ->
    <<"Value is not in the allowed set of values">>;
format_error_message(not_in_range, Expected, _Value) ->
    iolist_to_binary(
        io_lib:format("Value is not in the allowed range ~s", [safe_format(Expected)])
    );
format_error_message(missing_required_property, Property, _Value) ->
    iolist_to_binary(io_lib:format("Missing required property: ~s", [safe_format(Property)]));
format_error_message(no_extra_properties_allowed, _Expected, _Value) ->
    <<"Additional properties are not allowed">>;
format_error_message(no_extra_items_allowed, _Expected, _Value) ->
    <<"Additional items are not allowed">>;
format_error_message(not_allowed, _Expected, _Value) ->
    <<"Value is not allowed">>;
format_error_message(not_unique, _Expected, _Value) ->
    <<"Array items are not unique">>;
format_error_message(wrong_size, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Array size is invalid, expected ~s", [safe_format(Expected)]));
format_error_message(wrong_length, Expected, _Value) ->
    iolist_to_binary(
        io_lib:format("String length is invalid, expected ~s", [safe_format(Expected)])
    );
format_error_message(wrong_format, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Value does not match format: ~s", [safe_format(Expected)]));
format_error_message(not_divisible, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Value is not divisible by ~s", [safe_format(Expected)]));
format_error_message(not_multiple_of, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Value is not a multiple of ~s", [safe_format(Expected)]));
format_error_message(no_match, _Expected, _Value) ->
    <<"Value does not match the required pattern">>;
format_error_message(all_schemas_not_valid, _Expected, _Value) ->
    <<"Value does not match all of the required schemas">>;
format_error_message(any_schemas_not_valid, _Expected, _Value) ->
    <<"Value does not match any of the required schemas">>;
format_error_message(not_one_schema_valid, _Expected, _Value) ->
    <<"Value does not match exactly one of the required schemas">>;
format_error_message(more_than_one_schema_valid, _Expected, _Value) ->
    <<"Value matches more than one of the required schemas">>;
format_error_message(not_schema_valid, _Expected, _Value) ->
    <<"Value must not match the schema">>;
format_error_message(too_many_properties, Expected, _Value) ->
    iolist_to_binary(
        io_lib:format("Too many properties, maximum allowed: ~s", [safe_format(Expected)])
    );
format_error_message(too_few_properties, Expected, _Value) ->
    iolist_to_binary(
        io_lib:format("Too few properties, minimum required: ~s", [safe_format(Expected)])
    );
format_error_message(missing_dependency, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Missing dependency: ~s", [safe_format(Expected)]));
format_error_message(file_error, Reason, _Value) ->
    iolist_to_binary(io_lib:format("Schema file error: ~p", [Reason]));
format_error_message(Type, undefined, _Value) ->
    iolist_to_binary(io_lib:format("Validation failed: ~s", [Type]));
format_error_message(Type, Expected, _Value) ->
    iolist_to_binary(io_lib:format("Validation failed (~s): ~s", [Type, safe_format(Expected)])).

safe_format(undefined) -> <<"">>;
safe_format(V) when is_binary(V) -> V;
safe_format(V) when is_atom(V) -> atom_to_binary(V);
safe_format(V) when is_integer(V) -> integer_to_binary(V);
safe_format(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
safe_format(V) -> iolist_to_binary(io_lib:format("~p", [V])).

safe_value(V) when is_map(V) -> V;
safe_value(V) when is_list(V) -> V;
safe_value(V) when is_binary(V) -> V;
safe_value(V) when is_number(V) -> V;
safe_value(V) when is_boolean(V) -> V;
safe_value(null) -> null;
safe_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).

load_schemas_from_dir(Dir, RelativePrefix) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
                fun(File) ->
                    FilePath = filename:join([Dir, File]),
                    RelativePath = filename:join([RelativePrefix, File]),
                    case filelib:is_dir(FilePath) of
                        true ->
                            load_schemas_from_dir(FilePath, RelativePath);
                        false ->
                            case filename:extension(File) of
                                ".json" ->
                                    load_schema_file(FilePath, RelativePath);
                                _ ->
                                    ok
                            end
                    end
                end,
                Files
            );
        {error, Reason} ->
            ?LOG_ERROR("Failed to list directory ~s: ~p", [Dir, Reason])
    end.

load_schema_file(FilePath, RelativePath) ->
    case file:read_file(FilePath) of
        {ok, FileContent} ->
            try json:decode(FileContent) of
                Schema ->
                    jesse:add_schema(RelativePath, Schema),
                    ?LOG_DEBUG("Loaded JSON schema: ~s", [RelativePath])
            catch
                error:Reason ->
                    ?LOG_ERROR("Failed to decode JSON schema ~s: ~p", [FilePath, Reason])
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to read schema file ~s: ~p", [FilePath, Reason])
    end.

ensure_list(V) when is_list(V) -> V;
ensure_list(V) when is_binary(V) -> binary_to_list(V).
