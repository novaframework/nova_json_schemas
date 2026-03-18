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

init() ->
    jesse_database:load_all(),
    load_local_schemas(),
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Load all local JSON schemas from the main application's
%% priv/schemas directory
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
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
            case maps:get(render_errors, Options, false) of
                true ->
                    ?LOG_DEBUG("Rendering validation-errors and send back to requester"),
                    Req0 = cowboy_req:set_resp_headers(
                        #{<<"content-type">> => <<"application/json">>}, Req
                    ),
                    ErrorStruct = render_error(Errors),
                    ErrorJson = json:encode(ErrorStruct),
                    Req1 = cowboy_req:set_resp_body(ErrorJson, Req0),
                    Req2 = cowboy_req:reply(400, Req1),
                    {stop, Req2, State};
                _ ->
                    ?LOG_DEBUG(
                        "render_errors-option not set for plugin nova_json_schemas - returning plain 400-status to requester"
                    ),
                    Req0 = cowboy_req:reply(400, Req),
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

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
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
        version => <<"0.2.0">>,
        url => <<"https://github.com/novaframework/nova_json_schemas">>,
        authors => [<<"Niclas Axelsson <niclas@burbas.se>">>],
        description => <<"Validates JSON request bodies against JSON schemas using jesse">>,
        options => [
            {render_errors, <<"If true, validation errors are returned as JSON to the requester">>}
        ]
    }.

validate_json(SchemaLocation, Json, JesseOpts) ->
    case jesse:validate(SchemaLocation, Json, JesseOpts) of
        {error, {database_error, _, schema_not_found}} ->
            %% Load the schema
            {ok, MainApp} = nova:get_main_app(),
            PrivDir = code:priv_dir(MainApp),
            SchemaLocation0 = filename:join([PrivDir, SchemaLocation]),
            {ok, Filecontent} = file:read_file(SchemaLocation0),
            Schema = json:decode(Filecontent),
            jesse:add_schema(SchemaLocation, Schema),
            validate_json(SchemaLocation, Json, JesseOpts);
        {error, ValidationError} ->
            {error, ValidationError};
        {ok, _} ->
            ok
    end.

render_error([]) ->
    [];
render_error([{data_invalid, FieldInfo, Type, ActualValue, Field} | Tl]) ->
    [
        #{
            error_context => schema_violation,
            field_info => to_json_safe(FieldInfo),
            error_type => to_json_safe(Type),
            actual_value => to_json_safe(ActualValue),
            expected_value => to_json_safe(Field)
        }
        | render_error(Tl)
    ].

%% @doc Convert arbitrary Erlang terms to JSON-safe values.
%% json:encode/1 cannot encode tuples, pids, refs, etc.
-spec to_json_safe(term()) -> term().
to_json_safe(V) when is_binary(V) -> V;
to_json_safe(V) when is_atom(V) -> V;
to_json_safe(V) when is_number(V) -> V;
to_json_safe(V) when is_list(V) -> [to_json_safe(E) || E <- V];
to_json_safe(V) when is_map(V) ->
    maps:fold(fun(K, Val, Acc) -> Acc#{to_json_safe(K) => to_json_safe(Val)} end, #{}, V);
to_json_safe(V) ->
    iolist_to_binary(io_lib:format("~tp", [V])).

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
