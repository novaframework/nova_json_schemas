-module(nova_json_schemas).
-behaviour(nova_plugin).

-export([
    load_local_schemas/0,
    pre_request/2,
    post_request/2,
    plugin_info/0
]).

-include_lib("kernel/include/logger.hrl").

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
-spec pre_request(Req :: cowboy_req:req(), Options :: map()) ->
    {ok, Req0 :: cowboy_req:req()}
    | {stop, Req0 :: cowboy_req:req()}
    | {error, Reason :: term()}.
pre_request(
    Req = #{extra_state := #{json_schema := SchemaLocation} = Extra, json := JSON}, Options
) ->
    JesseOpts = maps:get(jesse_options, Extra, []),
    %% JSON have already been parsed so we can just continue with the validation
    case validate_json(SchemaLocation, JSON, JesseOpts) of
        ok ->
            ?LOG_DEBUG("Schema validation on JSON body successful"),
            {ok, Req};
        {error, Errors} ->
            ?LOG_DEBUG("Got validation-errors on JSON body. Errors: ~p", [Errors]),
            case maps:get(render_errors, Options, false) of
                true ->
                    ?LOG_DEBUG("Rendering validation-errors and send back to requester"),
                    JsonLib = nova:get_env(json_lib, thoas),
                    Req0 = cowboy_req:set_resp_headers(
                        #{<<"content-type">> => <<"application/json">>}, Req
                    ),
                    ErrorStruct = render_error(Errors),
                    ErrorJson = erlang:apply(JsonLib, encode, [ErrorStruct]),
                    Req1 = cowboy_req:set_resp_body(ErrorJson, Req0),
                    Req2 = cowboy_req:reply(400, Req1),
                    {stop, Req2};
                _ ->
                    ?LOG_DEBUG(
                        "render_errors-option not set for plugin nova_json_schemas - returning plain 400-status to requester"
                    ),
                    Req0 = cowboy_req:reply(400, Req),
                    {stop, Req0}
            end
    end;
pre_request(#{extra_state := #{json_schema := _SchemaLocation}}, _Options) ->
    %% The body have not been parsed. Log and error and stop
    ?LOG_ERROR(
        "JSON Schema is set in 'extra_state' but body have not yet been parsed - rearrange your plugins so that JSON plugin is ran before this.."
    ),
    {error, body_not_parsed};
pre_request(Req, _Options) ->
    %% 'json_schema' is not set or 'extra_state' is completly missing. Just continue.
    HasBody = cowboy_req:has_body(Req),
    if
        HasBody ->
            ?LOG_DEBUG("No schema is set for this route so will continue executing");
        true ->
            ok
    end,
    {ok, Req}.

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Options :: map()) ->
    {ok, Req0 :: cowboy_req:req()}
    | {stop, Req0 :: cowboy_req:req()}
    | {error, Reason :: term()}.
post_request(Req, _Options) ->
    {ok, Req}.

%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() ->
    {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary(), [
        {Key :: atom(), OptionDescription :: binary()}
    ]}.
plugin_info() ->
    {ok, Vsn} = application:get_key(nova_json_schemas, vsn),
    {ok, Desc} = application:get_key(nova_json_schemas, description),

    {<<"JSON schema plugin">>, list_to_binary(Vsn), <<"Niclas Axelsson <niclas@burbas.se">>,
        list_to_binary(Desc), [
            {render_errors, <<"If this is set, validation-errors is returned to the requester">>}
            %% Options is specified as {Key, Description}
        ]}.

validate_json(SchemaLocation, Json, JesseOpts) ->
    case jesse:validate(SchemaLocation, Json, JesseOpts) of
        {error, {database_error, _, schema_not_found}} ->
            %% Load the schema
            {ok, MainApp} = nova:get_main_app(),
            PrivDir = code:priv_dir(MainApp),
            SchemaLocation0 = filename:join([PrivDir, SchemaLocation]),
            {ok, Filecontent} = file:read_file(SchemaLocation0),
            JsonLib = nova:get_env(json_lib, thoas),
            {ok, Schema} = erlang:apply(JsonLib, decode, [Filecontent]),
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
    %% We don't do any fancy with this currently.
    [
        #{
            error_context => schema_violation,
            field_info => FieldInfo,
            error_type => Type,
            actual_value => ActualValue,
            expected_value => Field
        }
        | render_error(Tl)
    ].

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
            JsonLib = nova:get_env(json_lib, thoas),
            case erlang:apply(JsonLib, decode, [FileContent]) of
                {ok, Schema} ->
                    jesse:add_schema(RelativePath, Schema),
                    ?LOG_DEBUG("Loaded JSON schema: ~s", [RelativePath]);
                {error, Reason} ->
                    ?LOG_ERROR("Failed to decode JSON schema ~s: ~p", [FilePath, Reason])
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to read schema file ~s: ~p", [FilePath, Reason])
    end.
