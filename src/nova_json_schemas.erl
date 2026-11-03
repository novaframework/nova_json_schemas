-module(nova_json_schemas).
-moduledoc """
Nova plugin that validates JSON request bodies against JSON schemas with
[jesse](https://github.com/for-GET/jesse).

A route opts in with `extra_state => #{json_schema => "schemas/thing.json"}`.
See the README for the plugin order, the response codes and the error body.
""".
-behaviour(nova_plugin).

-export([
    init/0,
    load_local_schemas/0,
    pre_request/4,
    post_request/4,
    plugin_info/0
]).

-include_lib("kernel/include/logger.hrl").

-define(BODY_METHODS, [~"POST", ~"PUT", ~"PATCH"]).
-define(DEFAULT_MAX_ERRORS, 20).

init() ->
    jesse_database:load_all(),
    load_local_schemas(),
    #{}.

-doc """
Load every `*.json` file under the main application's `priv/schemas`
directory into jesse, keyed by its path relative to `priv`
(for example `"schemas/user.json"`).
""".
-spec load_local_schemas() -> ok.
load_local_schemas() ->
    case main_priv_dir() of
        {ok, PrivDir} ->
            SchemasDir = filename:join(PrivDir, "schemas"),
            case filelib:is_dir(SchemasDir) of
                true ->
                    load_schemas_from_dir(SchemasDir, "schemas");
                false ->
                    ?LOG_WARNING(#{msg => ~"Schemas directory not found", dir => SchemasDir})
            end;
        {error, Reason} ->
            ?LOG_WARNING(#{msg => ~"Schemas directory not found", reason => Reason})
    end,
    ok.

-doc """
Validate the decoded JSON body of a route against the request-body schema
named by `json_schema` in its `extra_state`.

POST, PUT and PATCH requests must carry a decoded JSON body. Requests with
other methods are validated only when a decoded body is present.
""".
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
    {ok, Req0 :: cowboy_req:req(), NewState :: any()}
    | {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(
    #{extra_state := #{json_schema := SchemaLocation} = Extra, json := JSON} = Req,
    _Env,
    Options,
    State
) ->
    validate_request(schema_key(SchemaLocation), JSON, Extra, Options, Req, State);
pre_request(
    #{extra_state := #{json_schema := SchemaLocation}, method := Method} = Req,
    _Env,
    _Options,
    State
) ->
    case lists:member(Method, ?BODY_METHODS) of
        true ->
            reject_undecoded_body(schema_key(SchemaLocation), Req, State);
        false ->
            {ok, Req, State}
    end;
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
        title => ~"Nova JSON Schema plugin",
        version => version(),
        url => ~"https://github.com/novaframework/nova_json_schemas",
        authors => [~"Niclas Axelsson <niclas@burbas.se>"],
        description => ~"Validates JSON request bodies against JSON schemas using jesse",
        options => [
            {render_errors, ~"If true, validation errors are returned as JSON to the requester"},
            {max_errors, ~"Most errors reported in the error body (default 20)"}
        ]
    }.

version() ->
    case application:get_key(nova_json_schemas, vsn) of
        {ok, Vsn} -> iolist_to_binary(io_lib:format("~ts", [Vsn]));
        _ -> ~"unknown"
    end.

schema_key(SchemaLocation) when is_binary(SchemaLocation) ->
    binary_to_list(SchemaLocation);
schema_key(SchemaLocation) ->
    SchemaLocation.

validate_request(SchemaKey, JSON, Extra, Options, Req, State) ->
    RouteOpts = maps:get(jesse_options, Extra, []),
    RenderErrors = maps:get(render_errors, Options, false) =:= true,
    MaxErrors =
        case proplists:get_value(allowed_errors, RouteOpts) of
            infinity -> infinity;
            _ -> max_errors(Options)
        end,
    AllowedErrors =
        case RenderErrors of
            true -> MaxErrors;
            false -> 0
        end,
    case validate_json(SchemaKey, JSON, RouteOpts ++ [{allowed_errors, AllowedErrors}]) of
        ok ->
            {ok, Req, State};
        {invalid, Errors} ->
            ?LOG_DEBUG(#{msg => ~"JSON body failed schema validation", schema => SchemaKey}),
            reply_invalid(Errors, RenderErrors, MaxErrors, Req, State);
        {schema_error, Reason} ->
            ?LOG_ERROR(#{
                msg => ~"JSON schema cannot be used for validation",
                schema => SchemaKey,
                reason => Reason
            }),
            {stop, cowboy_req:reply(500, Req), State}
    end.

max_errors(Options) ->
    case maps:get(max_errors, Options, ?DEFAULT_MAX_ERRORS) of
        MaxErrors when is_integer(MaxErrors), MaxErrors > 0 -> MaxErrors;
        Other -> error({invalid_option, max_errors, Other})
    end.

reply_invalid(Errors, true, MaxErrors, Req, State) ->
    {Reported, Truncated} = truncate(Errors, MaxErrors),
    Body = json:encode(#{
        errors => [render_error(Error) || Error <- Reported],
        truncated => Truncated
    }),
    Req0 = cowboy_req:reply(400, #{~"content-type" => ~"application/json"}, Body, Req),
    {stop, Req0, State};
reply_invalid(_Errors, false, _MaxErrors, Req, State) ->
    {stop, cowboy_req:reply(400, Req), State}.

truncate(Errors, MaxErrors) when is_integer(MaxErrors), length(Errors) > MaxErrors ->
    {lists:sublist(Errors, MaxErrors), true};
truncate(Errors, _MaxErrors) ->
    {Errors, false}.

reject_undecoded_body(SchemaKey, Req, State) ->
    case {cowboy_req:has_body(Req), is_decoded_content_type(Req)} of
        {false, _} ->
            {stop, cowboy_req:reply(400, Req), State};
        {true, false} ->
            Req0 = cowboy_req:reply(415, unsupported_media_type_headers(Req), Req),
            {stop, Req0, State};
        {true, true} ->
            ?LOG_ERROR(#{
                msg =>
                    ~"JSON body was not decoded before nova_json_schemas ran. Put {pre_request, nova_request_plugin, #{decode_json_body => true}} before nova_json_schemas in the plugin list",
                schema => SchemaKey
            }),
            {stop, cowboy_req:reply(500, Req), State}
    end.

%% Same rule as nova_request_plugin's decode_json_body clause, so a type it
%% does not decode is a 415 here and not a misdiagnosed plugin order.
is_decoded_content_type(#{headers := #{~"content-type" := <<"application/json", _/binary>>}}) ->
    true;
is_decoded_content_type(_Req) ->
    false.

unsupported_media_type_headers(#{method := ~"PATCH"}) ->
    #{~"accept" => ~"application/json", ~"accept-patch" => ~"application/json"};
unsupported_media_type_headers(_Req) ->
    #{~"accept" => ~"application/json"}.

validate_json(SchemaLocation, JSON, JesseOpts) ->
    case jesse:validate(SchemaLocation, JSON, JesseOpts) of
        {error, {database_error, _, schema_not_found}} ->
            case load_schema_on_demand(SchemaLocation) of
                ok ->
                    classify(jesse:validate(SchemaLocation, JSON, JesseOpts));
                {error, Reason} ->
                    {schema_error, Reason}
            end;
        Result ->
            classify(Result)
    end.

classify({ok, _}) ->
    ok;
classify({error, Errors}) when is_list(Errors) ->
    case lists:partition(fun is_data_invalid/1, Errors) of
        {DataErrors, []} ->
            {invalid, DataErrors};
        {_, SchemaErrors} ->
            {schema_error, [strip_schema(Error) || Error <- SchemaErrors]}
    end;
classify({error, Reason}) ->
    {schema_error, Reason}.

is_data_invalid({data_invalid, _Schema, _Info, _Value, _Path}) -> true;
is_data_invalid(_) -> false.

strip_schema({schema_invalid, _Schema, Info}) -> {schema_invalid, Info};
strip_schema(Error) -> Error.

load_schema_on_demand(SchemaLocation) ->
    maybe
        {ok, PrivDir} ?= main_priv_dir(),
        FilePath = filename:join(PrivDir, SchemaLocation),
        {ok, FileContent} ?=
            case file:read_file(FilePath) of
                {error, Reason} -> {error, {schema_file, FilePath, Reason}};
                Read -> Read
            end,
        add_schema(SchemaLocation, FileContent)
    end.

main_priv_dir() ->
    {ok, MainApp} = nova:get_main_app(),
    case code:priv_dir(MainApp) of
        {error, bad_name} -> {error, {no_priv_dir, MainApp}};
        PrivDir -> {ok, PrivDir}
    end.

add_schema(Key, FileContent) ->
    try json:decode(FileContent) of
        Schema when is_map(Schema) ->
            warn_if_unsupported_draft(Key, Schema),
            jesse:add_schema(Key, Schema);
        _ ->
            {error, schema_not_an_object}
    catch
        error:Reason ->
            {error, {json_decode, Reason}}
    end.

warn_if_unsupported_draft(Key, Schema) ->
    case jesse:validate_with_schema(maps:with([~"$schema"], Schema), null) of
        {error, [{schema_invalid, _, {schema_unsupported, Draft}}]} ->
            ?LOG_WARNING(#{
                msg =>
                    ~"JSON schema declares a $schema that jesse cannot validate; requests on routes using it get a 500",
                schema => Key,
                '$schema' => Draft
            });
        _ ->
            ok
    end.

render_error({data_invalid, _Schema, Info, Value, Path}) ->
    maps:merge(
        #{
            error_context => schema_violation,
            error_type => error_type(Info),
            pointer => json_pointer(Path)
        },
        error_detail(Info, Value)
    ).

error_type(Type) when is_atom(Type) -> Type;
error_type({Type, _}) when is_atom(Type) -> Type;
error_type(_) -> data_invalid.

error_detail(missing_required_property, Name) when is_binary(Name) -> #{property => Name};
error_detail({missing_dependency, Name}, _Value) when is_binary(Name) -> #{property => Name};
error_detail(_Info, _Value) -> #{}.

json_pointer(Path) ->
    iolist_to_binary([[$/, pointer_segment(Segment)] || Segment <- Path]).

pointer_segment(Index) when is_integer(Index) ->
    integer_to_binary(Index);
pointer_segment(Segment) when is_atom(Segment) ->
    pointer_segment(atom_to_binary(Segment));
pointer_segment(Segment) when is_list(Segment) ->
    pointer_segment(unicode:characters_to_binary(Segment));
pointer_segment(Segment) when is_binary(Segment) ->
    Escaped = binary:replace(Segment, ~"~", ~"~0", [global]),
    binary:replace(Escaped, ~"/", ~"~1", [global]).

load_schemas_from_dir(Dir, RelativePrefix) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            _ = [load_dir_entry(Dir, RelativePrefix, File) || File <- Files],
            ok;
        {error, Reason} ->
            ?LOG_ERROR(#{msg => ~"Failed to list schema directory", dir => Dir, reason => Reason})
    end.

load_dir_entry(Dir, RelativePrefix, File) ->
    FilePath = filename:join(Dir, File),
    RelativePath = filename:join(RelativePrefix, File),
    case {filelib:is_dir(FilePath), filename:extension(File)} of
        {true, _} -> load_schemas_from_dir(FilePath, RelativePath);
        {false, ".json"} -> load_schema_file(FilePath, RelativePath);
        {false, _} -> ok
    end.

load_schema_file(FilePath, RelativePath) ->
    case file:read_file(FilePath) of
        {ok, FileContent} ->
            case add_schema(RelativePath, FileContent) of
                ok ->
                    ?LOG_DEBUG(#{msg => ~"Loaded JSON schema", schema => RelativePath});
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        msg => ~"Failed to load JSON schema", file => FilePath, reason => Reason
                    })
            end;
        {error, Reason} ->
            ?LOG_ERROR(#{msg => ~"Failed to read schema file", file => FilePath, reason => Reason})
    end.
