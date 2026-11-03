-module(nova_json_schemas_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2]).
-export([log/2]).
-export([
    valid_body_passes/1,
    render_errors_lists_every_error/1,
    render_errors_hides_schema_and_paths/1,
    render_errors_uses_json_pointer_paths/1,
    jesse_allowed_errors_can_be_overridden/1,
    invalid_body_without_render_errors_is_bare_400/1,
    put_and_patch_are_validated/1,
    post_without_body_is_400/1,
    form_body_is_415/1,
    text_body_is_415/1,
    undecoded_json_body_is_500_with_hint/1,
    get_and_delete_with_schema_are_not_validated/1,
    get_with_json_body_is_not_rejected/1,
    unsupported_draft_warns_at_load/1,
    unsupported_draft_is_500/1,
    schema_loaded_on_demand/1,
    missing_schema_file_is_500/1,
    route_without_schema_is_untouched/1
]).

-define(PORT, 58608).
-define(LOG_TABLE, nova_json_schemas_SUITE_logs).

all() ->
    [
        valid_body_passes,
        render_errors_lists_every_error,
        render_errors_hides_schema_and_paths,
        render_errors_uses_json_pointer_paths,
        jesse_allowed_errors_can_be_overridden,
        invalid_body_without_render_errors_is_bare_400,
        put_and_patch_are_validated,
        post_without_body_is_400,
        form_body_is_415,
        text_body_is_415,
        undecoded_json_body_is_500_with_hint,
        get_and_delete_with_schema_are_not_validated,
        get_with_json_body_is_not_rejected,
        unsupported_draft_warns_at_load,
        unsupported_draft_is_500,
        schema_loaded_on_demand,
        missing_schema_file_is_500,
        route_without_schema_is_untouched
    ].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    AppDir = filename:join(PrivDir, "schema_app"),
    SchemasDir = filename:join([AppDir, "priv", "schemas"]),
    ok = filelib:ensure_path(filename:join(AppDir, "ebin")),
    ok = filelib:ensure_path(SchemasDir),
    ok = file:write_file(
        filename:join([AppDir, "ebin", "schema_app.app"]),
        ~"{application, schema_app, [{vsn, \"1\"}, {modules, []}, {applications, [kernel, stdlib]}]}.\n"
    ),
    write_schema(filename:join(SchemasDir, "tag.json"), tag_schema()),
    write_schema(filename:join(SchemasDir, "draft07.json"), #{
        ~"$schema" => ~"http://json-schema.org/draft-07/schema#", ~"type" => ~"object"
    }),
    EbinDir = [_ | _] = filename:join(AppDir, "ebin"),
    true = code:add_patha(EbinDir),
    ok = application:load(schema_app),

    Owner = spawn(fun() -> receive stop -> ok end end),
    ?LOG_TABLE = ets:new(?LOG_TABLE, [named_table, public, bag, {heir, Owner, none}]),
    ok = logger:add_handler(?MODULE, ?MODULE, #{level => all}),

    ok = application:set_env(nova, bootstrap_application, schema_app),
    ok = application:set_env(nova, environment, test),
    ok = application:set_env(nova, cowboy_configuration, #{port => ?PORT}),
    {ok, _} = application:ensure_all_started(nova_json_schemas),
    {ok, _} = application:ensure_all_started(nova),
    _ = sys:get_state(nova_plugin_manager),

    ok = filelib:ensure_path(filename:join(SchemasDir, "nested")),
    write_schema(filename:join([SchemasDir, "nested", "lazy.json"]), #{
        ~"$schema" => ~"http://json-schema.org/draft-04/schema#",
        ~"type" => ~"object",
        ~"required" => [~"id"]
    }),
    [{log_owner, Owner}, {load_warnings, logged(warning, ~"jesse cannot validate")} | Config].

end_per_suite(Config) ->
    application:stop(nova),
    logger:remove_handler(?MODULE),
    ?config(log_owner, Config) ! stop,
    ok.

init_per_testcase(_Case, Config) ->
    Config.

tag_schema() ->
    #{
        ~"$schema" => ~"http://json-schema.org/draft-06/schema#",
        ~"$id" => ~"tag",
        ~"type" => ~"object",
        ~"required" => [~"name", ~"slug"],
        ~"additionalProperties" => false,
        ~"properties" => #{
            ~"name" => #{~"type" => ~"string", ~"maxLength" => 10},
            ~"slug" => #{~"type" => ~"string"},
            ~"tags" => #{~"type" => ~"array", ~"items" => #{~"type" => ~"string"}},
            ~"a/b~c" => #{~"type" => ~"integer"}
        }
    }.

valid_body_passes(_Config) ->
    {200, _, _} = post_json("/tags", #{~"name" => ~"erlang", ~"slug" => ~"erlang"}).

render_errors_lists_every_error(_Config) ->
    {400, Headers, Body} = post_json("/tags", #{~"name" => 5, ~"extra" => true}),
    ?assertMatch({0, _}, binary:match(header(~"content-type", Headers), ~"application/json")),
    Errors = decode_errors(Body),
    ?assertEqual(
        lists:sort([
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"missing_required_property",
                ~"path" => ~"",
                ~"property" => ~"slug"
            },
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"wrong_type",
                ~"path" => ~"/name"
            },
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"no_extra_properties_allowed",
                ~"path" => ~"/extra"
            }
        ]),
        lists:sort(Errors)
    ).

render_errors_hides_schema_and_paths(_Config) ->
    {400, _, Body} = post_json("/tags", #{}),
    ?assertEqual(nomatch, binary:match(Body, [~"file:", ~"$id", ~"$schema", ~"field_info"])),
    ?assertEqual(nomatch, binary:match(Body, ~"properties")),
    [
        ?assertEqual([], maps:keys(Error) -- [~"error_context", ~"error_type", ~"path", ~"property"])
     || Error <- decode_errors(Body)
    ].

render_errors_uses_json_pointer_paths(_Config) ->
    {400, _, Body} = post_json("/tags", #{
        ~"name" => ~"erlang",
        ~"slug" => ~"erlang",
        ~"tags" => [~"ok", 1],
        ~"a/b~c" => ~"x"
    }),
    Paths = lists:sort([maps:get(~"path", Error) || Error <- decode_errors(Body)]),
    ?assertEqual([~"/a~1b~0c", ~"/tags/1"], Paths).

jesse_allowed_errors_can_be_overridden(_Config) ->
    {400, _, Body} = post_json("/first_error", #{~"name" => 5}),
    ?assertEqual(1, length(decode_errors(Body))).

invalid_body_without_render_errors_is_bare_400(_Config) ->
    {400, _, <<>>} = post_json("/plain/tags", #{~"name" => 5}).

put_and_patch_are_validated(_Config) ->
    {400, _, _} = request(put, "/tags", [{"content-type", "application/json"}], ~"{}"),
    {400, _, _} = request(patch, "/tags", [{"content-type", "application/json"}], ~"{}").

post_without_body_is_400(_Config) ->
    {400, _, _} = request(post, "/tags", [], <<>>).

form_body_is_415(_Config) ->
    {415, Headers, _} = request(
        post, "/tags", [{"content-type", "application/x-www-form-urlencoded"}], ~"name=x"
    ),
    ?assertEqual(~"application/json", header(~"accept", Headers)).

text_body_is_415(_Config) ->
    {415, _, _} = request(post, "/tags", [{"content-type", "text/plain"}], ~"{}").

undecoded_json_body_is_500_with_hint(_Config) ->
    ets:delete_all_objects(?LOG_TABLE),
    {500, _, _} = request(post, "/unordered/tags", [{"content-type", "application/json"}], ~"{}"),
    ?assertMatch([_ | _], logged(error, ~"JSON body was not decoded")),
    ?assertEqual([], logged(error, ~"Plugin crashed")).

get_and_delete_with_schema_are_not_validated(_Config) ->
    {200, _, _} = request(get, "/tags", [], <<>>),
    {200, _, _} = request(delete, "/tags", [], <<>>).

get_with_json_body_is_not_rejected(_Config) ->
    {200, _, _} = request(get, "/tags", [{"content-type", "application/json"}], ~"{}").

unsupported_draft_warns_at_load(Config) ->
    [Msg] = ?config(load_warnings, Config),
    ?assertMatch(#{schema := "schemas/draft07.json"}, Msg).

unsupported_draft_is_500(_Config) ->
    ets:delete_all_objects(?LOG_TABLE),
    {500, _, <<>>} = post_json("/draft07", #{}),
    [#{reason := [{schema_invalid, {schema_unsupported, _}}]}] =
        logged(error, ~"JSON schema cannot be used").

schema_loaded_on_demand(_Config) ->
    {400, _, Body} = post_json("/lazy", #{}),
    ?assertMatch([#{~"property" := ~"id"}], decode_errors(Body)),
    {200, _, _} = post_json("/lazy", #{~"id" => 1}).

missing_schema_file_is_500(_Config) ->
    ets:delete_all_objects(?LOG_TABLE),
    {500, _, _} = post_json("/missing", #{}),
    ?assertMatch([_], logged(error, ~"JSON schema cannot be used")),
    ?assertEqual([], logged(error, ~"Plugin crashed")).

route_without_schema_is_untouched(_Config) ->
    {200, _, _} = request(post, "/no_schema", [{"content-type", "text/plain"}], ~"hi").

log(#{level := Level, msg := {report, #{msg := Msg} = Report}}, _Config) when is_binary(Msg) ->
    ets:insert(?LOG_TABLE, {Level, Msg, Report});
log(_Event, _Config) ->
    ok.

logged(Level, Fragment) ->
    [
        Report
     || {_, Msg, Report} <- ets:lookup(?LOG_TABLE, Level),
        is_binary(Msg),
        binary:match(Msg, Fragment) =/= nomatch
    ].

write_schema(Path, Schema) ->
    ok = file:write_file(Path, json:encode(Schema)).

post_json(Path, Body) ->
    request(post, Path, [{"content-type", "application/json"}], iolist_to_binary(json:encode(Body))).

request(Method, Path, Headers, Body) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", ?PORT, [binary, {active, false}]),
    HeaderLines = [[Name, ": ", Value, "\r\n"] || {Name, Value} <- Headers],
    Request = [
        string:uppercase(atom_to_list(Method)),
        " ",
        Path,
        " HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n",
        HeaderLines,
        "content-length: ",
        integer_to_list(byte_size(Body)),
        "\r\n\r\n",
        Body
    ],
    ok = gen_tcp:send(Socket, Request),
    Response = recv_all(Socket, <<>>),
    [Head, RespBody] = binary:split(Response, ~"\r\n\r\n"),
    [StatusLine | RespHeaderLines] = binary:split(Head, ~"\r\n", [global]),
    [_, Status | _] = binary:split(StatusLine, ~" ", [global]),
    RespHeaders = [
        begin
            [Name, Value] = binary:split(Line, ~": "),
            {string:lowercase(Name), Value}
        end
     || Line <- RespHeaderLines
    ],
    {binary_to_integer(Status), RespHeaders, RespBody}.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} when is_binary(Data) -> recv_all(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.

decode_errors(Body) ->
    Errors = json:decode(Body),
    true = is_list(Errors),
    Errors.

header(Name, Headers) ->
    proplists:get_value(Name, Headers).
