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
    route_without_schema_is_untouched/1,
    errors_stop_at_the_first_by_default/1,
    max_errors_option_sets_the_cap/1,
    route_allowed_errors_cannot_raise_the_cap/1,
    route_allowed_errors_can_lower_the_cap/1,
    route_allowed_errors_is_ignored_without_render_errors/1,
    mixed_case_json_type_is_415/1,
    patch_415_sends_accept_patch/1,
    binary_schema_key_is_validated/1,
    schema_without_draft_does_not_warn/1,
    unique_items_on_a_huge_array_fails_fast/1,
    refs_between_loaded_schemas_resolve/1,
    refs_are_not_fetched_over_the_network/1,
    long_keys_give_a_bounded_pointer/1,
    huge_integer_is_400_under_draft6/1,
    huge_integer_is_400_under_draft4/1,
    other_methods_with_undecoded_json_body_are_500/1,
    only_exact_json_type_counts_as_undecoded/1,
    schema_path_outside_priv_is_500/1
]).

-define(PORT, 58625).
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
        route_without_schema_is_untouched,
        errors_stop_at_the_first_by_default,
        max_errors_option_sets_the_cap,
        route_allowed_errors_cannot_raise_the_cap,
        route_allowed_errors_can_lower_the_cap,
        route_allowed_errors_is_ignored_without_render_errors,
        mixed_case_json_type_is_415,
        patch_415_sends_accept_patch,
        binary_schema_key_is_validated,
        schema_without_draft_does_not_warn,
        unique_items_on_a_huge_array_fails_fast,
        refs_between_loaded_schemas_resolve,
        refs_are_not_fetched_over_the_network,
        long_keys_give_a_bounded_pointer,
        huge_integer_is_400_under_draft6,
        huge_integer_is_400_under_draft4,
        other_methods_with_undecoded_json_body_are_500,
        only_exact_json_type_counts_as_undecoded,
        schema_path_outside_priv_is_500
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
    write_schema(filename:join(SchemasDir, "strings.json"), #{
        ~"type" => ~"array", ~"items" => #{~"type" => ~"string"}
    }),
    write_schema(filename:join(SchemasDir, "draft07.json"), #{
        ~"$schema" => ~"http://json-schema.org/draft-07/schema#", ~"type" => ~"object"
    }),
    write_schema(filename:join(SchemasDir, "unique.json"), #{
        ~"type" => ~"object",
        ~"properties" => #{
            ~"ids" => #{~"type" => ~"array", ~"maxItems" => 10, ~"uniqueItems" => true}
        }
    }),
    write_schema(filename:join(SchemasDir, "keyed.json"), #{
        ~"type" => ~"object",
        ~"additionalProperties" => #{~"type" => ~"array", ~"items" => #{~"type" => ~"string"}}
    }),
    write_schema(filename:join(SchemasDir, "multiple_of_6.json"), multiple_of_schema(6)),
    write_schema(filename:join(SchemasDir, "multiple_of_4.json"), multiple_of_schema(4)),
    write_schema(filename:join(SchemasDir, "broken_multiple_of.json"), #{
        ~"$schema" => ~"http://json-schema.org/draft-06/schema#",
        ~"properties" => #{~"n" => #{~"multipleOf" => -1}}
    }),
    ok = filelib:ensure_path(filename:join(SchemasDir, "refs")),
    write_schema(filename:join([SchemasDir, "refs", "item.json"]), #{~"type" => ~"string"}),
    write_schema(filename:join([SchemasDir, "refs", "relative.json"]), #{
        ~"type" => ~"object", ~"properties" => #{~"x" => #{~"$ref" => ~"item.json"}}
    }),
    write_schema(filename:join([SchemasDir, "refs", "id_target.json"]), #{
        ~"$id" => ~"https://schemas.example.invalid/id_target.json", ~"type" => ~"string"
    }),
    write_schema(filename:join([SchemasDir, "refs", "by_id.json"]), #{
        ~"$id" => ~"https://schemas.example.invalid/by_id.json",
        ~"type" => ~"object",
        ~"properties" => #{~"x" => #{~"$ref" => ~"id_target.json"}}
    }),
    OutsideSchema = filename:join(AppDir, "outside.json"),
    write_schema(OutsideSchema, #{~"type" => ~"object"}),
    EbinDir = [_ | _] = filename:join(AppDir, "ebin"),
    true = code:add_patha(EbinDir),
    ok = application:load(schema_app),
    ok = application:set_env(schema_app, outside_schema, OutsideSchema),

    Owner = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
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
    [
        {log_owner, Owner},
        {app_dir, AppDir},
        {load_warnings, logged(warning, ~"jesse cannot validate")}
        | Config
    ].

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

multiple_of_schema(Draft) ->
    #{
        ~"$schema" => iolist_to_binary(
            io_lib:format("http://json-schema.org/draft-0~b/schema#", [Draft])
        ),
        ~"type" => ~"object",
        ~"properties" => #{~"n" => #{~"multipleOf" => 2}}
    }.

valid_body_passes(_Config) ->
    {200, _, _} = post_json("/tags", #{~"name" => ~"erlang", ~"slug" => ~"erlang"}).

render_errors_lists_every_error(_Config) ->
    {400, Headers, Body} = post_json("/collect/tags", #{~"name" => 5, ~"extra" => true}),
    ?assertMatch({0, _}, binary:match(header(~"content-type", Headers), ~"application/json")),
    #{~"truncated" := false} = json:decode(Body),
    Errors = decode_errors(Body),
    ?assertEqual(
        lists:sort([
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"missing_required_property",
                ~"pointer" => ~"",
                ~"property" => ~"slug"
            },
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"wrong_type",
                ~"pointer" => ~"/name"
            },
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"no_extra_properties_allowed",
                ~"pointer" => ~"/extra"
            }
        ]),
        lists:sort(Errors)
    ).

render_errors_hides_schema_and_paths(_Config) ->
    {400, _, Body} = post_json("/tags", #{}),
    ?assertEqual(nomatch, binary:match(Body, [~"file:", ~"$id", ~"$schema", ~"field_info"])),
    ?assertEqual(nomatch, binary:match(Body, ~"properties")),
    [
        ?assertEqual(
            [], maps:keys(Error) -- [~"error_context", ~"error_type", ~"pointer", ~"property"]
        )
     || Error <- decode_errors(Body)
    ].

render_errors_uses_json_pointer_paths(_Config) ->
    {400, _, Body} = post_json("/collect/tags", #{
        ~"name" => ~"erlang",
        ~"slug" => ~"erlang",
        ~"tags" => [~"ok", 1],
        ~"a/b~c" => ~"x"
    }),
    Paths = lists:sort([maps:get(~"pointer", Error) || Error <- decode_errors(Body)]),
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

errors_stop_at_the_first_by_default(_Config) ->
    {400, _, Body} = post_json("/array", lists:duplicate(10000, 1)),
    ?assertMatch(#{~"truncated" := true}, json:decode(Body)),
    ?assertEqual(1, length(decode_errors(Body))).

max_errors_option_sets_the_cap(_Config) ->
    {400, _, Body} = post_json("/capped/array", lists:duplicate(10, 1)),
    ?assertMatch(#{~"truncated" := true}, json:decode(Body)),
    ?assertEqual(3, length(decode_errors(Body))),
    {400, _, AtCap} = post_json("/capped/array", lists:duplicate(3, 1)),
    ?assertMatch(#{~"truncated" := true}, json:decode(AtCap)),
    ?assertEqual(3, length(decode_errors(AtCap))),
    {400, _, BelowCap} = post_json("/capped/array", lists:duplicate(2, 1)),
    ?assertMatch(#{~"truncated" := false}, json:decode(BelowCap)),
    ?assertEqual(2, length(decode_errors(BelowCap))).

route_allowed_errors_cannot_raise_the_cap(_Config) ->
    {400, _, Body} = post_json("/all_errors", lists:duplicate(30, 1)),
    ?assertMatch(#{~"truncated" := true}, json:decode(Body)),
    ?assertEqual(1, length(decode_errors(Body))),
    {400, _, Capped} = post_json("/capped/all_errors", lists:duplicate(30, 1)),
    ?assertEqual(3, length(decode_errors(Capped))),
    {400, _, Many} = post_json("/many_errors", lists:duplicate(30, 1)),
    ?assertEqual(1, length(decode_errors(Many))),
    {400, _, CappedMany} = post_json("/capped/many_errors", lists:duplicate(30, 1)),
    ?assertEqual(3, length(decode_errors(CappedMany))),
    {400, _, Negative} = post_json("/negative_errors", lists:duplicate(30, 1)),
    ?assertEqual(1, length(decode_errors(Negative))).

route_allowed_errors_can_lower_the_cap(_Config) ->
    {400, _, Body} = post_json("/capped/first_error", lists:duplicate(10, 1)),
    ?assertMatch(#{~"truncated" := true}, json:decode(Body)),
    ?assertEqual(1, length(decode_errors(Body))).

route_allowed_errors_is_ignored_without_render_errors(_Config) ->
    {Millis, {400, _, <<>>}} = timed_post("/plain/unique_all_errors", huge_duplicate_ids()),
    ?assert(Millis < 2000, Millis).

mixed_case_json_type_is_415(_Config) ->
    ets:delete_all_objects(?LOG_TABLE),
    {Status, _, _} = request(post, "/tags", [{"content-type", "Application/JSON"}], ~"{}"),
    ?assert(lists:member(Status, [415, 400]), Status),
    ?assertEqual([], logged(error, ~"JSON body was not decoded")).

patch_415_sends_accept_patch(_Config) ->
    {415, Headers, _} = request(patch, "/tags", [{"content-type", "text/plain"}], ~"{}"),
    ?assertEqual(~"application/json", header(~"accept-patch", Headers)).

binary_schema_key_is_validated(_Config) ->
    {400, _, Body} = post_json("/binary_key", #{~"name" => ~"erlang"}),
    ?assertMatch([#{~"property" := ~"slug"}], decode_errors(Body)),
    {200, _, _} = post_json("/binary_key", #{~"name" => ~"erlang", ~"slug" => ~"erlang"}).

schema_without_draft_does_not_warn(Config) ->
    ?assertEqual([], [
        W
     || #{schema := "schemas/strings.json"} = W <- ?config(load_warnings, Config)
    ]).

unique_items_on_a_huge_array_fails_fast(_Config) ->
    {Millis, {400, _, Body}} = timed_post("/unique", huge_duplicate_ids()),
    ?assert(Millis < 2000, Millis),
    ?assertMatch(
        [#{~"error_type" := ~"wrong_size", ~"pointer" := ~"/ids"}], decode_errors(Body)
    ).

refs_between_loaded_schemas_resolve(_Config) ->
    {200, _, _} = post_json("/refs/relative", #{~"x" => ~"ok"}),
    {400, _, Relative} = post_json("/refs/relative", #{~"x" => 1}),
    ?assertMatch([#{~"error_type" := ~"wrong_type", ~"pointer" := ~"/x"}], decode_errors(Relative)),
    {200, _, _} = post_json("/refs/by_id", #{~"x" => ~"ok"}),
    {400, _, ById} = post_json("/refs/by_id", #{~"x" => 1}),
    ?assertMatch([#{~"error_type" := ~"wrong_type", ~"pointer" := ~"/x"}], decode_errors(ById)).

refs_are_not_fetched_over_the_network(Config) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    Self = self(),
    Acceptor = spawn_link(fun() -> record_fetches(Listen, Self) end),
    RemoteDir = filename:join([?config(app_dir, Config), "priv", "remote"]),
    ok = filelib:ensure_path(RemoteDir),
    write_schema(filename:join(RemoteDir, "root.json"), #{
        ~"$id" => iolist_to_binary(io_lib:format("http://127.0.0.1:~b/base/root.json", [Port])),
        ~"type" => ~"object",
        ~"properties" => #{~"a" => #{~"$ref" => ~"other.json"}}
    }),
    ets:delete_all_objects(?LOG_TABLE),
    {Status, _, _} = post_json("/remote", #{~"a" => 1}),
    Fetches =
        receive
            {fetch, Request} -> [Request]
        after 500 -> []
        end,
    unlink(Acceptor),
    exit(Acceptor, kill),
    ok = gen_tcp:close(Listen),
    ?assertEqual([], Fetches),
    ?assertEqual(500, Status),
    ?assertMatch([_], logged(error, ~"JSON schema cannot be used")).

long_keys_give_a_bounded_pointer(_Config) ->
    Key = binary:copy(~"~", 100000),
    {400, _, Body} = post_json("/keyed", #{Key => [1]}),
    ?assert(byte_size(Body) < 1024, byte_size(Body)),
    ?assertMatch([#{~"pointer" := ~""}], decode_errors(Body)),
    Segment = binary:copy(~"k", 254),
    {400, _, Ancestor} = post_json("/keyed", #{Segment => [1]}),
    ?assertMatch([#{~"pointer" := <<"/", Segment:254/binary>>}], decode_errors(Ancestor)),
    Short = binary:copy(~"k", 253),
    {400, _, Full} = post_json("/keyed", #{Short => [1]}),
    ?assertMatch([#{~"pointer" := <<"/", Short:253/binary, "/0">>}], decode_errors(Full)).

huge_integer_is_400_under_draft6(_Config) ->
    assert_huge_integer_is_400("/multiple_of_6"),
    ets:delete_all_objects(?LOG_TABLE),
    {500, _, _} = post_json("/broken_multiple_of", #{~"n" => 3}),
    ?assertMatch([_], logged(error, ~"JSON schema cannot be used")).

huge_integer_is_400_under_draft4(_Config) ->
    assert_huge_integer_is_400("/multiple_of_4").

assert_huge_integer_is_400(Path) ->
    ets:delete_all_objects(?LOG_TABLE),
    Huge = <<"{\"n\":1", (binary:copy(~"0", 400))/binary, "}">>,
    {400, _, Body} = request(post, Path, [{"content-type", "application/json"}], Huge),
    ?assertEqual(
        [
            #{
                ~"error_context" => ~"schema_violation",
                ~"error_type" => ~"not_multiple_of",
                ~"pointer" => ~""
            }
        ],
        decode_errors(Body)
    ),
    ?assertEqual([], ets:lookup(?LOG_TABLE, error)),
    {400, _, Small} = post_json(Path, #{~"n" => 3}),
    ?assertMatch(
        [#{~"error_type" := ~"not_multiple_of", ~"pointer" := ~"/n"}], decode_errors(Small)
    ),
    {200, _, _} = post_json(Path, #{~"n" => 4}).

other_methods_with_undecoded_json_body_are_500(_Config) ->
    [
        begin
            ets:delete_all_objects(?LOG_TABLE),
            {500, _, _} = request(
                Method, "/unordered/any", [{"content-type", "application/json"}], ~"{}"
            ),
            ?assertMatch([_], logged(error, ~"JSON body was not decoded"))
        end
     || Method <- [options, propfind]
    ],
    {200, _, _} = request(options, "/unordered/any", [], <<>>),
    {200, _, _} = request(get, "/unordered/any", [{"content-type", "application/json"}], ~"{}"),
    {400, _, _} = request(options, "/any", [{"content-type", "application/json"}], ~"{}").

only_exact_json_type_counts_as_undecoded(_Config) ->
    ets:delete_all_objects(?LOG_TABLE),
    {415, _, _} = request(
        post, "/unordered/tags", [{"content-type", "application/json-seq"}], ~"{}"
    ),
    {415, _, _} = request(
        patch, "/unordered/tags", [{"content-type", "application/json-patch+json"}], ~"[]"
    ),
    {415, _, _} = request(post, "/unordered/tags", [{"content-type", "application/json;"}], ~"{}"),
    {200, _, _} = request(
        options, "/unordered/any", [{"content-type", "application/json-seq"}], ~"{}"
    ),
    ?assertEqual([], logged(error, ~"JSON body was not decoded")),
    {500, _, _} = request(
        post, "/unordered/tags", [{"content-type", "application/json; charset=utf-8"}], ~"{}"
    ),
    ?assertMatch([_], logged(error, ~"JSON body was not decoded")).

schema_path_outside_priv_is_500(_Config) ->
    [
        begin
            ets:delete_all_objects(?LOG_TABLE),
            {500, _, _} = post_json(Path, #{}),
            ?assertMatch(
                [#{reason := {schema_path_outside_priv, _}}],
                logged(error, ~"JSON schema cannot be used")
            )
        end
     || Path <- ["/outside/absolute", "/outside/parent", "/outside/nested_parent"]
    ].

huge_duplicate_ids() ->
    N = 30000,
    #{~"ids" => lists:seq(1, N) ++ [N]}.

timed_post(Path, Body) ->
    Encoded = iolist_to_binary(json:encode(Body)),
    T0 = erlang:monotonic_time(millisecond),
    Response = request(post, Path, [{"content-type", "application/json"}], Encoded),
    {erlang:monotonic_time(millisecond) - T0, Response}.

record_fetches(Listen, Parent) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            {ok, Request} = gen_tcp:recv(Socket, 0, 2000),
            Parent ! {fetch, Request},
            ok = gen_tcp:send(
                Socket,
                ~"HTTP/1.1 200 OK\r\ncontent-length: 2\r\nconnection: close\r\n\r\n{}"
            ),
            gen_tcp:close(Socket),
            record_fetches(Listen, Parent);
        {error, _} ->
            ok
    end.

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
    request(
        post, Path, [{"content-type", "application/json"}], iolist_to_binary(json:encode(Body))
    ).

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
    #{~"errors" := Errors} = json:decode(Body),
    true = is_list(Errors),
    Errors.

header(Name, Headers) ->
    proplists:get_value(Name, Headers).
