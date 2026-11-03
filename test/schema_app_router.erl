-module(schema_app_router).
-behaviour(nova_router).

-export([routes/1]).

-define(ALL_METHODS, [get, post, put, patch, delete]).

routes(_Environment) ->
    [
        #{
            prefix => "",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, nova_json_schemas, #{render_errors => true}}
            ],
            routes => [
                route("/tags", "schemas/tag.json"),
                route("/draft07", "schemas/draft07.json"),
                route("/lazy", "schemas/nested/lazy.json"),
                route("/missing", "schemas/missing.json"),
                {"/first_error", fun schema_app_controller:handle/1, #{
                    methods => [post],
                    extra_state => #{
                        json_schema => "schemas/tag.json",
                        jesse_options => [{allowed_errors, 0}]
                    }
                }},
                {"/no_schema", fun schema_app_controller:handle/1, #{methods => ?ALL_METHODS}},
                route("/binary_key", ~"schemas/tag.json"),
                route("/array", "schemas/strings.json"),
                route_with_errors("/all_errors", "schemas/strings.json", infinity),
                route_with_errors("/many_errors", "schemas/strings.json", 50),
                route_with_errors("/two_errors", "schemas/strings.json", 2),
                route("/unique", "schemas/unique.json"),
                route("/keyed", "schemas/keyed.json"),
                route("/multiple_of_6", "schemas/multiple_of_6.json"),
                route("/multiple_of_4", "schemas/multiple_of_4.json"),
                route("/broken_multiple_of", "schemas/broken_multiple_of.json"),
                route("/refs/relative", "schemas/refs/relative.json"),
                route("/refs/by_id", "schemas/refs/by_id.json"),
                route("/remote", "remote/root.json"),
                any_method_route("/any", "schemas/tag.json"),
                route("/outside/absolute", outside_schema()),
                route("/outside/parent", "../outside.json"),
                route("/outside/nested_parent", "schemas/../../outside.json")
            ]
        },
        #{
            prefix => "/collect",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, nova_json_schemas, #{render_errors => true}}
            ],
            routes => [route_with_errors("/tags", "schemas/tag.json", 10)]
        },
        #{
            prefix => "/plain",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, nova_json_schemas, #{}}
            ],
            routes => [
                route("/tags", "schemas/tag.json"),
                route_with_errors("/unique_all_errors", "schemas/unique.json", infinity)
            ]
        },
        #{
            prefix => "/unordered",
            security => false,
            plugins => [
                {pre_request, nova_json_schemas, #{}},
                {pre_request, nova_request_plugin, #{decode_json_body => true}}
            ],
            routes => [
                route("/tags", "schemas/tag.json"),
                any_method_route("/any", "schemas/tag.json")
            ]
        }
    ].

route(Path, Schema) ->
    {Path, fun schema_app_controller:handle/1, #{
        methods => ?ALL_METHODS, extra_state => #{json_schema => Schema}
    }}.

route_with_errors(Path, Schema, AllowedErrors) ->
    {Path, fun schema_app_controller:handle/1, #{
        methods => [post],
        extra_state => #{json_schema => Schema, jesse_options => [{allowed_errors, AllowedErrors}]}
    }}.

any_method_route(Path, Schema) ->
    {Path, fun schema_app_controller:handle/1, #{extra_state => #{json_schema => Schema}}}.

outside_schema() ->
    {ok, Path} = application:get_env(schema_app, outside_schema),
    Path.
