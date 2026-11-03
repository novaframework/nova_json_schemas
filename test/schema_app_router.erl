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
                {"/all_errors", fun schema_app_controller:handle/1, #{
                    methods => [post],
                    extra_state => #{
                        json_schema => "schemas/strings.json",
                        jesse_options => [{allowed_errors, infinity}]
                    }
                }}
            ]
        },
        #{
            prefix => "/capped",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, nova_json_schemas, #{render_errors => true, max_errors => 3}}
            ],
            routes => [route("/array", "schemas/strings.json")]
        },
        #{
            prefix => "/plain",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, nova_json_schemas, #{}}
            ],
            routes => [route("/tags", "schemas/tag.json")]
        },
        #{
            prefix => "/unordered",
            security => false,
            plugins => [
                {pre_request, nova_json_schemas, #{}},
                {pre_request, nova_request_plugin, #{decode_json_body => true}}
            ],
            routes => [route("/tags", "schemas/tag.json")]
        }
    ].

route(Path, Schema) ->
    {Path, fun schema_app_controller:handle/1, #{
        methods => ?ALL_METHODS, extra_state => #{json_schema => Schema}
    }}.
