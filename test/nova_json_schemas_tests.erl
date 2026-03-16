-module(nova_json_schemas_tests).

-include_lib("eunit/include/eunit.hrl").

%%% =================================================================
%%% to_json_pointer/1
%%% =================================================================

to_json_pointer_empty_test() ->
    ?assertEqual(<<"/">>, nova_json_schemas:to_json_pointer([])).

to_json_pointer_single_segment_test() ->
    ?assertEqual(<<"/name">>, nova_json_schemas:to_json_pointer([<<"name">>])).

to_json_pointer_nested_test() ->
    ?assertEqual(
        <<"/address/street">>,
        nova_json_schemas:to_json_pointer([<<"address">>, <<"street">>])
    ).

to_json_pointer_integer_index_test() ->
    ?assertEqual(
        <<"/items/0/name">>,
        nova_json_schemas:to_json_pointer([<<"items">>, 0, <<"name">>])
    ).

to_json_pointer_atom_segment_test() ->
    ?assertEqual(<<"/foo">>, nova_json_schemas:to_json_pointer([foo])).

to_json_pointer_escapes_tilde_test() ->
    ?assertEqual(<<"/a~0b">>, nova_json_schemas:to_json_pointer([<<"a~b">>])).

to_json_pointer_escapes_slash_test() ->
    ?assertEqual(<<"/a~1b">>, nova_json_schemas:to_json_pointer([<<"a/b">>])).

to_json_pointer_escapes_both_test() ->
    ?assertEqual(<<"/a~0b~1c">>, nova_json_schemas:to_json_pointer([<<"a~b/c">>])).

%%% =================================================================
%%% format_error_message/3
%%% =================================================================

format_error_message_wrong_type_test() ->
    ?assertEqual(
        <<"Must be of type string">>,
        nova_json_schemas:format_error_message(wrong_type, <<"string">>, 42)
    ).

format_error_message_not_in_enum_test() ->
    ?assertEqual(
        <<"Value is not in the allowed set of values">>,
        nova_json_schemas:format_error_message(not_in_enum, [1, 2], 3)
    ).

format_error_message_missing_required_property_test() ->
    ?assertEqual(
        <<"Missing required property: name">>,
        nova_json_schemas:format_error_message(missing_required_property, <<"name">>, undefined)
    ).

format_error_message_no_extra_properties_test() ->
    ?assertEqual(
        <<"Additional properties are not allowed">>,
        nova_json_schemas:format_error_message(no_extra_properties_allowed, undefined, undefined)
    ).

format_error_message_not_unique_test() ->
    ?assertEqual(
        <<"Array items are not unique">>,
        nova_json_schemas:format_error_message(not_unique, undefined, [1, 1])
    ).

format_error_message_no_match_test() ->
    ?assertEqual(
        <<"Value does not match the required pattern">>,
        nova_json_schemas:format_error_message(no_match, undefined, <<"abc">>)
    ).

format_error_message_file_error_test() ->
    ?assertEqual(
        <<"Schema file error: enoent">>,
        nova_json_schemas:format_error_message(file_error, enoent, undefined)
    ).

format_error_message_unknown_type_no_details_test() ->
    ?assertEqual(
        <<"Validation failed: custom_check">>,
        nova_json_schemas:format_error_message(custom_check, undefined, undefined)
    ).

format_error_message_unknown_type_with_details_test() ->
    ?assertEqual(
        <<"Validation failed (custom_check): some detail">>,
        nova_json_schemas:format_error_message(custom_check, <<"some detail">>, undefined)
    ).

%%% =================================================================
%%% safe_format/1
%%% =================================================================

safe_format_undefined_test() ->
    ?assertEqual(<<"">>, nova_json_schemas:safe_format(undefined)).

safe_format_binary_test() ->
    ?assertEqual(<<"hello">>, nova_json_schemas:safe_format(<<"hello">>)).

safe_format_atom_test() ->
    ?assertEqual(<<"foo">>, nova_json_schemas:safe_format(foo)).

safe_format_integer_test() ->
    ?assertEqual(<<"42">>, nova_json_schemas:safe_format(42)).

safe_format_float_test() ->
    Result = nova_json_schemas:safe_format(3.14),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"3.14", _/binary>>, Result).

safe_format_other_test() ->
    Result = nova_json_schemas:safe_format({some, tuple}),
    ?assert(is_binary(Result)).

%%% =================================================================
%%% safe_value/1
%%% =================================================================

safe_value_map_test() ->
    M = #{a => 1},
    ?assertEqual(M, nova_json_schemas:safe_value(M)).

safe_value_list_test() ->
    ?assertEqual([1, 2], nova_json_schemas:safe_value([1, 2])).

safe_value_binary_test() ->
    ?assertEqual(<<"hi">>, nova_json_schemas:safe_value(<<"hi">>)).

safe_value_number_test() ->
    ?assertEqual(42, nova_json_schemas:safe_value(42)),
    ?assertEqual(1.5, nova_json_schemas:safe_value(1.5)).

safe_value_boolean_test() ->
    ?assertEqual(true, nova_json_schemas:safe_value(true)),
    ?assertEqual(false, nova_json_schemas:safe_value(false)).

safe_value_null_test() ->
    ?assertEqual(null, nova_json_schemas:safe_value(null)).

safe_value_other_test() ->
    Result = nova_json_schemas:safe_value({some, tuple}),
    ?assert(is_binary(Result)).

%%% =================================================================
%%% render_one_error/1
%%% =================================================================

render_one_error_data_invalid_with_details_test() ->
    Error = {data_invalid, #{}, {wrong_type, <<"string">>}, 42, [<<"name">>]},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"/name">>, maps:get(path, Result)),
    ?assertEqual(wrong_type, maps:get(type, Result)),
    ?assertEqual(<<"Must be of type string">>, maps:get(message, Result)),
    ?assertEqual(42, maps:get(actual_value, Result)),
    ?assertEqual(<<"string">>, maps:get(expected, Result)).

render_one_error_data_invalid_simple_test() ->
    Error = {data_invalid, #{}, not_in_enum, <<"foo">>, [<<"status">>]},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"/status">>, maps:get(path, Result)),
    ?assertEqual(not_in_enum, maps:get(type, Result)),
    ?assertEqual(<<"foo">>, maps:get(actual_value, Result)),
    ?assertNot(maps:is_key(expected, Result)).

render_one_error_schema_invalid_with_details_test() ->
    Error = {schema_invalid, #{}, {missing_required_property, <<"id">>}},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"">>, maps:get(path, Result)),
    ?assertEqual(missing_required_property, maps:get(type, Result)).

render_one_error_schema_invalid_simple_test() ->
    Error = {schema_invalid, #{}, some_error},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"">>, maps:get(path, Result)),
    ?assertEqual(some_error, maps:get(type, Result)).

render_one_error_parse_error_test() ->
    Error = {data_error, {parse_error, <<"bad json">>}},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"">>, maps:get(path, Result)),
    ?assertEqual(parse_error, maps:get(type, Result)).

render_one_error_schema_parse_error_test() ->
    Error = {schema_error, {parse_error, <<"bad schema">>}},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"">>, maps:get(path, Result)),
    ?assertEqual(schema_parse_error, maps:get(type, Result)).

render_one_error_unknown_test() ->
    Error = {totally_unexpected, error},
    Result = nova_json_schemas:render_one_error(Error),
    ?assertEqual(<<"">>, maps:get(path, Result)),
    ?assertEqual(unknown_error, maps:get(type, Result)).

%%% =================================================================
%%% render_errors/1
%%% =================================================================

render_errors_empty_test() ->
    ?assertEqual([], nova_json_schemas:render_errors([])).

render_errors_multiple_test() ->
    Errors = [
        {data_invalid, #{}, not_in_enum, <<"x">>, [<<"status">>]},
        {data_invalid, #{}, {wrong_type, <<"integer">>}, <<"abc">>, [<<"age">>]}
    ],
    Result = nova_json_schemas:render_errors(Errors),
    ?assertEqual(2, length(Result)),
    [First, Second] = Result,
    ?assertEqual(<<"/status">>, maps:get(path, First)),
    ?assertEqual(<<"/age">>, maps:get(path, Second)).

%%% =================================================================
%%% build_problem_details/3
%%% =================================================================

build_problem_details_flat_test() ->
    ErrorList = [
        #{path => <<"/name">>, type => wrong_type, message => <<"Must be of type string">>}
    ],
    Result = nova_json_schemas:build_problem_details(422, ErrorList, false),
    ?assertEqual(<<"about:blank">>, maps:get(type, Result)),
    ?assertEqual(<<"Validation Error">>, maps:get(title, Result)),
    ?assertEqual(422, maps:get(status, Result)),
    ?assertEqual(<<"Request body failed JSON schema validation">>, maps:get(detail, Result)),
    ?assertEqual(ErrorList, maps:get(errors, Result)).

build_problem_details_custom_status_test() ->
    Result = nova_json_schemas:build_problem_details(400, [], false),
    ?assertEqual(400, maps:get(status, Result)).

build_problem_details_grouped_test() ->
    ErrorList = [
        #{path => <<"/name">>, type => wrong_type, message => <<"Must be of type string">>},
        #{path => <<"/name">>, type => wrong_length, message => <<"String length is invalid">>},
        #{path => <<"/age">>, type => wrong_type, message => <<"Must be of type integer">>}
    ],
    Result = nova_json_schemas:build_problem_details(422, ErrorList, true),
    Grouped = maps:get(errors, Result),
    ?assert(is_map(Grouped)),
    ?assertEqual(2, map_size(Grouped)),
    ?assertEqual(
        [<<"Must be of type string">>, <<"String length is invalid">>],
        maps:get(<<"/name">>, Grouped)
    ),
    ?assertEqual(
        [<<"Must be of type integer">>],
        maps:get(<<"/age">>, Grouped)
    ).

%%% =================================================================
%%% group_errors_by_field/1
%%% =================================================================

group_errors_by_field_empty_test() ->
    ?assertEqual(#{}, nova_json_schemas:group_errors_by_field([])).

group_errors_by_field_single_test() ->
    Errors = [#{path => <<"/name">>, message => <<"required">>}],
    Result = nova_json_schemas:group_errors_by_field(Errors),
    ?assertEqual(#{<<"/name">> => [<<"required">>]}, Result).

group_errors_by_field_multiple_same_field_test() ->
    Errors = [
        #{path => <<"/name">>, message => <<"required">>},
        #{path => <<"/name">>, message => <<"too short">>}
    ],
    Result = nova_json_schemas:group_errors_by_field(Errors),
    ?assertEqual(#{<<"/name">> => [<<"required">>, <<"too short">>]}, Result).

%%% =================================================================
%%% Tests requiring jesse application (test generators with setup)
%%% =================================================================

jesse_setup() ->
    jesse_database:load_all(),
    ok.

jesse_cleanup(_) ->
    ok.

validate_json_test_() ->
    {setup, fun jesse_setup/0, fun jesse_cleanup/1, [
        {"valid JSON passes validation", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            jesse:add_schema(<<"test_schema">>, Schema),
            ?assertEqual(
                ok,
                nova_json_schemas:validate_json(
                    <<"test_schema">>, #{<<"name">> => <<"Alice">>}, []
                )
            )
        end},
        {"invalid JSON fails validation", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            jesse:add_schema(<<"test_schema_inv">>, Schema),
            ?assertMatch(
                {error, _},
                nova_json_schemas:validate_json(<<"test_schema_inv">>, #{<<"name">> => 42}, [])
            )
        end}
    ]}.

pre_request_test_() ->
    {setup, fun jesse_setup/0, fun jesse_cleanup/1, [
        {"passthrough when no schema configured", fun() ->
            Req = #{method => <<"GET">>, has_body => false},
            ?assertEqual(
                {ok, Req, my_state},
                nova_json_schemas:pre_request(Req, #{}, #{}, my_state)
            )
        end},
        {"error when body not parsed", fun() ->
            Req = #{extra_state => #{json_schema => <<"some_schema">>}},
            ?assertEqual(
                {error, body_not_parsed},
                nova_json_schemas:pre_request(Req, #{}, #{}, my_state)
            )
        end},
        {"valid JSON passes through", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            jesse:add_schema(<<"pre_req_test">>, Schema),
            JSON = #{<<"name">> => <<"Alice">>},
            Req = #{
                extra_state => #{json_schema => <<"pre_req_test">>},
                json => JSON
            },
            ?assertMatch(
                {ok, _, my_state},
                nova_json_schemas:pre_request(Req, #{}, #{}, my_state)
            )
        end},
        {"invalid JSON returns 422 without render", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            jesse:add_schema(<<"pre_req_no_render">>, Schema),
            JSON = #{<<"name">> => 42},
            Req = #{
                extra_state => #{json_schema => <<"pre_req_no_render">>},
                json => JSON
            },
            meck:new(cowboy_req, [passthrough]),
            meck:expect(cowboy_req, reply, fun(Status, R) -> R#{status => Status} end),
            try
                {stop, ResultReq, my_state} =
                    nova_json_schemas:pre_request(Req, #{}, #{}, my_state),
                ?assertEqual(422, maps:get(status, ResultReq))
            after
                meck:unload(cowboy_req)
            end
        end},
        {"invalid JSON respects custom status code", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            jesse:add_schema(<<"pre_req_custom_status">>, Schema),
            JSON = #{<<"name">> => 42},
            Req = #{
                extra_state => #{json_schema => <<"pre_req_custom_status">>},
                json => JSON
            },
            meck:new(cowboy_req, [passthrough]),
            meck:expect(cowboy_req, reply, fun(Status, R) -> R#{status => Status} end),
            try
                {stop, ResultReq, my_state} =
                    nova_json_schemas:pre_request(
                        Req, #{}, #{status_code => 400}, my_state
                    ),
                ?assertEqual(400, maps:get(status, ResultReq))
            after
                meck:unload(cowboy_req)
            end
        end},
        {"invalid JSON renders RFC 9457 problem+json", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            jesse:add_schema(<<"pre_req_render">>, Schema),
            JSON = #{<<"name">> => 42},
            Req = #{
                extra_state => #{json_schema => <<"pre_req_render">>},
                json => JSON
            },
            meck:new(cowboy_req, [passthrough]),
            meck:expect(cowboy_req, set_resp_headers, fun(_Headers, R) -> R end),
            meck:expect(cowboy_req, set_resp_body, fun(Body, R) -> R#{resp_body => Body} end),
            meck:expect(cowboy_req, reply, fun(Status, R) -> R#{status => Status} end),
            try
                {stop, ResultReq, my_state} =
                    nova_json_schemas:pre_request(
                        Req, #{}, #{render_errors => true}, my_state
                    ),
                ?assertEqual(422, maps:get(status, ResultReq)),
                RespBody = json:decode(iolist_to_binary(maps:get(resp_body, ResultReq))),
                ?assertEqual(<<"about:blank">>, maps:get(<<"type">>, RespBody)),
                ?assertEqual(<<"Validation Error">>, maps:get(<<"title">>, RespBody)),
                ?assertEqual(422, maps:get(<<"status">>, RespBody)),
                ?assert(is_list(maps:get(<<"errors">>, RespBody)))
            after
                meck:unload(cowboy_req)
            end
        end},
        {"invalid JSON renders grouped errors", fun() ->
            Schema = #{
                <<"type">> => <<"object">>,
                <<"required">> => [<<"name">>, <<"age">>],
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>},
                    <<"age">> => #{<<"type">> => <<"integer">>}
                }
            },
            jesse:add_schema(<<"pre_req_grouped">>, Schema),
            JSON = #{<<"name">> => 42, <<"age">> => <<"not_int">>},
            Req = #{
                extra_state => #{json_schema => <<"pre_req_grouped">>},
                json => JSON
            },
            meck:new(cowboy_req, [passthrough]),
            meck:expect(cowboy_req, set_resp_headers, fun(_Headers, R) -> R end),
            meck:expect(cowboy_req, set_resp_body, fun(Body, R) -> R#{resp_body => Body} end),
            meck:expect(cowboy_req, reply, fun(Status, R) -> R#{status => Status} end),
            try
                {stop, ResultReq, my_state} =
                    nova_json_schemas:pre_request(
                        Req,
                        #{},
                        #{render_errors => true, group_by_field => true},
                        my_state
                    ),
                RespBody = json:decode(iolist_to_binary(maps:get(resp_body, ResultReq))),
                Errors = maps:get(<<"errors">>, RespBody),
                ?assert(is_map(Errors))
            after
                meck:unload(cowboy_req)
            end
        end}
    ]}.

%%% =================================================================
%%% post_request/4
%%% =================================================================

post_request_passthrough_test() ->
    Req = #{method => <<"GET">>},
    ?assertEqual({ok, Req, my_state}, nova_json_schemas:post_request(Req, #{}, #{}, my_state)).

%%% =================================================================
%%% plugin_info/0
%%% =================================================================

plugin_info_test() ->
    application:load(nova_json_schemas),
    Info = nova_json_schemas:plugin_info(),
    ?assert(is_map(Info)),
    ?assert(maps:is_key(title, Info)),
    ?assert(maps:is_key(version, Info)),
    ?assert(maps:is_key(options, Info)),
    Options = maps:get(options, Info),
    OptionKeys = [K || {K, _} <- Options],
    ?assert(lists:member(render_errors, OptionKeys)),
    ?assert(lists:member(status_code, OptionKeys)),
    ?assert(lists:member(group_by_field, OptionKeys)).
