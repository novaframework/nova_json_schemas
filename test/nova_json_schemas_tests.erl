-module(nova_json_schemas_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% render_error tests
%%--------------------------------------------------------------------

render_error_empty_test() ->
    ?assertEqual([], nova_json_schemas:render_error([])).

render_error_single_test() ->
    Input = [{data_invalid, #{<<"type">> => <<"string">>}, wrong_type, 123, <<"name">>}],
    [Error] = nova_json_schemas:render_error(Input),
    ?assertEqual(schema_violation, maps:get(error_context, Error)),
    ?assertEqual(wrong_type, maps:get(error_type, Error)),
    ?assertEqual(123, maps:get(actual_value, Error)),
    ?assertEqual(<<"name">>, maps:get(expected_value, Error)),
    ?assertEqual(#{<<"type">> => <<"string">>}, maps:get(field_info, Error)).

render_error_multiple_test() ->
    Input = [
        {data_invalid, #{}, wrong_type, 1, <<"a">>},
        {data_invalid, #{}, missing_required_property, undefined, <<"b">>}
    ],
    Result = nova_json_schemas:render_error(Input),
    ?assertEqual(2, length(Result)).

%%--------------------------------------------------------------------
%% validate_json tests
%%--------------------------------------------------------------------

validate_json_valid_test() ->
    {ok, _} = application:ensure_all_started(jesse),
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"name">>]
    },
    jesse:add_schema("test_schema", Schema),
    ?assertEqual(
        ok, nova_json_schemas:validate_json("test_schema", #{<<"name">> => <<"alice">>}, [])
    ).

validate_json_invalid_test() ->
    {ok, _} = application:ensure_all_started(jesse),
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"age">> => #{<<"type">> => <<"integer">>}
        },
        <<"required">> => [<<"age">>]
    },
    jesse:add_schema("test_schema_invalid", Schema),
    {error, _} = nova_json_schemas:validate_json(
        "test_schema_invalid", #{<<"age">> => <<"not_a_number">>}, []
    ).

%%--------------------------------------------------------------------
%% plugin_info test
%%--------------------------------------------------------------------

plugin_info_test() ->
    {ok, _} = application:ensure_all_started(nova_json_schemas),
    Result = nova_json_schemas:plugin_info(),
    ?assertMatch({_, _, _, _, _}, Result).

%%--------------------------------------------------------------------
%% pre_request passthrough tests
%%--------------------------------------------------------------------

pre_request_no_schema_no_body_test() ->
    Req = #{has_body => false},
    ?assertEqual({ok, Req}, nova_json_schemas:pre_request(Req, #{})).

pre_request_missing_body_test() ->
    Req = #{extra_state => #{json_schema => <<"some_schema">>}},
    ?assertEqual({error, body_not_parsed}, nova_json_schemas:pre_request(Req, #{})).
