-module(nova_json_schemas).
-behaviour(nova_plugin).

-export([
         pre_request/2,
         post_request/2,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Options :: map()) ->
                         {ok, Req0 :: cowboy_req:req()} |
                         {stop, Req0 :: cowboy_req:req()} |
                         {error, Reason :: term()}.
pre_request(Req = #{extra_state := #{json_schema := SchemaLocation}, json := JSON}, Options) ->
    %% JSON have already been parsed so we can just continue with the validation
    case validate_json(SchemaLocation, JSON) of
	ok ->
	    logger:debug("Schema validation on JSON body successful"),
	    {ok, Req};
	{error, Errors} ->
	    logger:debug("Got validation-errors on JSON body. Errors: ~p", [Errors]),
	    case maps:get(render_errors, Options, false) of
		true ->
		    logger:debug("Rendering validation-errors and send back to requester"),
		    JsonLib = nova:get_env(json_lib, thoas),
		    Req0 = cowboy_req:set_resp_headers(#{<<"content-type">> => <<"application/json">>}, Req),
		    ErrorStruct = render_error(Errors),
		    ErrorJson = erlang:apply(JsonLib, encode, [ErrorStruct]),
		    Req1 = cowboy_req:set_resp_body(ErrorJson, Req0),
		    Req2 = cowboy_req:reply(400, Req1),
		    {stop, Req2};
		_ ->
		    logger:debug("render_errors-option not set for plugin nova_json_schemas - returning plain 400-status to requester"),
		    Req0 = cowboy_req:reply(400, Req),
		    {stop, Req0}
	    end
    end;
pre_request(Req = #{extra_state := #{json_schema := SchemaLocation}}, _Options) ->
    %% The body have not been parsed. Log and error and stop
    logger:error("JSON Schema is set in 'extra_state' but body have not yet been parsed - rearrange your plugins so that JSON plugin is ran before this.."),
    {error, body_not_parsed};
pre_request(Req, _Options) ->
    %% 'json_schema' is not set or 'extra_state' is completly missing. Just continue.
    logger:debug("No schema is set for this route so will continue executing"),
    {ok, Req}.

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Options :: map()) ->
                          {ok, Req0 :: cowboy_req:req()} |
                          {stop, Req0 :: cowboy_req:req()} |
                          {error, Reason :: term()}.
post_request(Req, _Options) ->
    {ok, Req}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() -> {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary(),
                       [{Key :: atom(), OptionDescription :: atom()}]}.
plugin_info() ->
    {<<"JSON schema plugin">>,
     <<"0.0.1">>,
     <<"Niclas Axelsson <niclas@burbas.se">>,
     <<"Validating JSON with schemas">>,
     [
      {render_errors, <<"If this is set, validation-errors is returned to the requester">>}
     ]}. %% Options is specified as {Key, Description}


validate_json(SchemaLocation, Json) ->
    case jesse:validate(SchemaLocation, Json) of
	{error, {database_error, _, schema_not_found}} ->
	    %% Load the schema
	    {ok, MainApp} = nova:get_main_app(),
	    PrivDir = code:lib_dir(MainApp, priv),
	    SchemaLocation0 = filename:join([PrivDir, SchemaLocation]),
	    {ok, Filecontent} = file:read_file(SchemaLocation0),
	    JsonLib = nova:get_env(json_lib, thoas),
	    {ok, Schema} = erlang:apply(JsonLib, decode, [Filecontent]),
	    jesse:add_schema(SchemaLocation, Schema),
	    validate_json(SchemaLocation, Json);
	{error, ValidationError} ->
	    {error, ValidationError};
	{ok, _} ->
	    ok
    end.


render_error([]) -> [];
render_error([{data_invalid, FieldInfo, Type, ActualValue, Field}|Tl]) ->
    %% We don't do any fancy with this currently.
    [#{field_info => FieldInfo,
       error_type => Type,
       actual_value => ActualValue,
       expected_value => Field}|render_error(Tl)].
