# nova_json_schemas

Nova plugin for validating input data against JSON schema.

## Usage

First include `nova_json_schemas` in your `rebar.config` file:

```erlang

{deps, [
        {nova_json_schemas, {git, "https://github.com/novaframework/nova_json_schemas.git", {branch, "master"}}}
	]}.
```

Update your router to make use of this plugin and to tell where JSON-schemas for each route will be located:

```erlang
routes(_Environment) ->
    [
      #{prefix => "",
        security => false,
	plugins => [
		   {pre_request, nova_json_schemas, #{render_errors => true}},
                   {pre_request, nova_request_plugin, #{decode_json_body => true}}
		   ],
	routes => [
	       {"/my_route", {my_controller, do_something}, #{extra_state => #{json_schema => "./schemas/my_schema.jsons"}}}
	         ]
	}
    ].
```

**Note!** You need to invoke the `nova_request_plugin` with `decode_json_body` set to true _AFTER_ the `nova_json_schemas` in the list since Nova runs the plugins bottom-up.

Then put `my_schema.jsons` that contains the schema in your `priv/schemas/` directory (json schemas is read related from priv-directory of your main nova application).

### Additional validation and other options to Jesse

The schema validation is performed by the Jesse library
(https://github.com/for-GET/jesse). To perform additional custom
validation, you can pass an `external_validator` option to Jesse via a
`jesse_options` entry in the `extra_state`. Other options to Jesse are
passed the same way:

``` erlang
  #{extra_state => #{json_schema => "...",
                     jesse_options => [{external_validator, ValidatorFun}]
                    }
   }
```

The validation function takes a value and a Jesse state and should return a
new state; see the Jesse documentation for details.


## Report bugs and/or contribute

If you find a bug or want to contribute either open a issue (Bug) or create a pull request :)
