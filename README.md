# nova_json_schemas

Nova plugin that validates JSON request bodies against JSON schemas, using
[jesse](https://github.com/for-GET/jesse).

Requires OTP 27 or later (schema files are read with the OTP `json` module)
and Nova 0.13 or later.

## Usage

Add `nova_json_schemas` to the deps in your `rebar.config`:

```erlang
{deps, [
    nova_json_schemas
]}.
```

Add the plugin to the routes that should be validated, and point each route
at its schema with `json_schema` in `extra_state`:

```erlang
routes(_Environment) ->
    [
        #{
            prefix => "/api",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{decode_json_body => true}},
                {pre_request, nova_json_schemas, #{render_errors => true}}
            ],
            routes => [
                {"/tags", fun my_tag_controller:create/1, #{
                    methods => [post],
                    extra_state => #{json_schema => "schemas/tag.json"}
                }}
            ]
        }
    ].
```

Nova runs the plugins of a phase in list order, so `nova_request_plugin` with
`decode_json_body => true` must come **before** `nova_json_schemas`: it
decodes the body into the `json` key that this plugin validates.

Put the schema in `priv/schemas/tag.json` of your main Nova application
(the `bootstrap_application`). `json_schema` is a path relative to that
`priv` directory, written as a string (`"schemas/tag.json"`). Every `*.json`
file under `priv/schemas/` is loaded when the plugin starts; other paths
under `priv` are loaded on first use.

## Supported schema drafts

jesse validates JSON Schema drafts 3, 4 and 6. A schema's `$schema` must be
one of these exact values:

- `http://json-schema.org/draft-03/schema#`
- `http://json-schema.org/draft-04/schema#`
- `http://json-schema.org/draft-06/schema#`

A schema without `$schema` is validated as draft-06. Draft-07 and later are
not supported. Such a schema logs a warning when it is loaded, and every
request to a route that uses it gets a `500` (with an error in the log), since
it is a server configuration problem and not a client error. A draft-07
schema that does not use `if`/`then`/`else` usually works as draft-06 once its
`$schema` line is changed.

## Which requests are validated

- `POST`, `PUT` and `PATCH` requests to a route with `json_schema` must carry
  a JSON body.
- Requests with any other method are validated only if a decoded body is
  present (`nova_request_plugin` never decodes `GET` or `DELETE` bodies), so
  `json_schema` on a `GET` route, for example to document a response for an
  OpenAPI generator, does not reject the request.
- Routes without `json_schema` are not touched.

## Responses

| Situation | Status |
| --- | --- |
| Body is valid | request continues to the controller |
| Body fails the schema | `400`, with an error body if `render_errors => true` |
| No body (`POST`/`PUT`/`PATCH`) | `400` |
| Body is not JSON (form, `text/plain`, no `content-type`) | `415`, with `accept: application/json` |
| JSON body that is empty or malformed | `400` (sent by `nova_request_plugin`) |
| JSON body not decoded, `nova_request_plugin` missing or after this plugin | `500`, and an error in the log |
| Schema missing, unreadable or of an unsupported draft | `500`, and an error in the log |

These follow RFC 9110: `415 Unsupported Media Type` when the content is in a
format the route does not accept, `400 Bad Request` when the request is
missing its content or the content is invalid.

## Error body

With `render_errors => true`, a body that fails validation gets a `400` with
`content-type: application/json` and a JSON array holding one object per
error. Every error is reported, not only the first:

```json
[
  {"error_context": "schema_violation", "error_type": "missing_required_property", "path": "", "property": "slug"},
  {"error_context": "schema_violation", "error_type": "wrong_type", "path": "/name"}
]
```

| Key | Always | Meaning |
| --- | --- | --- |
| `error_context` | yes | always `"schema_violation"` |
| `error_type` | yes | the jesse error, for example `wrong_type`, `missing_required_property`, `no_extra_properties_allowed`, `not_in_enum`, `wrong_length`, `not_in_range` |
| `path` | yes | JSON Pointer (RFC 6901) to the value that failed; `""` is the whole body |
| `property` | no | the missing property, for `missing_required_property` and `missing_dependency` |

The body never contains the schema, its `$id` or a server path.

Without `render_errors` (the default) the `400` has no body.

## Options

Plugin options, in the plugin's options map:

| Option | Default | Meaning |
| --- | --- | --- |
| `render_errors` | `false` | send the error body described above |

Route options, in `extra_state`:

| Key | Meaning |
| --- | --- |
| `json_schema` | schema path relative to the main application's `priv` directory |
| `jesse_options` | options passed to `jesse:validate/3` |

`jesse_options` is a proplist. For example, an extra validation function:

```erlang
#{extra_state => #{
    json_schema => "schemas/tag.json",
    jesse_options => [{external_validator, fun my_validator:validate/2}]
}}
```

The plugin passes `{allowed_errors, infinity}` so that every error is
reported. To stop at the first error, set `{allowed_errors, 0}` in
`jesse_options`. See the jesse documentation for the other options.

## License

Apache License 2.0, see [LICENSE](LICENSE).
