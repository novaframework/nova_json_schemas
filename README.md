# nova_json_schemas

Nova plugin that validates JSON request bodies against JSON schemas, using
[jesse](https://github.com/for-GET/jesse).

Requires OTP 27 or later (schema files are read with the OTP `json` module)
and Nova 0.18 or later. A body counts as JSON when its `content-type` is
`application/json` or any `application/*+json` type (RFC 6839), such as
`application/vnd.api+json` or `application/merge-patch+json`. The type is
compared case-insensitively and parameters such as `charset=utf-8` are
ignored. `application/json-seq` is not JSON. This is the rule
`nova_request_plugin` uses to decide what to decode in the Nova release after
0.18.1; Nova 0.18.1 and earlier decode only types that start with lowercase
`application/json`, so with them a `+json` or mixed-case body reaches this
plugin undecoded and gets the `500` for a wrong plugin order.

## Usage

Add `nova_json_schemas` to the deps in your `rebar.config`:

```erlang
{deps, [
    {nova_json_schemas, "~> 0.3"}
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
`priv` directory, written as a string (`"schemas/tag.json"`) or a binary
(`~"schemas/tag.json"`). Every `*.json`
file under `priv/schemas/` is loaded when the plugin starts; other paths
under `priv` are loaded on first use. A path that leads outside `priv` (an
absolute path, or one that climbs out with `..`) is not read: requests to
that route get a `500` and the log names `schema_path_outside_priv`.

## `$ref`

A `$ref` resolves only against schemas that are already loaded: everything
under `priv/schemas/`, plus schemas loaded on first use. Relative refs
(`"item.json"`, `"../common/id.json"`) resolve against the referring
schema's path or its `$id`. The plugin never fetches a ref over `http(s)` or
reads it from the working directory, even when a schema's `$id` is an
`http` URI. A ref that is not loaded makes the route answer `500`.

A route can choose another loader with `{schema_loader_fun, Fun}` in its
`jesse_options`; that replaces this rule for the route.

## Supported schema drafts

jesse validates JSON Schema drafts 3, 4 and 6. A schema's `$schema` must be
one of these exact values:

- `http://json-schema.org/draft-03/schema#`
- `http://json-schema.org/draft-04/schema#`
- `http://json-schema.org/draft-06/schema#`

A schema without `$schema` is validated as draft-06, so any draft-07 or later
keyword in it (`if`/`then`/`else`, for example) is silently ignored. Always
declare `$schema`. Draft-07 and later are not supported. Such a schema logs a warning when it is loaded, and every
request to a route that uses it gets a `500` (with an error in the log), since
it is a server configuration problem and not a client error. A draft-07
schema that does not use `if`/`then`/`else` usually works as draft-06 once its
`$schema` line is changed.

## Which requests are validated

`json_schema` names the schema of the **request body**. It is enforced
wherever a body is decoded:

- `POST`, `PUT` and `PATCH` requests to a route with `json_schema` must carry
  a JSON body.
- Requests with any other method are validated only if a decoded body is
  present. `nova_request_plugin` never decodes `GET` or `DELETE` bodies, so
  on those methods `json_schema` is not enforced.
- Any other method (`OPTIONS`, `HEAD`, a custom method) with a JSON body
  that was not decoded gets a `500`, the same as `POST`, since
  `nova_request_plugin` would have decoded it. This catches a wrong plugin
  order on every method. Other types, such as `application/json-seq`, are
  not JSON by this rule: they pass through on these methods and get a `415`
  on `POST`, `PUT` and `PATCH`.
- Routes without `json_schema` are not touched.

See [ADR 0001](docs/adr/0001-json-schema-route-key.md).

## Responses

| Situation | Status |
| --- | --- |
| Body is valid | request continues to the controller |
| Body fails the schema | `400`, with an error body if `render_errors => true` |
| No body (`POST`/`PUT`/`PATCH`) | `400` |
| Body is not JSON (form, `text/plain`, `application/json-seq`, no `content-type`) | `415`, with `accept: application/json` (and `accept-patch: application/json` on `PATCH`) |
| JSON body that is empty or malformed | `400` (sent by `nova_request_plugin`) |
| JSON body not decoded, `nova_request_plugin` missing or after this plugin (any method except `GET` and `DELETE`) | `500`, and an error in the log |
| Schema missing, unreadable, outside `priv`, of an unsupported draft, or with a `$ref` that is not loaded | `500`, and an error in the log |

These follow RFC 9110: `415 Unsupported Media Type` when the content is in a
format the route does not accept, `400 Bad Request` when the request is
missing its content or the content is invalid.

## Error body

With `render_errors => true`, a body that fails validation gets a `400` with
`content-type: application/json` and a JSON array with one object per error.
jesse stops at the first error unless the route sets `allowed_errors` in its
`jesse_options` (see [Options](#options)). With `{allowed_errors, 10}`:

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
| `path` | yes | JSON Pointer (RFC 6901) to the value that failed; `""` is the whole body. At most 256 bytes: a longer path is cut at a whole segment, so it points to an ancestor of the value |
| `property` | no | the missing property, for `missing_required_property` and `missing_dependency` |

The body never contains the schema, its `$id` or a server path.

An integer in the body too large for jesse to divide by `multipleOf` (or
draft-03 `divisibleBy`) fails validation with `error_type`
`not_multiple_of` and `path` `""`.

Without `render_errors` (the default) the `400` has no body, and jesse stops at
the first error.

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

With `render_errors => true` the route's `jesse_options` go to jesse as
they are, so `allowed_errors` sets how many errors jesse collects: the
default `0` stops at the first error and `{allowed_errors, N}` stops after
`N + 1` errors. Without `render_errors` the `400` has no body, so jesse
always stops at the first error. See the jesse documentation for the other
options.

Keep `N` small and finite, such as 10. The client controls how many errors a
body has, and with jesse 1.8.x collecting errors costs CPU time that grows
with the square of the error count, while the error body grows linearly with
it: 40 000 errors take about 4 s, and a 1 MB body can produce about 500 000
errors and an error body of about 39 MB. Any atom compares greater than an
integer in jesse's check, so `infinity`, and also a misspelt atom such as
`infnity`, means no limit.

An `allowed_errors` above 0 also lets validation continue past an error that
would otherwise end it. `uniqueItems` compares every pair of items, so on a
large array it is quadratic in the array length, and a `maxItems` error no
longer stops validation before it runs. Keep the default, or bound the
request body size, on routes whose schema uses `uniqueItems`.

## Regular expressions

jesse runs `pattern` and `patternProperties` with OTP's `re`. A pathological
regex that hits `re`'s match limit is treated as no match. For `pattern` that
fails closed: the value is rejected. For `patternProperties` a property that
should have matched is then not checked against that sub-schema, which fails
closed only if the schema also sets `additionalProperties: false`.

## License

Apache License 2.0, see [LICENSE](LICENSE).
