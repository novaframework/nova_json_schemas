# 1. The `json_schema` route key names the request-body schema

Date: 2026-11-03

## Status

Accepted

## Context

A Nova route opts in to validation with `extra_state => #{json_schema => Path}`.
The key had no written contract, and two tools read it:

- This plugin validates the decoded request body against it.
- rebar3_nova's `openapi` generator reads the same key and documents it as
  the request schema and also as the `200` response schema.

So a `GET` route that set `json_schema` to document its response made this
plugin reject or crash every request, because a `GET` has no decoded body.
Nova's `nova_request_plugin` never decodes `GET` or `DELETE` bodies.

The two tools also key schemas differently. This plugin keys a schema by its
path relative to the main application's `priv` directory
(`"schemas/tag.json"`). The `openapi` generator keys component schemas by
file basename (`tag`), so two schemas with the same basename in different
directories collide there.

## Decision

`json_schema` names the schema of the **request body**.

- It is enforced on `POST`, `PUT` and `PATCH`, which must carry a decoded
  JSON body.
- On every other method it is enforced only when a decoded body is present.
  Where no body is decoded, the key is not enforced. The exception is a
  JSON body (`application/json` or `application/*+json`) on a method that
  `nova_request_plugin` decodes (anything except `GET` and `DELETE`, so
  `OPTIONS` or a custom method too) that reached this plugin undecoded: that
  is a wrong plugin order and gets a `500`.
- The value is a path relative to the main application's `priv` directory, as
  a string or a binary.

The plugin does not define a response-schema key.

## Consequences

- A route can keep `json_schema` without breaking `GET` or `DELETE` requests,
  but the key still means "request body": documenting a response with it is
  not supported by this plugin's contract.
- Using `json_schema` as the response schema, and keying schemas by basename,
  are rebar3_nova `openapi` behaviours that disagree with this contract. A
  separate response-schema key, and keying by relative path, are follow-ups
  for rebar3_nova.
- A future request-schema option, if one is added, must keep `json_schema`
  working as described here.
