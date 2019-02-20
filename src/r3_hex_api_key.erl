%% Vendored from r3_hex_core v0.4.0, do not edit manually

-module(r3_hex_api_key).
-export([
    list/1,
    get/2,
    add/3,
    delete/2,
    delete_all/1
]).

list(Config) when is_map(Config) ->
    r3_hex_api:get(Config, ["keys"]).

get(Config, Name) when is_map(Config) ->
    r3_hex_api:get(Config, ["keys", Name]).

add(Config, Name, Permissions) when is_map(Config) ->
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    r3_hex_api:post(Config, ["keys"], Params).

delete(Config, Name) when is_map(Config) ->
    r3_hex_api:delete(Config, ["keys", Name]).

delete_all(Config) when is_map(Config) ->
    r3_hex_api:delete(Config, ["keys"]).
