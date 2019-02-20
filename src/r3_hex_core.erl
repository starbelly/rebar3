%% Vendored from r3_hex_core v0.4.0, do not edit manually

-module(r3_hex_core).

-include("r3_hex_core.hrl").

-export([default_config/0, version/0]).

-export_type([config/0]).

%% https://hex.pm/docs/public_keys
-define(HEXPM_PUBLIC_KEY, <<"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
0wIDAQAB
-----END PUBLIC KEY-----">>).


-type config() :: #{
    api_key => binary() | undefined,
    api_url => binary(),
    http_adapter => module(),
    http_etag => binary() | undefined,
    http_adapter_config => map(),
    http_user_agent_fragment => binary(),
    repo_key => binary() | undefined,
    repo_name => binary(),
    repo_public_key => binary(),
    repo_url => binary(),
    repo_verify => boolean(),
    repo_verify_origin => boolean()
}.

-spec default_config() -> config().
default_config() ->
    #{
        api_key => undefined,
        api_url => <<"https://hex.pm/api">>,
        http_adapter => r3_hex_http_httpc,
        http_adapter_config => #{profile => default},
        http_etag => undefined,
        http_user_agent_fragment => <<"(httpc)">>,
        repo_key => undefined,
        repo_name => <<"hexpm">>,
        repo_public_key => ?HEXPM_PUBLIC_KEY,
        repo_url => <<"https://repo.hex.pm">>,
        repo_verify => true,
        repo_verify_origin => true
    }.

version() ->
  ?HEX_CORE_VERSION.
