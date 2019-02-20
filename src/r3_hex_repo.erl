%% Vendored from r3_hex_core v0.4.0, do not edit manually

-module(r3_hex_repo).
-export([
    get_names/1,
    get_versions/1,
    get_package/2,
    get_tarball/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Gets names resource from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_names(r3_hex_core:default_config()).
%% {ok, {200, ...,
%%     [
%%         #{name => <<"package1">>},
%%         #{name => <<"package2">>},
%%     ]}}
%% '''
%% @end
get_names(#{repo_name := Repository} = Config) when is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> r3_hex_registry:decode_names(Data, Repository);
            false -> r3_hex_registry:decode_names(Data, no_verify)
        end
    end,
    get_protobuf(Config, <<"/names">>, Decoder).

%% @doc
%% Gets versions resource from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_versions(Config).
%% {ok, {200, ...,
%%     [
%%         #{name => <<"package1">>, retired => [],
%%           versions => [<<"1.0.0">>]},
%%         #{name => <<"package2">>, retired => [<<"0.5.0>>"],
%%           versions => [<<"0.5.0">>, <<"1.0.0">>]},
%%     ]}}
%% '''
%% @end
get_versions(#{repo_name := Repository} = Config) when is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> r3_hex_registry:decode_versions(Data, Repository);
            false -> r3_hex_registry:decode_versions(Data, no_verify)
        end
    end,
    get_protobuf(Config, <<"/versions">>, Decoder).

%% @doc
%% Gets package resource from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_package(r3_hex_core:default_config(), <<"package1">>).
%% {ok, {200, ...,
%%     {
%%         #{checksum => ..., version => <<"0.5.0">>, dependencies => []},
%%         #{checksum => ..., version => <<"1.0.0">>, dependencies => [
%%             #{package => <<"package2">>, optional => true, requirement => <<"~> 0.1">>}
%%         ]},
%%     ]}}
%% '''
%% @end
get_package(#{repo_name := Repository} = Config, Name) when is_binary(Name) and is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> r3_hex_registry:decode_package(Data, Repository, Name);
            false -> r3_hex_registry:decode_package(Data, no_verify, no_verify)
        end
    end,
    get_protobuf(Config, <<"/packages/", Name/binary>>, Decoder).

%% @doc
%% Gets tarball from the repository.
%%
%% Examples:
%%
%% ```
%% > {ok, {200, _, Tarball}} = r3_hex_repo:get_tarball(<<"package1">>, <<"1.0.0">>, r3_hex_core:default_config()),
%% > {ok, #{metadata := Metadata}} = r3_hex_tarball:unpack(Tarball, memory).
%% '''
%% @end
get_tarball(Config, Name, Version) ->
    URI = maps:get(repo_url, Config),
    ReqHeaders = make_headers(Config),

    case get(Config, tarball_url(URI, Name, Version), ReqHeaders) of
        {ok, {200, RespHeaders, Tarball}} ->
            {ok, {200, RespHeaders, Tarball}};

        Other ->
            Other
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get(Config, URI, Headers) ->
    r3_hex_http:request(Config, get, URI, Headers, undefined).

get_protobuf(Config, Path, Decoder) ->
    PublicKey = maps:get(repo_public_key, Config),
    ReqHeaders = make_headers(Config),

    case get(Config, build_url(Path, Config), ReqHeaders) of
        {ok, {200, RespHeaders, Compressed}} ->
            Signed = zlib:gunzip(Compressed),
            case decode(Signed, PublicKey, Decoder, Config) of
                {ok, Decoded} ->
                    {ok, {200, RespHeaders, Decoded}};

                {error, _} = Error ->
                    Error
            end;

        Other ->
            Other
    end.

decode(Signed, PublicKey, Decoder, Config) ->
    Verify = maps:get(repo_verify, Config, true),

    case Verify of
        true ->
            case r3_hex_registry:decode_and_verify_signed(Signed, PublicKey) of
                {ok, Payload} ->
                    Decoder(Payload);
                Other ->
                    Other
            end;
        false ->
            #{payload := Payload} = r3_hex_registry:decode_signed(Signed),
            Decoder(Payload)
    end.

tarball_url(URI, Name, Version) ->
    Filename = tarball_filename(Name, Version),
    <<URI/binary, "/tarballs/", Filename/binary>>.

build_url(Path, #{repo_url := URI}) ->
    <<URI/binary, Path/binary>>.

tarball_filename(Name, Version) ->
    <<Name/binary, "-", Version/binary, ".tar">>.

make_headers(Config) ->
    maps:fold(fun set_header/3, #{}, Config).

set_header(http_etag, ETag, Headers) when is_binary(ETag) -> maps:put(<<"if-none-match">>, ETag, Headers);
set_header(repo_key, Token, Headers) when is_binary(Token) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) -> Headers.
