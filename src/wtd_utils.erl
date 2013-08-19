%%% @author     Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       utils for wtd
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordonguthrie@backawinner.gg

-module(wtd_utils).

-export([
         get_root_dir/0,
         proxy_rec_to_list/1,
         sign_string/2,
         make_http_req/3,
         get_ack/0
        ]).

-include("wtd.hrl").

-define(MEGA, 1000000000000).
-define(SEC,  1000000).

get_root_dir() ->
    filename:dirname(code:where_is_file("erlang_wtd.app")) ++ "/../".

proxy_rec_to_list(Rec) when is_record(Rec, proxy) ->
    	lists:zip(record_info(fields, proxy), tl(tuple_to_list(Rec))).

sign_string(PrivateKey, String) ->
    Sig = xmerl_ucs:to_utf8(String),
    binary_to_list(base64:encode(crypto:sha_mac(PrivateKey, Sig))).

make_http_req(Proxy, Path, Body) ->
    #proxy{domain      = Domain,
           epmd_port   = EPMDPort,
           wtd_node    = {PublicKey, _Name},
           private_key = PrivateKey} = Proxy,
    URL = "http://" ++ Domain ++ ":" ++ integer_to_list(EPMDPort),
    Method = post,
    MD5 = binary_to_list(crypto:md5(term_to_binary(Body))),
    ContentMD5 = {"content-md5", MD5},
    {ContentType, Headers} = standard_content_type_and_hdrs([ContentMD5]),
    HTTPAuthHeader = hmac_api_lib:sign(PrivateKey, PublicKey, Method, Path,
                                       Headers, ContentType),
    Request = {URL ++ Path, [HTTPAuthHeader | Headers], ContentType, Body},
    httpc:request(Method, Request, [], []).

get_ack() ->
    {Mega, Sec, Micro} = now(),
    "ack" ++ integer_to_list(?MEGA * Mega + ?SEC * Sec + Micro).

%%%===================================================================
%%% Internal functions
%%%===================================================================
standard_content_type_and_hdrs(Hdrs) when is_list(Hdrs) ->
    Date = dh_date:format("D, j M Y H:i:s"),
    Headers = [{"content-type", "application/json"},
               {"date",         Date},
               {"accept",       "application/json"}],
    ContentType = "application/json",
    {ContentType, Hdrs ++ Headers}.

