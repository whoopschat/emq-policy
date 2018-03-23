%%%--------------------------------------------------------------------------------
%% The MIT License (MIT)
%%
%% Copyright (c) 2017 WhoopsChat
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/ or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%%--------------------------------------------------------------------------------

-module(emq_policy_server_util_http).

-include("emq_policy_server.hrl").

-import(emq_policy_server_util_logger, [log/2]).

-export([request/3, requestSync/3, env_http_request/0]).

env_http_request() ->
  Config = application:get_env(emq_policy_server, api, undefined),
  Method = proplists:get_value(method, Config, post),
  Url = proplists:get_value(url, Config),
  ServerKey = proplists:get_value(server_key, Config),
  #http_request{url = Url, method = Method, server_key = ServerKey}.

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------
request(get, Url, Params) ->
  ibrowse:send_req(Url, [], get, [],[]),
  Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
  {ok, RequestId} = httpc:request(get, Req, [{autoredirect, true}], [{sync, false}]),
  receive {http, {RequestId, _Result}} -> ok after 0.01 -> ok end,
  receive {http, {RequestId, {error, _Reason}}} -> ok after 0.01 -> ok end,
  ok;
request(post, Url, Params) ->
  Ibrowse = ibrowse:send_req(Url, [{"Content-Type", "application/x-www-form-urlencoded"}], post, mochiweb_util:urlencode(Params)),
  log("~nibrowse: ~p~n=====================================================~n", [Ibrowse]),
  Req = {Url, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)},
  {ok, RequestId} = httpc:request(post, Req, [{autoredirect, true}], [{sync, false}]),
  receive {http, {RequestId, _Result}} -> ok after 0.01 -> ok end,
  receive {http, {RequestId, {error, _Reason}}} -> ok after 0.01 -> ok end,
  ok.

requestSync(get, Url, Params) ->
  Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
  reply_response(httpc:request(get, Req, [{autoredirect, true}], []));

requestSync(post, Url, Params) ->
  Req = {Url, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)},
  reply_response(httpc:request(post, Req, [{autoredirect, true}], [])).

reply_response({ok, {{_, Code, _}, _Headers, Body}}) ->
  {ok, Code, Body};
reply_response({ok, Code, Body}) ->
  {ok, Code, Body};
reply_response({error, Error}) ->
  {error, Error}.




