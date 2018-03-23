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

-module(emq_policy_server_module_acl).

-behaviour(emqttd_acl_mod).

%% include
-include("emq_policy_server.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emq_policy_server_util_format, [parser_app_by_clientId/1, parser_device_by_clientId/1, validate_clientId/2]).
-import(emq_policy_server_util_logger, [log/2]).

%% Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

init(Env) ->
  {ok, Env}.

check_acl({Client = #mqtt_client{client_id = ClientId}, PubSub, Topic}, _Env) ->
  log("acl log  : ~p ~p ~p~n=====================================================~n", [Client, PubSub, Topic]),
  access(PubSub, ClientId, Topic).

reload_acl(_State) -> ok.

access(subscribe, ClientId, Topic) ->
  log("subscribe log (subscribe.acl):~nclientId:~s topic: ~s)~n=====================================================~n", [ClientId, Topic]),
  deny;
access(publish, ClientId, Topic) ->
  log("publish log (publish.acl):~nclientId:~s topic: ~s)~n=====================================================~n", [ClientId, Topic]),
  App = parser_app_by_clientId(ClientId),
  IsTopic = string:str(binary_to_list(Topic), "$" ++ App ++ "/") > 0,
  if
    IsTopic ->
      allow;
    true ->
      deny
  end.

description() -> "Emq Policy Server ACL module".