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

-module(emq_policy_server_module_hook).

%% include
-include("emq_policy_server.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emq_policy_server_base_app, [parser_app_by_client/1, parser_device_by_client/1, validate_client_format/2]).
-import(emq_policy_server_base_http, [request/3, env_http_request/0]).

-export([load/1, unload/0]).

%% hooks
-export([hook_client_connected/3, hook_client_disconnected/3]).
-export([hook_message_publish/2, hook_message_delivered/4, hook_message_ack/4]).

load(Env) ->
  emqttd:hook('client.connected', fun ?MODULE:hook_client_connected/3, [Env]),
  emqttd:hook('client.disconnected', fun ?MODULE:hook_client_disconnected/3, [Env]),
  emqttd:hook('message.publish', fun ?MODULE:hook_message_publish/2, [Env]),
  emqttd:hook('message.delivered', fun ?MODULE:hook_message_delivered/4, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:hook_message_ack/4, [Env]).
unload() ->
  emqttd:unhook('client.connected', fun ?MODULE:hook_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:hook_client_disconnected/3),
  emqttd:unhook('message.publish', fun ?MODULE:hook_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:hook_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:hook_message_ack/4).

%% hook client connected
hook_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  io:format("hook log (client.connected):~nclient ~s connected, connack: ~w~n=====================================================~n", [ClientId, ConnAck]),
  request_connect_hook(Client, env_http_request(), client_connected),
  {ok, Client}.

%% hook client connected
hook_client_disconnected(Reason, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  io:format("hook log (client.disconnected):~nclient ~s disconnected, reason: ~w~n=====================================================~n", [ClientId, Reason]),
  request_connect_hook(Client, env_http_request(), client_disconnected),
  ok.

%% transform message and return
hook_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

hook_message_publish(Message, _Env) ->
  io:format("hook log (message.publish):~npublish ~s~n=====================================================~n", [emqttd_message:format(Message)]),
  request_message_publish_hook(Message, env_http_request(), message_publish),
  {ok, Message}.

hook_message_delivered(ClientId, Username, Message, _Env) ->
%%   io:format("hook log (message.delivered):~ndelivered to client(~s/~s): ~s~n=====================================================~n", [Username, ClientId, emqttd_message:format(Message)]),
%%   request_message_delivered_ask_hook(ClientId, Username, Message, env_http_request(), message_delivered),
  {ok, Message}.

%% hook message ask
hook_message_ack(ClientId, Username, Message, _Env) ->
%%   io:format("hook log (message.acked):~nclient(~s/~s) acked: ~s~n=====================================================~n", [Username, ClientId, emqttd_message:format(Message)]),
%%   request_message_delivered_ask_hook(ClientId, Username, Message, env_http_request(), message_acked),
  {ok, Message}.

request_connect_hook(_Client = #mqtt_client{username = Username, client_id = ClientId}, _ApiReq = #http_request{method = Method, url = Url, params = Params, server_id = ServerId}, Action) ->
  Mod = hook,
  case request(Method, Url, connected_params_val(Params, Mod, Action, ServerId, ClientId, Username)) of
    {ok, 200, _Body} ->
      ok;
    {ok, _Code, _Body} ->
      error;
    {error, _Error} ->
      error
  end.

request_message_publish_hook(Message, _ApiReq = #http_request{method = Method, url = Url, params = Params, server_id = ServerId}, Action) ->
  Mod = hook,
  case request(Method, Url, message_publish_params_val(Params, Mod, Action, ServerId, Message)) of
    {ok, 200, _Body} ->
      ok;
    {ok, _Code, _Body} ->
      error;
    {error, _Error} ->
      error
  end.

%%request_message_delivered_ask_hook(ClientId, Username, Message, _ApiReq = #http_request{method = Method, url = Url, params = Params, server_id = ServerId}, Action) ->
%%  Mod = hook,
%%  case request(Method, Url, message_delivered_ask_params_val(Params, Mod, Action, ServerId, ClientId, Username, Message)) of
%%    {ok, 200, _Body} ->
%%      ok;
%%    {ok, _Code, _Body} ->
%%      error;
%%    {error, _Error} ->
%%      error
%%  end.

%%--------------------------------------------------------------------
%% Params
%%--------------------------------------------------------------------

connected_params_val(Params, Mod, Action, ServerId, ClientId, Username) ->
  lists:map(fun
              ({Param, "%mod"}) -> {Param, Mod};
              ({Param, "%act"}) -> {Param, Action};
              ({Param, "%sid"}) -> {Param, ServerId};
              ({Param, "%cid"}) -> {Param, ClientId};
              ({Param, "%aid"}) -> {Param, parser_app_by_client(ClientId)};
              ({Param, "%device"}) -> {Param, parser_device_by_client(ClientId)};
              ({Param, "%user"}) -> {Param, Username};
              ({Param, "%pass"}) -> {Param, ""};
              ({Param, "%topic"}) -> {Param, ""};
              ({Param, "%payload"}) -> {Param, ""};
              (Param) -> Param
            end, Params).

message_publish_params_val(Params, Mod, Action, ServerId, _Message = #mqtt_message{topic = Topic, payload = Payload, from = {ClientId, Username}}) ->
  lists:map(fun
              ({Param, "%mod"}) -> {Param, Mod};
              ({Param, "%act"}) -> {Param, Action};
              ({Param, "%sid"}) -> {Param, ServerId};
              ({Param, "%cid"}) -> {Param, ClientId};
              ({Param, "%aid"}) -> {Param, parser_app_by_client(ClientId)};
              ({Param, "%device"}) -> {Param, parser_device_by_client(ClientId)};
              ({Param, "%user"}) -> {Param, Username};
              ({Param, "%pass"}) -> {Param, ""};
              ({Param, "%topic"}) -> {Param, Topic};
              ({Param, "%payload"}) -> {Param, Payload};
              (Param) -> Param
            end, Params).

%%message_delivered_ask_params_val(Params, Mod, Action, ServerId, ClientId, Username, _Message = #mqtt_message{topic = Topic, payload = Payload}) ->
%%  lists:map(fun
%%              ({Param, "%mod"}) -> {Param, Mod};
%%              ({Param, "%act"}) -> {Param, Action};
%%              ({Param, "%sid"}) -> {Param, ServerId};
%%              ({Param, "%cid"}) -> {Param, ClientId};
%%              ({Param, "%aid"}) -> {Param, parser_app_by_client(ClientId)};
%%              ({Param, "%device"}) -> {Param, parser_device_by_client(ClientId)};
%%              ({Param, "%user"}) -> {Param, Username};
%%              ({Param, "%pass"}) -> {Param, ""};
%%              ({Param, "%topic"}) -> {Param, Topic};
%%              ({Param, "%payload"}) -> {Param, Payload};
%%              (Param) -> Param
%%            end, Params).