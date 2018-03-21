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

-import(emq_policy_server_base_app, [parser_app_by_clientId/1, parser_device_by_clientId/1, validate_clientId/2]).
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
  emqttd:hook('message.acked', fun ?MODULE:hook_message_ack/4, [Env]),
  receiveHook().
unload() ->
  emqttd:unhook('client.connected', fun ?MODULE:hook_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:hook_client_disconnected/3),
  emqttd:unhook('message.publish', fun ?MODULE:hook_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:hook_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:hook_message_ack/4).

receiveHook() ->
  receive {http, {RequestId, _Result}} ->
    io:format("RequestId ~s~n", [RequestId]),
    ok end,
  ok;

%%--------------------------------------------------------------------
%% Client Hook
%%--------------------------------------------------------------------

%% hook client connected
hook_client_connected(ConnAck, Client = #mqtt_client{username = Username, client_id = ClientId, client_pid = ClientPid}, _Env) ->
  io:format("hook log (client.connected):~nclient ~s connected, connack: ~w~n=====================================================~n", [ClientId, ConnAck]),
  request_connect_hook(Client, client_connected, env_http_request()),
  IsClient = validate_clientId(ClientId, Username),
  if
    IsClient ->
      handle_connect_subscribe(ClientId, ClientPid, Username),
      true;
    true ->
      false
  end,
  {ok, Client}.

%% hook client connected
hook_client_disconnected(Reason, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  io:format("hook log (client.disconnected):~nclient ~s disconnected, reason: ~w~n=====================================================~n", [ClientId, Reason]),
  request_connect_hook(Client, client_disconnected, env_http_request()),
  ok.

%% handle connect subscribe
handle_connect_subscribe(_ClientId, _ClientPid, undefined) -> ok;
handle_connect_subscribe(ClientId, ClientPid, Username) ->
  PrivateTopic = list_to_binary("$" ++ parser_app_by_clientId(ClientId) ++ "/+/" ++ binary_to_list(Username) ++ "/"),
  CommandTopic = list_to_binary("$" ++ parser_app_by_clientId(ClientId) ++ "/command/+/" ++ binary_to_list(Username) ++ "/"),
  TopicTable = [{PrivateTopic, 1}, {CommandTopic, 1}],
  ClientPid ! {subscribe, TopicTable},
  ok.

%%--------------------------------------------------------------------
%% Message Hook
%%--------------------------------------------------------------------

%% transform message and return
hook_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

hook_message_publish(Message = #mqtt_message{topic = Topic, payload = Payload, from = {ClientId, Username}}, _Env) ->
  io:format("hook log (message.publish):~npublish ~s~n=====================================================~n", [emqttd_message:format(Message)]),
  request_message_hook(Topic, Payload, ClientId, Username, message_publish, env_http_request()),
  {ok, Message}.

hook_message_delivered(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  io:format("hook log (message.delivered):~ndelivered to client(~s/~s): ~s~n=====================================================~n", [Username, ClientId, emqttd_message:format(Message)]),
  request_message_hook(Topic, Payload, ClientId, Username, message_delivered, env_http_request()),
  {ok, Message}.

%% hook message ask
hook_message_ack(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  io:format("hook log (message.acked):~nclient(~s/~s) acked: ~s~n=====================================================~n", [Username, ClientId, emqttd_message:format(Message)]),
  request_message_hook(Topic, Payload, ClientId, Username, message_delivered, env_http_request()),
  {ok, Message}.

%%--------------------------------------------------------------------
%% Request Hook
%%--------------------------------------------------------------------

request_connect_hook(#mqtt_client{username = Username, client_id = ClientId}, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = hook,
  Params = [
    {server_key, ServerKey}
    , {mod, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
  ],
  request(Method, Url, Params).

request_message_hook(Topic, Payload, ClientId, Username, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = hook,
  Params = [
    {server_key, ServerKey}
    , {mod, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
    , {topic, Topic}
    , {payload, Payload}
  ],
  request(Method, Url, Params).