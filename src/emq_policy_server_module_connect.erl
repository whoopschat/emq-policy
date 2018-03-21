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

-module(emq_policy_server_module_connect).

%% include
-include("emq_policy_server.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).

-import(emq_policy_server_base_app, [parser_app_by_client/1, validate_client_account/2]).

%% hooks
-export([on_client_connected/3, on_client_disconnected/3]).

load(Env) ->
  emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
  emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]).
unload() ->
  emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
  emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3).

%% on client connected
on_client_connected(_ConnAck, Client = #mqtt_client{username = Username, client_id = ClientId, client_pid = ClientPid}, _Env) ->
  IsClient = validate_client_account(ClientId, Username),
  if
    IsClient ->
      handle_connect_subscribe(ClientId, ClientPid, Username),
      true;
    true ->
      false
  end,
  {ok, Client}.

%% on client disconnected
on_client_disconnected(_Reason, _Client = #mqtt_client{client_id = _ClientId}, _Env) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% handle connect subscribe
handle_connect_subscribe(_ClientId, _ClientPid, undefined) -> ok;
handle_connect_subscribe(ClientId, ClientPid, Username) ->
  PrivateTopic = list_to_binary("$private/" ++ parser_app_by_client(ClientId) ++ "/+/" ++ binary_to_list(Username) ++ "/"),
  CommandTopic = list_to_binary("$command/" ++ parser_app_by_client(ClientId) ++ "/+/" ++ binary_to_list(Username) ++ "/"),
  TopicTable = [{PrivateTopic, 1}, {CommandTopic, 1}],
  ClientPid ! {subscribe, TopicTable},
  ok.

