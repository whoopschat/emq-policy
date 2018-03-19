%%%--------------------------------------------------------------------------------
%% The MIT License (MIT)
%%
%% Copyright (c) 2017 Whoopschat
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

-module(emq_policy_server_module_group).

%% include
-include("whoopschat_emq_policy.hrl").
-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").

-export([load/1, unload/0]).

-import(emq_policy_server_base_app, [parser_app_by_client/1, parser_device_by_client/1, validate_client_format/2]).
-import(emq_policy_server_base_http, [request/3, env_http_request/0]).
-import(emq_policy_server_base_binary, [trimBOM/1]).

%% hooks
-export([on_message_ack/4, on_client_connected/3]).

load(Env) ->
  emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:on_message_ack/4, [Env]).
unload() ->
  emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
  emqttd:unhook('message.acked', fun ?MODULE:on_message_ack/4).

%% message ask
on_message_ack(ClientId, Username, Message, _Env) ->
  IsClient = validate_client_format(ClientId, Username),
  if
    IsClient ->
      Client = emqttd_cm:lookup(ClientId),
      handle_ask_group_subscribe(Message, Client),
      true;
    true ->
      false
  end,
  {ok, Message}.

%% on client connected
on_client_connected(_ConnAck, Client = #mqtt_client{username = Username, client_id = ClientId}, _Env) ->
  IsClient = validate_client_format(ClientId, Username),
  if
    IsClient ->
      handle_connect_group_subscribe(env_http_request(), Client),
      true;
    true ->
      false
  end,
  {ok, Client}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%　handle ask group subscribe : handle
handle_ask_group_subscribe(Message = #mqtt_message{topic = <<"$command/", _/binary>>, payload = Payload}, _Client = #mqtt_client{username = Username, client_id = ClientId, client_pid = ClientPid}) ->
  Topic = Message#mqtt_message.topic,
  SubAsk = validate_group_subscribe_asked(Topic, ClientId, Username),
  UnSubAsk = validate_group_un_subscribe_asked(Topic, ClientId, Username),
  if
    SubAsk ->
      TopicTable = [{list_to_binary("$group/" ++ parser_app_by_client(ClientId) ++ "/+/" ++ binary_to_list(Payload) ++ "/"), 1}],
      ClientPid ! {subscribe, TopicTable},
      true;
    UnSubAsk ->
      Topics = [list_to_binary("$group/" ++ parser_app_by_client(ClientId) ++ "/+/" ++ binary_to_list(Payload) ++ "/")],
      ClientPid ! {unsubscribe, Topics},
      true;
    true ->
      false
  end,
  ok;
handle_ask_group_subscribe(_Message, _Client) -> ok.

%% handle connect group subscribe
handle_connect_group_subscribe(#http_request{method = Method, url = Url, params = Params, server_id = ServerId}, #mqtt_client{username = Username, client_id = ClientId, client_pid = ClientPid}) ->
  Mod = group,
  Action = list,
  case request(Method, Url, group_params_val(Params, Mod, ServerId, ClientId, Username, Action)) of
    {ok, 200, Body} -> subscribe_group_list(ClientPid, ClientId, Body), ok;
    {ok, _Code, _Body} -> error;
    {error, _Error} -> error
  end.

subscribe_group_list(ClientPid, ClientId, Body) ->
  Json = trimBOM(list_to_binary(Body)),
  IsJson = jsx:is_json(Json),
  if
    IsJson ->
      BodyObj = jsx:decode(Json),
      {_, Code} = lists:keyfind(<<"code">>, 1, BodyObj),
      if
        Code == 1 ->
          {_, Data} = lists:keyfind(<<"data">>, 1, BodyObj),
          AppKey = parser_app_by_client(ClientId),
          GroupTopic = fun(GroupID) ->
            list_to_binary("$group/" ++ AppKey ++ "/+/" ++ binary_to_list(GroupID) ++ "/") end,
          TopicTable = [{GroupTopic(GroupID), 1} || GroupID <- Data],
          ClientPid ! {subscribe, TopicTable},
          ok;
        true -> error
      end;
    true ->
      error
  end.

%%　validate group-subscribe cmd
validate_group_subscribe_asked(Topic, ClientId, Username) ->
  string:equal(binary_to_list(Topic), "$command/" ++ parser_app_by_client(ClientId) ++ "/group-subscribe/" ++ binary_to_list(Username) ++ "/").

%%　validate group-unsubscribe cmd
validate_group_un_subscribe_asked(Topic, ClientId, Username) ->
  string:equal(binary_to_list(Topic), "$command/" ++ parser_app_by_client(ClientId) ++ "/group-unsubscribe/" ++ binary_to_list(Username) ++ "/").

%% group params val %%
group_params_val(Params, Mod, ServerId, ClientId, Username, Action) ->
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
