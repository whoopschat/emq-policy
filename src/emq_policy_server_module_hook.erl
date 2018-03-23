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

-import(emq_policy_server_util_format, [parser_app_by_clientId/1, parser_device_by_clientId/1, parser_username_by_clientId/1, validate_clientId/2, replace_str/3, format_from/1]).
-import(emq_policy_server_util_http, [requestSync/3, env_http_request/0]).
-import(emq_policy_server_util_logger, [errorLog/2, debugLog/2]).
-import(emq_policy_server_util_binary, [trimBOM/1]).

-export([load/1, unload/0]).

%% hooks
-export([hook_client_subscribe/4, hook_client_unsubscribe/4]).
-export([hook_client_connected/3, hook_client_disconnected/3]).
-export([hook_message_publish/2, hook_message_delivered/4, hook_message_ack/4]).

load(Env) ->
  emqttd:hook('client.subscribe', fun ?MODULE:hook_client_subscribe/4, [Env]),
  emqttd:hook('client.unsubscribe', fun ?MODULE:hook_client_unsubscribe/4, [Env]),
  emqttd:hook('client.connected', fun ?MODULE:hook_client_connected/3, [Env]),
  emqttd:hook('client.disconnected', fun ?MODULE:hook_client_disconnected/3, [Env]),
  emqttd:hook('message.publish', fun ?MODULE:hook_message_publish/2, [Env]),
  emqttd:hook('message.delivered', fun ?MODULE:hook_message_delivered/4, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:hook_message_ack/4, [Env]).

unload() ->
  emqttd:unhook('client.subscribe', fun ?MODULE:hook_client_subscribe/4),
  emqttd:unhook('client.unsubscribe', fun ?MODULE:hook_client_unsubscribe/4),
  emqttd:unhook('client.connected', fun ?MODULE:hook_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:hook_client_disconnected/3),
  emqttd:unhook('message.publish', fun ?MODULE:hook_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:hook_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:hook_message_ack/4).

%%--------------------------------------------------------------------
%% Client Hook
%%--------------------------------------------------------------------

hook_client_subscribe(ClientId, Username, TopicTable, _Env) ->
  debugLog("~nclient log (client.subscribe):~nclient(~s/~s) will subscribe: ~p~n=====================================================~n", [Username, ClientId, TopicTable]),
  {ok, TopicTable}.

hook_client_unsubscribe(ClientId, Username, TopicTable, _Env) ->
  debugLog("~nclient log (client.unsubscribe):~nclient(~s/~s) unsubscribe ~p~n=====================================================~n", [ClientId, Username, TopicTable]),
  {ok, TopicTable}.

%% hook client connected
hook_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  debugLog("~nclient log (client.connected):~nclient ~s connected, connack: ~w~n=====================================================~n", [ClientId, ConnAck]),
  request_connect_hook(Client, client_connected, env_http_request()),
  {ok, Client}.

%% hook client connected
hook_client_disconnected(Reason, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  debugLog("~nclient log (client.disconnected):~nclient ~s disconnected, reason: ~w~n=====================================================~n", [ClientId, Reason]),
  request_connect_hook(Client, client_disconnected, env_http_request()),
  ok.

%%--------------------------------------------------------------------
%% Message Hook
%%--------------------------------------------------------------------

%% transform message and return
hook_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

hook_message_publish(Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  debugLog("~nmessage log (message.publish):~npublish ~s~n=====================================================~n", [emqttd_message:format(Message)]),
  {FromClientId, FromUsername} = format_from(Message#mqtt_message.from),
  request_message_hook(Topic, Payload, FromClientId, FromUsername, message_publish, env_http_request()),
  {ok, Message}.

%% hook message delivered
hook_message_delivered(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  debugLog("~nmessage log (message.delivered):~ndelivered to client(~s/~s): ~s~n=====================================================~n", [Username, ClientId, emqttd_message:format(Message)]),
  request_message_hook(Topic, Payload, ClientId, Username, message_delivered, env_http_request()),
  {ok, Message}.

%% hook message ask
hook_message_ack(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  debugLog("~nmessage log (message.acked):~nclient(~s/~s) acked: ~s~n=====================================================~n", [Username, ClientId, emqttd_message:format(Message)]),
  request_message_hook(Topic, Payload, ClientId, Username, message_ask, env_http_request()),
  {ok, Message}.

%%--------------------------------------------------------------------
%% Request Hook
%%--------------------------------------------------------------------

request_connect_hook(#mqtt_client{username = Username, client_id = ClientId}, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = client,
  Params = [
    {server_key, ServerKey}
    , {app_id, parser_app_by_clientId(ClientId)}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    debugLog("~naction: ~p~nCode: ~p~nBody: ~p~n=====================================================~n", [Action, Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientId, Username, Json);
      true ->
        error
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n=====================================================~n", [Action, Error]),
      error
  end.

request_message_hook(Topic, Payload, ClientId, Username, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = message,
  Params = [
    {server_key, ServerKey}
    , {app_id, parser_app_by_clientId(ClientId)}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
    , {topic, Topic}
    , {payload, Payload}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    debugLog("~naction: ~p~nCode: ~p~nBody: ~p~n=====================================================~n", [Action, Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientId, Username, Json);
      true ->
        error
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n=====================================================~n", [Action, Error]),
      error
  end.

handle_request_result(ClientId, Username, Json) ->
  JSONBody = jsx:decode(Json),
  case lists:keyfind(<<"sub_list">>, 1, JSONBody) of {_, SubList} ->
    handleResultSub(ClientId, Username, SubList);
    _ ->
      true
  end,
  case lists:keyfind(<<"un_sub_list">>, 1, JSONBody) of {_, UnSubList} ->
    handleResultUnSub(ClientId, Username, UnSubList);
    _ ->
      true
  end,
  case lists:keyfind(<<"pub_list">>, 1, JSONBody) of {_, PubList} ->
    handleResultPub(ClientId, Username, PubList);
    _ ->
      true
  end,
  ok.

handleResultPub(ClientId, _Username, PubList) when is_list(PubList) ->
  try
    lists:map(fun(Pub) ->
      if is_list(Pub) ->
        Len = erlang:length(Pub),
        if
          Len >= 2 ->
            Topic = lists:nth(1, Pub),
            Payload = lists:nth(2, Pub),
            Msg = emqttd_message:make(ClientId, 1, Topic, Payload),
            emqttd:publish(Msg#mqtt_message{retain = true}),
            ok;
          true ->
            error
        end;
        true -> error end,
      Pub end, PubList)
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok;
handleResultPub(_, _, _) ->
  ok.

handleResultSub(ClientId, Username, SubList) when is_list(SubList) ->
  try
    Client = emqttd_cm:lookup(ClientId),
    ClientPid = Client#mqtt_client.client_pid,
    TopicTable = [{handleTopic(Topic, ClientId, Username), 1} || Topic <- SubList],
    ClientPid ! {subscribe, TopicTable}
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok;
handleResultSub(_, _, _) ->
  ok.

handleResultUnSub(ClientId, Username, UnSubList) when is_list(UnSubList) ->
  try
    Client = emqttd_cm:lookup(ClientId),
    ClientPid = Client#mqtt_client.client_pid,
    Topics = [handleTopic(Topic, ClientId, Username) || Topic <- UnSubList],
    ClientPid ! {unsubscribe, Topics}
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok;
handleResultUnSub(_, _, _) ->
  ok.

handleTopic(Topic, ClientId, Username) ->
  try
    FixUsername = replace_str(binary_to_list(Topic), ":username", binary_to_list(Username)),
    FixAppId = replace_str(FixUsername, ":app_id", parser_app_by_clientId(ClientId)),
    list_to_binary(FixAppId)
  catch
    throw:_Term ->
      Topic;
    exit:_Reason ->
      Topic;
    error:_Reason ->
      Topic
  end.