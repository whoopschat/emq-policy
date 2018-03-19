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

-module(emq_policy_server_app_auth).

-behaviour(emqttd_auth_mod).

%% include
-include("whoopschat_emq_policy.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emq_policy_server_base_app, [parser_app_by_client/1, parser_device_by_client/1, validate_system_format/2, validate_client_format/2]).
-import(emq_policy_server_base_binary2, [trimBOM/1]).
-import(emq_policy_server_base_http, [request/3, env_http_request/0]).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

%% Callbacks
-export([init/1, check/3, description/0]).

init(Env) ->
  {ok, Env}.

check(#mqtt_client{username = Username}, Password, _Env) when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
  {error, username_or_password_undefined};
check(Client = #mqtt_client{username = Username, client_id = ClientId}, Password, _Env) ->
  IsClient = validate_client_format(ClientId, Username),
  IsSystem = validate_system_format(ClientId, Username),
  if
    IsClient ->
      ApiReq = env_http_request(),
      request_client_auth(Client, Password, ApiReq);
    IsSystem ->
      ApiReq = env_http_request(),
      request_system_auth(Client, Password, ApiReq);
    true ->
      {error, "ClientId format error"}
  end.

request_client_auth(Client = #mqtt_client{username = Username, client_id = ClientId}, Password, ApiReq = #http_request{method = Method, url = Url, params = Params, server_id = ServerId}) ->
  Mod = auth,
  Action = user,
  case request(Method, Url, user_params_val(Params, Mod, Action, ServerId, ClientId, Username, Password)) of
    {ok, 200, Body} ->
      Json = trimBOM(list_to_binary(Body)),
      IsJson = jsx:is_json(Json),
      if
        IsJson ->
          BodyObj = jsx:decode(Json),
          {_, Code} = lists:keyfind(<<"code">>, 1, BodyObj),
          {_, Msg} = lists:keyfind(<<"msg">>, 1, BodyObj),
          if
            Code == 1 ->
              IsSuper = is_superuser(Client, ApiReq),
              io:format("auth client log[success]:~ncode ~p,msg ~p,super ~p~n=====================================================~n", [Code, Msg, IsSuper]),
              {ok, IsSuper};
            true ->
              io:format("auth client log[error]:~ncode ~p,msg ~p~n=====================================================~n", [Code, Msg]),
              {error, Msg}
          end;
        true ->
          io:format("auth client log[error]:~nauth return json format error~n=====================================================~n"),
          {error, "auth return json error"}
      end;
    {ok, Code, _Body} ->
      {error, Code};
    {error, Error} ->
      {error, Error}
  end.

request_system_auth(Client = #mqtt_client{username = Username, client_id = ClientId}, Password, ApiReq = #http_request{method = Method, url = Url, params = Params, server_id = ServerId}) ->
  Mod = auth,
  Action = system,
  case request(Method, Url, user_params_val(Params, Mod, Action, ServerId, ClientId, Username, Password)) of
    {ok, 200, Body} ->
      Json = trimBOM(list_to_binary(Body)),
      IsJson = jsx:is_json(Json),
      if
        IsJson ->
          BodyObj = jsx:decode(Json),
          {_, Code} = lists:keyfind(<<"code">>, 1, BodyObj),
          {_, Msg} = lists:keyfind(<<"msg">>, 1, BodyObj),
          if
            Code == 1 ->
              IsSuper = is_superuser(Client, ApiReq),
              io:format("auth system log[success]:~ncode ~p,msg ~p,super ~p~n=====================================================~n", [Code, Msg, IsSuper]),
              {ok, IsSuper};
            true ->
              io:format("auth system log[error]:~ncode ~p,msg ~p~n=====================================================~n", [Code, Msg]),
              {error, Msg}
          end;
        true ->
          io:format("auth system log[error]:~nauth return json format error~n=====================================================~n"),
          {error, "auth return json error"}
      end;
    {ok, Code, _Body} ->
      {error, Code};
    {error, Error} ->
      {error, Error}
  end.

description() -> "Emq Policy Server AUTH module".

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | #http_request{}, mqtt_client()) -> boolean()).
is_superuser(_Client, undefined) ->
  false;
is_superuser(#mqtt_client{username = Username, client_id = ClientId}, #http_request{method = Method, url = Url, params = Params, server_id = ServerId}) ->
  Mod = auth,
  Action = super,
  case request(Method, Url, super_user_params_val(Params, Mod, Action, ServerId, ClientId, Username)) of
    {ok, 200, Body} ->
      Json = trimBOM(list_to_binary(Body)),
      IsJson = jsx:is_json(Json),
      if
        IsJson ->
          BodyObj = jsx:decode(Json),
          {_, Code} = lists:keyfind(<<"code">>, 1, BodyObj),
          if
            Code == 1 -> true;
            true -> false
          end;
        true ->
          false
      end;
    {ok, _Code, _Body} ->
      false;
    {error, _Error} ->
      false
  end.

%%--------------------------------------------------------------------
%% Params
%%--------------------------------------------------------------------

user_params_val(Params, Mod, Action, ServerId, ClientId, Username, Password) ->
  lists:map(fun
              ({Param, "%mod"}) -> {Param, Mod};
              ({Param, "%act"}) -> {Param, Action};
              ({Param, "%sid"}) -> {Param, ServerId};
              ({Param, "%cid"}) -> {Param, ClientId};
              ({Param, "%aid"}) -> {Param, parser_app_by_client(ClientId)};
              ({Param, "%device"}) -> {Param, parser_device_by_client(ClientId)};
              ({Param, "%user"}) -> {Param, Username};
              ({Param, "%pass"}) -> {Param, Password};
              ({Param, "%topic"}) -> {Param, ""};
              ({Param, "%payload"}) -> {Param, ""};
              (Param) -> Param
            end, Params).

super_user_params_val(Params, Mod, Action, ServerId, ClientId, Username) ->
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