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

-module(emq_policy_server_module_auth).

-behaviour(emqttd_auth_mod).

%% include
-include("emq_policy_server.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emq_policy_server_base_app, [validate_clientId/2]).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

%% Callbacks
-export([init/1, check/3, description/0]).

init(Env) ->
  {ok, Env}.

check(#mqtt_client{username = Username}, Password, _Env) when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
  {error, username_or_password_undefined};
check(Client = #mqtt_client{username = Username, client_id = ClientId}, Password, _Env) ->
  IsClient = validate_clientId(ClientId, Username),
  if
    IsClient ->
      {ok, false};
    true ->
      {error, "ClientId Format Error"}
  end.

description() -> "Emq Policy Server AUTH module".