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

-module(emq_policy_server_util_format).

%% include
-include_lib("emqttd/include/emqttd.hrl").

-export([parser_app_by_clientId/1, parser_device_by_clientId/1, parser_username_by_clientId/1, validate_boolean/1, validate_clientId/2]).

%%　client : $client/{$app_id}/{$device}/{$username}/
parser_app_by_clientId(ClientId) ->
  ClientSplit = string:tokens(binary_to_list(ClientId), "/"),
  Len = erlang:length(ClientSplit),
  if
    Len >= 2 ->
      lists:nth(2, ClientSplit);
    true ->
      ""
  end.

%%　client : $client/{$app_id}/{$device}/{$username}/
parser_device_by_clientId(ClientId) ->
  ClientSplit = string:tokens(binary_to_list(ClientId), "/"),
  Len = erlang:length(ClientSplit),
  if
    Len >= 3 ->
      lists:nth(3, ClientSplit);
    true ->
      ""
  end.

parser_username_by_clientId(ClientId) ->
  ClientSplit = string:tokens(binary_to_list(ClientId), "/"),
  Len = erlang:length(ClientSplit),
  if
    Len >= 4 ->
      lists:nth(4, ClientSplit);
    true ->
      ""
  end.

%%　validate clientId format
validate_clientId(ClientId, Username) ->
  ClientSplit = string:tokens(binary_to_list(ClientId), "/"),
  Len = erlang:length(ClientSplit),
  if
    Len >= 4 ->
      string:equal(binary_to_list(ClientId), "$client/" ++ lists:nth(2, ClientSplit) ++ "/" ++ lists:nth(3, ClientSplit) ++ "/" ++ binary_to_list(Username) ++ "/");
    true ->
      false
  end.

validate_boolean(1) ->
  true;
validate_boolean(<<"1">>) ->
  true;
validate_boolean(<<"true">>) ->
  true;
validate_boolean(true) ->
  true;
validate_boolean(_Value) ->
  false.

