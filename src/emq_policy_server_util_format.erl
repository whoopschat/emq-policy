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

-export([parser_app_by_clientId/1, parser_device_by_clientId/1, parser_username_by_clientId/1, validate_boolean/1, validate_clientId/2, format_from/1,replace_str/3]).

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

format_from({ClientId, Username}) ->
  {ClientId, Username};
format_from(From) when is_atom(From) ->
  {a2b(From), a2b(From)};
format_from(From) when is_binary(From) ->
  {From, parser_username_by_clientId(From)};
format_from(_) ->
  {<<>>, <<>>}.

a2b(A) -> atom_to_binary(A, utf8).

replace_str(Str, Find, New) when is_binary(Str);is_binary(Find);is_binary(New) ->
  First = string:str(Str, Find),
  Len = string:len(Find),
  End = First + Len,
  string:concat(string:concat(string:substr(Str, 1, Len), New), string:substr(Str, End, string:len(Str) - End));
replace_str(Str, _Find, _New) ->
  Str.

