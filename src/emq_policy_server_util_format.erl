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

-export([validate_boolean/1, format_from/1, replace_str/3]).

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
  {From, From};
format_from(_) ->
  {<<>>, <<>>}.

a2b(A) -> atom_to_binary(A, utf8).

replace_str(Str, Find, New) ->
  try
    Total = string:len(Str),
    Len = string:len(Find),
    Index = string:str(Str, Find),
    First = string:substr(Str, 1, Index - 1),
    End = string:substr(Str, Index + Len, Total - Index - Len + 1),
    string:concat(string:concat(First, New), End)
  catch
    throw:_Term ->
      Str;
    exit:_Reason ->
      Str;
    error:_Reason ->
      Str
  end;
replace_str(Str, _Find, _New) ->
  Str.

