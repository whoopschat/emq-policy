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

-module(emq_policy_server_app_acl).

-behaviour(emqttd_acl_mod).

%% include
-include("emq_policy_server.hrl").
-include_lib("emqttd/include/emqttd.hrl").

%% Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).


init(Env) ->
  {ok, Env}.

check_acl({_Client, _PubSub, _Topic}, _Env) ->
%%  Access = access(PubSub),
  deny.

reload_acl(_State) -> ok.

%%access(subscribe) -> 1;
%%access(publish) -> 2.

description() -> "Emq Policy Server ACL module".