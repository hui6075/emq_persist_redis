%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_persist_redis_client).

-behaviour(ecpool_worker).

-define(ENV(Key, Opts), proplists:get_value(Key, Opts)).

-include("emq_persist_redis.hrl").

-export([connect/1, store_logging/1]).

%%--------------------------------------------------------------------
%% Redis Connect/Query
%% connect() is callback of ecpool_worker.
%%--------------------------------------------------------------------

connect(Opts) ->
    eredis:start_link(?ENV(host, Opts),
                      ?ENV(port, Opts),
                      ?ENV(database, Opts),
                      ?ENV(password, Opts),
                      no_reconnect).

%% Redis Query.
-spec(store_logging(string()) -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
store_logging(CmdStr) ->
    ecpool:with_client(?APP, fun(RedisClient) -> eredis:q_noreply(RedisClient, CmdStr) end).

%% ecpool:with_client() will returns which query fun return.
%% -spec q_async(Client::client(), Command::[any()]) -> ok.
%% see: https://github.com/wooga/eredis/blob/master/src/eredis.erl