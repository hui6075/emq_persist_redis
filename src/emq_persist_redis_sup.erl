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

-module(emq_persist_redis_sup).

-behaviour(supervisor).

-include("emq_persist_redis.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% pool_spec(ChildId, Pool, Mod, Opts)
init([]) ->
    {ok, Server} = application:get_env(?APP, server),
    PoolSpec = ecpool:pool_spec(?APP, ?APP, emq_persist_redis_client, Server),
    {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.