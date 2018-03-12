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

-module(emq_persist_redis_app).

-behaviour(application).

-include("emq_persist_redis.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_persist_redis_sup:start_link(),
    %% ok = emqttd_access_control:register_mod(auth, emq_auth_demo, []),
    %% ok = emqttd_access_control:register_mod(acl, emq_acl_demo, []),
    %% emq_persist_redis_config:register(),
    %% application:get_all_env(Application) -> [{Par :: atom(), Val :: term()}].
    emq_persist_redis:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    %% ok = emqttd_access_control:unregister_mod(auth, emq_auth_demo),
    %% ok = emqttd_access_control:unregister_mod(acl, emq_acl_demo),
    %% emq_auth_redis_config:unregister().
    emq_persist_redis:unload(application:get_all_env()).
