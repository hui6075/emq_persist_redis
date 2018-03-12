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

-module(emq_persist_redis).

-include_lib("emqttd/include/emqttd.hrl").

-include("emq_persist_redis.hrl").

-export([load/1, unload/1]).

%% Hooks functions

-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_client_subscribe/4, on_client_unsubscribe/4]).

-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4, on_session_terminated/4]).

-export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

%% Called when the plugin application start
load(Env) ->
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
    emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
    emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]),
    %% emqttd:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
    %% emqttd:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
    %% emqttd:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
    %% emqttd:hook('session.terminated', fun ?MODULE:on_session_terminated/4, [Env]),
    {_, PubListLen} = lists:keyfind(pub_list_length, 1, Env),
    if
        PubListLen>0 ->
            emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]);
        true ->
            ok
    end.
    %% emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
    %% emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]).

%% Hook functions.
on_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, Env) ->
    {_, LogListLen} = lists:keyfind(log_list_length, 1, Env),
    Info = "CONNECTED    at " ++ log_time() ++ " Result:   " ++ get_connect_result(ConnAck),
    %% store client connect info
    ConnCmd = ["LPUSH", binary_to_list(ClientId), Info],
    emq_persist_redis_client:store_logging(ConnCmd),
    TrimCmd = ["LTRIM", binary_to_list(ClientId), 0, LogListLen-1],
    emq_persist_redis_client:store_logging(TrimCmd),
    %% store client id to mqtt_client_set
    {_, ClientSet} = lists:keyfind(client_set, 1, Env),
    ClientSetCmd = ["SADD", ClientSet, binary_to_list(ClientId)],
    emq_persist_redis_client:store_logging(ClientSetCmd),
    {ok, Client}.

on_client_disconnected(Reason, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
    Info = "DISCONNECTED at " ++ log_time() ++ " Reason:   " ++ io_lib:format("~s", [Reason]),
    DisconnCmd = ["LPUSH", binary_to_list(ClientId), Info],
    emq_persist_redis_client:store_logging(DisconnCmd),
    ok.

on_client_subscribe(ClientId, _Username, TopicTable, _Env) ->
    Info = "SUBSCRIBE    at " ++ log_time() ++ " Topic(s):" ++ io_lib:format("~s", [format_sub_table(TopicTable)]),
    SubCmd = ["LPUSH", binary_to_list(ClientId), Info],
    emq_persist_redis_client:store_logging(SubCmd),
    {ok, TopicTable}.

on_client_unsubscribe(ClientId, _Username, TopicTable, _Env) ->
    Info = "UNSUBSCRIBE  at " ++ log_time() ++ " Topic(s):" ++ io_lib:format("~s", [format_unsub_table(TopicTable)]),
    UnsubCmd = ["LPUSH", binary_to_list(ClientId), Info],
    emq_persist_redis_client:store_logging(UnsubCmd),
    {ok, TopicTable}.

on_session_created(ClientId, Username, _Env) ->
    io:format("session(~s/~s) created.", [ClientId, Username]).

on_session_subscribed(ClientId, Username, {Topic, Opts}, _Env) ->
    io:format("session(~s/~s) subscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
    {ok, {Topic, Opts}}.

on_session_unsubscribed(ClientId, Username, {Topic, Opts}, _Env) ->
    io:format("session(~s/~s) unsubscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
    ok.

on_session_terminated(ClientId, _Username, Reason, _Env) ->
    Info = "TERMINATED   at " ++ log_time() ++ " Reason:   " ++ io_lib:format("~s", [Reason]),
    TermCmd = ["LPUSH", binary_to_list(ClientId), Info],
    emq_persist_redis_client:store_logging(TermCmd).

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message = #mqtt_message{topic = Topic, payload = _Payload}, Env) ->
    {_, PubListLen} = lists:keyfind(pub_list_length, 1, Env),
    %% Info = log_time() ++ "PUBLISH: " ++ binary_to_list(Payload),
    Info = log_time() ++ " PUBLISH " ++ format_msg(Message),
    %% store PUBLISH msg
    PubCmd = ["LPUSH", binary_to_list(Topic), Info],
    emq_persist_redis_client:store_logging(PubCmd),
    %% limit list length
    TrimCmd = ["LTRIM", binary_to_list(Topic), 0, PubListLen-1],
    emq_persist_redis_client:store_logging(TrimCmd),
    {ok, Message}.

on_message_delivered(ClientId, Username, Message, _Env) ->
    io:format("delivered to client(~s/~s): ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

on_message_acked(ClientId, Username, Message, _Env) ->
    io:format("client(~s/~s) acked: ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

%% Called when the plugin application stop
unload(Env) ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
    emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
    emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4),
    %% emqttd:unhook('session.created', fun ?MODULE:on_session_created/3),
    %% emqttd:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
    %% emqttd:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
    %% emqttd:unhook('session.terminated', fun ?MODULE:on_session_terminated/4),
    {_, PubListLen} = lists:keyfind(pub_list_length, 1, Env),
    if
        PubListLen>0 ->
            emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2);
        true ->
            ok
    end.
    %% emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4),
    %% emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/4).

%% Helper
log_time() ->
    {{Year,Mon,Day},{Hour,Min,Sec}} = calendar:now_to_local_time(os:timestamp()),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",[Year, Mon, Day, Hour, Min, Sec]).

%% MQTT Connect Return Codes
%% 0: CONNACK_ACCEPT,       Connection accepted
%% 1: CONNACK_PROTO_VER,    Unacceptable protocol version
%% 2: CONNACK_INVALID_ID,   Client Identifier is correct UTF-8 but not allowed by the Server
%% 3: CONNACK_SERVER,       Server unavailable
%% 4: CONNACK_CREDENTIALS,  Username or password is malformed
%% 5: CONNACK_AUTH,         Client is not authorized to connect
get_connect_result(ConnAck) ->
    case ConnAck of
        0 -> "Connection accepted";
        1 -> "Unacceptable protocol version";
        2 -> "Client Identifier is correct UTF-8 but not allowed by the Server";
        3 -> "Server unavailable";
        4 -> "Username or password is malformed";
        5 -> "Client is not authorized to connect";
        _ -> "Reject by unknown reason"
    end.

%% type of subscription TopicTable: [{<<topic1>>, [{qos, Qos1}|Opts]}, {<<topic2>>, [{qos, Qos2}|Opts]}]
format_sub_table(TopicTable) ->
    lists:map(fun({Top, [Term]}) ->
                      case Term of
                          {qos, QoS} ->
                              " {" ++ binary_to_list(Top) ++ "|" ++ integer_to_list(QoS) ++ "}";
                          _Opts ->
                              " {" ++ binary_to_list(Top) ++ "}"
                      end
              end, TopicTable).

%% type of unsubscription TopicTable: [{<<topic1>>,[]},{<<topic2>>,[]}]
format_unsub_table(TopicTable) ->
    lists:map(fun({Top, _Opt}) -> " {" ++ binary_to_list(Top) ++ "}" end, TopicTable).

%% -type(mqtt_msg_from() :: atom() | {binary(), undefined | binary()}).
format_msg(#mqtt_message{pktid = PktId, from = {ClientId, undefined},
                     qos = Qos, retain = Retain, dup = Dup, payload =Payload}) ->
    io_lib:format("Message(Q~p, R~p, D~p, PktId:~p, From:~s, Payload:~s)",
                  [i(Qos), i(Retain), i(Dup), PktId, ClientId, Payload]);

format_msg(#mqtt_message{pktid = PktId, from = {ClientId, Username},
                     qos = Qos, retain = Retain, dup = Dup, payload =Payload}) ->
    io_lib:format("Message(Q~p, R~p, D~p, PktId:~p, From:~s, Username:~s, Payload:~s)",
                  [i(Qos), i(Retain), i(Dup), PktId, ClientId, Username, Payload]);

format_msg(#mqtt_message{pktid = PktId, from = From,
                     qos = Qos, retain = Retain, dup = Dup, payload =Payload}) ->
    io_lib:format("Message(Q~p, R~p, D~p, PktId:~p, From:~s, Payload:~s)",
                  [i(Qos), i(Retain), i(Dup), PktId, From, Payload]).

i(true)  -> 1;
i(false) -> 0;
i(I) when is_integer(I) -> I.