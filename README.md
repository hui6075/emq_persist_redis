emq-persist-redis
===================

This is a plugin for the EMQ broker to store mqtt clients logging info, PUBLISH messages.

Usage(Require Erlang/OTP 19+)
-------------
\> git clone https://github.com/emqtt/emq-relx.git </br>
\> cd emq-relx </br>
\> vi Makefile </br>
```
DEPS += emq_persist_redis
dep_emq_persist_redis = git https://github.com/hui6075/emq_persist_redis master
```
\> make </br>
\> cd _rel/emqttd && ./bin/emqttd console </br>

Plugin Config
-------------

\> vi etc/emq_persist_redis.conf </br>
```
## Redis server configure
persist.redis.server = 127.0.0.1:6379
persist.redis.database = 0
persist.redis.password = ""

## Logging LIST(per client) length
persist.redis.log_list_length = 10

## PUBLISH LIST(per topic) length
persist.redis.pub_list_length = 100

## client SET name
persist.redis.client_set = client_set
```
Redis Table
------------
##### Logging LIST:
```
-------------------------------------------------------------------------------------
|  127.0.0.1:6379> LRANGE client1 0 -1                                              |
|  1) "DISCONNECTED at 2018-03-13 19:01:14 Reason:   normal"                        |
|  2) "UNSUBSCRIBE  at 2018-03-13 11:31:53 Topic(s): {topic4|local}"                |
|  3) "SUBSCRIBE    at 2018-03-13 11:31:51 Topic(s): {topic4|0|local}"              |
|  4) "UNSUBSCRIBE  at 2018-03-13 11:20:08 Topic(s): {topic3|$queue}"               |
|  5) "SUBSCRIBE    at 2018-03-13 11:20:07 Topic(s): {topic3|0|$queue}"             |
|  6) "UNSUBSCRIBE  at 2018-03-13 11:19:40 Topic(s): {topic2|$share|group3}"        |
|  7) "SUBSCRIBE    at 2018-03-13 11:19:39 Topic(s): {topic2|0|$share|group3}"      |
|  8) "UNSUBSCRIBE  at 2018-03-12 11:01:12 Topic(s): {test0} {test1} {test2}"       |
|  9) "SUBSCRIBE    at 2018-03-12 11:01:12 Topic(s): {test0|0} {test1|1} {test2|2}" |
| 10) "CONNECTED    at 2018-03-12 11:01:09 Result:   Connection accepted"           |
-------------------------------------------------------------------------------------
```
##### PUBLISH LIST:
```
----------------------------------------------------------------------------------------------
|  127.0.0.1:6379> LRANGE sensor 0 -1                                                        |
|  1) "2018-03-12 11:02:43 PUBLISH Message(Q2, R0, D0, PktId:3, From:client1, Payload:msg3)" |
|  2) "2018-03-12 11:02:42 PUBLISH Message(Q2, R0, D0, PktId:2, From:client1, Payload:msg2)" |
|  3) "2018-03-12 11:02:40 PUBLISH Message(Q2, R0, D0, PktId:1, From:client1, Payload:msg1)" |
----------------------------------------------------------------------------------------------
```
##### client SET:
```
---------------------------------------------
|  127.0.0.1:6379> SMEMBERS mqtt_client_set |
|  1) "client1"                             |
|  2) "client2"                             |
---------------------------------------------
```

Plugin and Hooks
-----------------

[Plugin Design](http://docs.emqtt.com/en/latest/design.html#plugin-design)

[Hooks Design](http://docs.emqtt.com/en/latest/design.html#hooks-design)

License
-------

Apache License Version 2.0
