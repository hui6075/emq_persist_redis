emq-persist-redis
===================

This is a plugin for the EMQ broker to store mqtt clients logging info, PUBLISH messages.

Plugin Config
-------------

etc/emq_persist_redis.conf

### Redis server configure
persist.redis.server = 127.0.0.1:6379
persist.redis.database = 0
persist.redis.password = ""

### Logging LIST(per client) length
persist.redis.log_list_length = 10

### PUBLISH LIST(per topic) length
persist.redis.pub_list_length = 100

### client SET name
persist.redis.client_set = client_set

Redis Table
------------
### Logging LIST:
```
------------------------------------------------------------------------------------
|  127.0.0.1:6379> LRANGE client1 0 -1                                             |
|  1) "DISCONNECTED at 2018-03-12 11:01:14 Reason:   normal"                       |
|  2) "UNSUBSCRIBE  at 2018-03-12 11:01:12 Topic(s): {test} {test1} {test2}"       |
|  3) "SUBSCRIBE    at 2018-03-12 11:01:12 Topic(s): {test|0} {test1|1} {test2|2}" |
|  4) "CONNECTED    at 2018-03-12 11:01:09 Result:   Connection accepted"          |
------------------------------------------------------------------------------------
```
### PUBLISH LIST:
```
----------------------------------------------------------------------------------------------
|  127.0.0.1:6379> LRANGE sensor 0 -1                                                        |
|  1) "2018-03-12 11:02:43 PUBLISH Message(Q2, R0, D0, PktId:3, From:client1, Payload:msg3)" |
|  2) "2018-03-12 11:02:42 PUBLISH Message(Q2, R0, D0, PktId:2, From:client1, Payload:msg2)" |
|  3) "2018-03-12 11:02:40 PUBLISH Message(Q2, R0, D0, PktId:1, From:client1, Payload:msg1)" |
----------------------------------------------------------------------------------------------
```
### client SET:
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
