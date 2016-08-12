# erldog
DataDog agent with full implementation in erlang OTP Application

Website: http://posilva.github.io/erldog/

## Counter

    application:ensure_all_started(erldog).
    C = erldog_api:create_counter("com.test.counter", 15).
    erldog_api:send_metric(C).

## Gauge

    application:ensure_all_started(erldog).
    G = erldog_api:create_gauge("com.test.gauge", 77).
    erldog_api:send_metric(G).

## Histogram

    application:ensure_all_started(erldog).
    H = erldog_api:create_histogram("com.test.histogram", 80).
    erldog_api:send_metric(H).

## Set

    application:ensure_all_started(erldog).
    S = erldog_api:create_set("com.test.set", 17).
    erldog_api:send_metric(S).

## Multiple values
To send multiple metric's values in a time use `create_points/1` to prepare our data:
    
    [P1] = erldog_api:create_points(17).
    timer:sleep(100),
    [P2] = erldog_api:create_points(22).
    H = erldog_api:create_histogram("com.test.histogram", [P1, P2]).
    erldog_api:send_metric(H).
        
## Multiple metrics
To send multiple metrics in one request - just pass a list to `erldog_api:send_metric/1`

    application:ensure_all_started(erldog).
    H = erldog_api:create_histogram("com.test.histogram", 80).
    G = erldog_api:create_gauge("com.test.gauge", 77).
    erldog_api:send_metric([H, G]).
    

== Planned Features ==

1. Support all API V1 Calls
2. Support aggregation with update in a configured interval
3. Support store mechanism for aggregation to avoid client application side accumulators (you do not need to keep the counter on your side, just call increment(V) or decrement(V) with the storage active )
4. Support Cache persistence mechanism to be failure resilient and  no loose data

  

 
