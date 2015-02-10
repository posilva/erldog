# erldog
DataDog agent with full implementation in erlang OTP Application

Website: http://posilva.github.io/erldog/

== Planned Features ==

1. Support all API V1 Calls
2. Support aggregation with update in a configured interval
3. Support store mechanism for aggregation to avoid client application side accumulators (I do not need to keep the counter on your side, just call increment(V) or decrement(V) with the storage active )
4. Support Cache persistence mechanism to be failure resilient and  no loose data

  

 
