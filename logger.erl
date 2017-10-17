-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).
stop(Logger) ->
   Logger ! stop.


init(Nodes) ->
	InitClock = time:clock(Nodes),
	HoldBackQueue = [],
    loop(InitClock, HoldBackQueue).

loop(NodeClock, HoldBackQueue) ->
    receive
        {log, From, Time, Msg} ->
        	UNodeClock = time:update(From, Time, NodeClock),
        	SortedQueue = addQueue(From, Time, Msg, HoldBackQueue),
        	UHoldBackQueue = flush(UNodeClock, SortedQueue, length(SortedQueue)),
            %log(From, Time, Msg),
            loop(UNodeClock,UHoldBackQueue);
        stop ->
            % io:format(HoldBackQueue),
			ok
	end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

% add the current Msg to the queue and then sort the queue based on Time
addQueue(From, Time, Msg, HBQueue) ->
    %Queue = lists:keysort(2,[{From, Time, Msg}| HBQueue]), 
    Queue = lists:keysort(2, lists:append(HBQueue,[{From, Time, Msg}])), 
    Queue.

flush(UNodeClock, Queue, 1) ->
    Clock = lists:keysort(2,UNodeClock),
    [{From, Time, Msg}] = Queue,
    [{_, NodeTime} | _] = Clock,
    case time:safe(Time,NodeTime) of
    	false ->
	        Queue;
	    true ->
	    	log(From, Time, Msg),
	    	io:format("Queue Empty ~n"),
        	[]
     end;

flush(UNodeClock, Queue, Length) -> 
	Clock = lists:keysort(2,UNodeClock), % sort the nodes based on the their updated clock
	[{From, Time, Msg} | RestQueue] = Queue, % First one in line 
	[{_, NodeTime} | _] = Clock, % getting the node time 
	case time:safe(Time,NodeTime) of % if safe -> print and continue to the next one in line. Safe means that the message's time is lower or equal to the local node time. Which determines that the node has not yet gotten the message so we print
	    false ->
	        Queue;
	     true ->
	        log(From, Time, Msg),
	        flush(Clock, RestQueue, Length-1)
     end.




