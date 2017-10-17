-module(time).
-export([zero/0,inc/2,merge/2,leq/2,clock/1,update/3,safe/2]).


% return an initial Lamport value (could it be 0)
zero() ->
	0.

% return the time T incremented by one (you will probably ignore the Name but we will use it later)
inc(_, T) -> 
	T+1.

% merge the two Lamport time stamps (i.e. take the maximum value)
merge(Ti, Tj) -> 
	erlang:max(Ti,Tj).

% true if Ti is less than or equal to Tj
leq(Ti,Tj) -> 
	if 
		Ti =< Tj -> 
			true;
		true -> 
			false
	end.


% return a clock that can keep track of the nodes
% the clock function goes through all the workers and sets their clock to 0 
clock(Nodes) -> 
	List = lists:foldl(fun(Node,Clock) -> [{Node, 0}| Clock ] end, [], Nodes),
    io:format("~w~n", [List]),
    List.

% return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
	case lists:keyfind(Node,1,Clock) of
		{Node, CurrentT} ->
			RestClock = lists:keydelete(Node,1,Clock),
			[{Node, time:merge(Time,CurrentT)} | RestClock];
		false ->
			[{Node, Time} | Clock]
    end.


% is it safe to log an event that happened at a given time, true or false
safe(Time, NodeT) -> 
	leq(Time,NodeT).