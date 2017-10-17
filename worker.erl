-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
		stop -> 
			ok
	end.

peers(Wrk, Peers) ->
   	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, LogicalT)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
        	%io:format("Receive ~n"),
         	LTimeInc= time:inc(Name,time:merge(LogicalT,Time)), % Each time a new message is sent we increment its time so that the ones before gets prioritized.
            Log ! {log, Name, LTimeInc, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, LTimeInc);
		stop -> 
			ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
    	%io:format("Send ~n"),
        Selected = select(Peers),
        Time = LogicalT,
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter), % If this jitter function comes after we set the log in the next line then we wont have any problem
		Log ! {log, Name, Time, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, time:inc(Name,Time))
	end.

%The selection of which peer to send a message to. This selection is done randomly
select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).


% slight delay between sending the message to the peer and informing the logger
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).