-module(test).
-export([run/2]).



%report on your initial observations



run(Sleep, Jitter) ->
    % we start by creating the logging process and four workers. 
    Log = logger:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),

    % When the workers have been created we send them a message with their peers.
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),

    timer:sleep(5000),

    logger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).


%%-------------------------------------------------%%

% How do you know that they are printed in the wrong order? 