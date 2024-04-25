-module(ring).
-export([start/2, ring_process/3, message_round/0]).

start(N, M) ->
    Processes = lists:map(fun(_) -> spawn(?MODULE, message_round, []) end, lists:seq(1, N)),
    ring_process(M, 0, Processes).

ring_process(0, Message, Processes) ->
    [exit(Pid, kill) || Pid <- Processes], 
    Message;

ring_process(M, Message, [First|Rest]) ->
    First ! {Message, Rest, self()},
    receive
        mess -> ring_process((M-1), mess, [First|Rest])
    end.

message_round() ->
    receive
        {Message, [], Ring_Pid} -> Ring_Pid ! (Message + 1);
        {Message, [First|Rest], Ring_Pid} -> First ! {(Message + 1), Rest, Ring_Pid}
    end,
    message_round().