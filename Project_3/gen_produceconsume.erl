-module(gen_produceconsume).
-export([start/2, stop/1,  produce/2, consume/1]).
-callback handle_produce(Input :: term()) -> {ok, Task :: term()}.
-callback handle_consume(Task :: term()) -> ok.

start(Callback, T) -> 
    buffer = {Callback, [], T},
    spawn(fun() -> pc_bufferloop(buffer) end).

stop(Pid) -> exit(Pid, kill).

produce(Pid, T) ->
    Pid ! {produce, self()},
    receive
        {Cb, Q, Max} ->
            case length(Q)==Max of
                true -> 
                    pc_bufferloop({Cb, Q, Max});
                false ->
                    {ok, Task} = Cb:handle_produce(T),
                    pc_bufferloop({Cb, Q ++ [Task], Max})
            end
    end.

consume(Pid) ->
    Pid ! {consume, self()},
    receive
        {Cb, Q, Max} -> 
            case Q of
                [] ->
                    pc_bufferloop({Cb, Q, Max});
                [H|T] ->
                    ok = Cb:handle_consume(H),
                    pc_bufferloop({Cb, T, Max})
            end
    end.


pc_bufferloop({Cb, Q, Max}) ->
    receive
        {produce, Pid} -> Pid ! {Cb, Q, Max};
        {consume, Pid} -> Pid ! {Cb, Q, Max}
    end.

