-module(barrier).
-export([start/1, wait/2]).

start(Refs) ->
    spawn_link(fun () -> loop(lists:sort(Refs), [], []) end).

loop(RefList, Waiting, Holding) when Holding =:= RefList ->
    [Pid ! {continue, Ref} || {Pid, Ref} <- Waiting],
    loop(RefList, [], []);

loop(RefList, Waiting, Holding) ->
    receive
        {arrive, {Pid, Ref}} ->
            case lists:member(Ref, RefList) of
                true ->
                    loop(RefList, [{Pid, Ref}|Waiting], lists:sort([Ref|Holding]));
                false ->
                    Pid ! {continue, Ref},
                    loop(RefList, Waiting, Holding)
            end
    end.

wait(Barrier, Ref) ->
    Barrier ! {arrive, {self(), Ref}},
    receive
	{continue, Ref} ->
	    ok
    end.