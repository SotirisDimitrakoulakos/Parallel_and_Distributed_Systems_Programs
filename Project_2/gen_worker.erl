-module(gen_worker).
-export([start/2, stop/1, async/2, await/1, await_all/1, await_all/2]).
-callback handle_work(Value :: term()) -> {result, Result :: term()} | no_result.

start(Callback, Max) -> spawn(fun() -> pids = [spawn_work(Callback) || _ <- lists:seq(1, Max)], loop(pids, 1) end).

loop(pids, I) ->
    receive
        {pidA, Ref, {schedule, W}} -> pid = list:nth(I, pids), pid ! {pidA, Ref , W}, pidA ! {Ref}, loop(pids, (I rem length(pids)) + 1);
        {_pidA, _Ref, stop} -> lists:foreach(fun(P) -> P ! {self(), 0, stop} end, pids), exit(normal)
    end.

spawn_work(Callback) ->
    receive
        {_pid, _Ref, stop} -> exit(normal);
        {pid, Ref, W} ->
            case (catch Callback:handle_work(W)) of
                {result, Result} -> pid ! {Ref, {result, Result}};
                no_result -> pid ! {Ref, no_result};
                error -> pid ! {Ref, error};
                {'EXIT', _Reason} -> pid ! {Ref, error}
            end,
            spawn_work(Callback)
    end.

stop(pid) -> pid ! {self(), 0, stop}.

async(pid, W) -> R = make_ref(), pid ! {self(), R, {schedule, W}},
    receive
        {R} -> R
    end.

await(Ref) ->
    receive
        {Ref, Result} -> Result
    end.

await() ->
    receive
        {_Ref, Result} -> Result
    end.

await_all(Refs) when is_list(Refs) -> await_all(Refs, []);

await_all(Length) when is_number(Length) -> await_all(Length, []).

await_all(0, Acc) -> Acc;

await_all(Length, Acc) ->
    case await() of
        {result, Result} -> await_all(Length-1, [Result|Acc]);
        _ -> await_all(Length-1, Acc)
    end.