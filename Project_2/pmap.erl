-module(pmap).
-behaviour(gen_worker).
-export([handle_work/1, unordered/2, unordered/3, ordered/3]).

handle_work({Fun, V}) -> {result, Fun(V)}.

unordered(Fun, L) ->
    WorkPool = gen_worker:start(?MODULE, length(L)), [gen_worker:async(WorkPool, {Fun, V}) || V <- L],
    Result = gen_worker:awaint_all(length(L)), gen_worker:stop(WorkPool),
    Result.

unordered(Fun, L, MaxWorkers) ->
    Max = case MaxWorkers >= length(L) of
            true ->length(L);
            false -> MaxWorkers
          end,
    WorkPool = gen_worker:start(?MODULE, Max), [gen_worker:async(WorkPool, {Fun, V}) || V <- L],
    Result = gen_worker:awaint_all(length(L)), gen_worker:stop(WorkPool),
    Result.

ordered(Fun, L, MaxWorkers) -> 
    Max = case MaxWorkers >= length(L) of
            true -> length(L);
            false -> MaxWorkers
          end,
    WorkPool = gen_worker:start(?MODULE, Max),
    Refs = [gen_worker:async(WorkPool, {Fun, V}) || V <- L],
    Result = gen_worker:await_all(Refs),
    gen_worker:stop(workPool),
    Result.