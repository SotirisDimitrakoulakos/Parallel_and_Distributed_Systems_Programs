-module(monitor).
-export([start/0]).

start() ->
    pid = spawn(fun() -> double:start() end),
    spawn(fun() -> MRef = erlang:monitor(process, pid),
        receive
            {'DOWN', MRef, process, _pid, _Reason} -> demonitor(MRef), start()
        end
    end).
