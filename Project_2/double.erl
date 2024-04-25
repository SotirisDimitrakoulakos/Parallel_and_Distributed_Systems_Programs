-module(double).
-export([start/0, double/0]).

start() ->
    pid = spawn(?MODULE, double, []),
    register(double, pid),
    pid.

double() ->
    try
        receive
            {Pid, Ref, N} when is_number(N) -> Pid ! {Ref, N * 2}
        end(),
        double()
    catch
        throw:Reason ->
            %io:format("Excetion: ~p~n", [Reason]),
            double()
    end.