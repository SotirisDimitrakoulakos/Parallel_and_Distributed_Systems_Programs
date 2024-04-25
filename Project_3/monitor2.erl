-module(monitor).
-behaviour(supervisor).
-export([start_link/0, init/1, double/0]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, 
    [#{id => double_id, start => {double, start_link, []}}]}}.

double() ->
    double:start().
