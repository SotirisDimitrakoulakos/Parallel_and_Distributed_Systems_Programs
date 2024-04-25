-module(allocator).
-export([start/1, request/2, release/2]).

start(Resources = #{}) ->
    spawn_link(fun () -> allocator(Resources) end).

request(Pid, R) ->
    Ref = make_ref(),
    Pid ! {request, {self(), Ref, R}},
    receive
        {granted, Ref, Resources} ->
            Resources;
        {wait, Ref, resource_not_found} ->
            request(Pid, R)
    end.

release(Pid, R) ->
    Ref = make_ref(),
    Pid ! {release, {self(), Ref, R}},
    receive
        {released, Ref} ->
            ok
    end.

allocator(Resources) ->
    receive
        {request, {Pid, Ref, N}} when N =< length(Resources) ->
            case getResources(Resources, N, #{}) of
                {ok, NotR, Req} -> Pid ! {granted, Ref, Req}, allocator(NotR);
                {error,resource_not_found,_} -> Pid ! {wait,Ref, resource_not_found}, allocator(Resources)
            end;
        {release, {Pid, Ref, Released}} -> Pid ! {released, Ref}, allocator(Released ++ Resources)
    end.

getResources(AllResources, [RName|T], Found) ->
	Resource = maps:find(RName, AllResources),
	case Resource of
        {ok, R} ->
            NFound = maps:put(RName, R, Found),
            NAll = maps:remove(RName, AllResources),
            getResources(NAll, T, NFound);
        error ->
            {error, resource_not_found, RName}
	end;

getResources(AllResources, [], Found) -> {ok, AllResources, Found}.