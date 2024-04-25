-module(mapreduce).
-export([mapreduce/5, mapreduce/6, test/0, test_distributed/0]).

mapreduce(Mapper, Mappers, Reducer, Reducers, Input) ->
    Master = self(), Ref = make_ref(),
    Partitions = partition(Mappers, Input),
    ReducerPids = [spawn_reducer(Master, Ref, Reducer) || _ <- lists:seq(1, Reducers)],        
    MapperPids = [spawn_mapper(Master, Ref, Mapper, ReducerPids, Part) || Part <- Partitions],  
    [receive 
        {mapped, {Pid, Ref}} -> ok 
     end || Pid <- MapperPids], [R ! {reduce, Master, Ref} || R <- ReducerPids],                                    
    Output = [receive
                {reduced, {Pid, Ref, Data}} -> Data
              end || Pid <- ReducerPids], lists:sort(lists:flatten(Output)).

mapreduce(Nodes, Mapper, Mappers, Reducer, Reducers, Input) ->
    Master = self(), Ref = make_ref(),
    Partitions = partition(Mappers, Input),
    ReducerPids = distribute_reducers(Nodes, Reducer, Reducers, [], {Master, Ref}),
    MapperPids = distribute_mappers(Nodes, Mapper, Partitions, ReducerPids, [], {Master, Ref}),
    [receive 
        {mapped, {Pid, Ref}} -> ok 
     end || Pid <- MapperPids], [R ! {reduce, Master, Ref} || R <- ReducerPids],
    Output = [receive
                {reduced, {Pid, Ref, Data}} -> Data
              end || Pid <- ReducerPids], lists:sort(lists:flatten(Output)).

distribute_reducers(_, _, 0, ReducerPids, _) -> ReducerPids;

distribute_reducers([H|T], Reducer, Reducers, ReducerPids, {Master, Ref}) ->
    distribute_reducers(T ++ [H], Reducer, Reducers - 1, [spawn_reducer(H, Master, Ref, Reducer)|ReducerPids], {Master, Ref}).

distribute_mappers(_, _, [], _, MapperPids, _) -> MapperPids;

distribute_mappers([H|T], Mapper, [Part|Rest], ReducerPids, MapperPids, {Master, Ref}) ->
    distribute_mappers(T ++ [H], Mapper, Rest, ReducerPids, [spawn_mapper(H, Master, Ref, Mapper, ReducerPids, Part)|MapperPids], {Master, Ref}).

spawn_mapper(Master, Ref, Mapper, ReducerPids, Data) -> spawn_link(fun () -> mapper(Master, Ref, Mapper, ReducerPids, Data) end).

spawn_mapper(Node, Master, Ref, Mapper, ReducerPids, Data) -> spawn_link(Node, fun () -> mapper(Master, Ref, Mapper, ReducerPids, Data) end).

mapper(Master, Ref, Mapper, Reducers, Data) ->
    Map = [{erlang:phash2(MapKey, length(Reducers)), {MapKey, MapValue}} || {DataKey, DataValue} <- Data,{MapKey, MapValue} <- Mapper(DataKey, DataValue)],
    [lists:nth(Phash, Reducers) ! {map_data, Ref, Element} || {Phash, Element} <- Map], Master ! {mapped, {self(), Ref}}.

spawn_reducer(Master, Ref, Reducer) -> spawn_link(fun () -> reducer(Master, Ref, Reducer, []) end).

spawn_reducer(Node, Master, Ref, Reducer) -> spawn_link(Node, fun () -> reducer(Master, Ref, Reducer, []) end).

reducer(Master, Ref, Reducer, Data) ->
    receive
        {map_data, Ref, MapperData} ->
            reducer(Master, Ref, Reducer, [MapperData | Data]);    
        {reduce, Master, Ref} ->                                   
            Reduced = [KV || {K,Vs} <- groupkeys(lists:sort(Data)), KV <- Reducer(K,Vs)],
		    Master ! {reduced, {self(), Ref, Reduced}}
    end.

groupkeys([]) -> [];

groupkeys([{K, V}|Rest]) -> groupkeys(K, [V], Rest).

groupkeys(K, Vs, [{K, V}|Rest]) -> groupkeys(K, [V|Vs], Rest);

groupkeys(K, Vs, Rest) -> [{K, lists:reverse(Vs)}|groupkeys(Rest)].

partition(N, L) -> partition(N, L, length(L)).

partition(1, L, _) -> [L];

partition(N, L, Len) -> {Prefix, Suffix} = lists:split(Len div N, L), [Prefix | partition(N - 1, Suffix, Len - (Len div N))].

test() ->
    Mapper = fun (_Key, Text) -> [{Word, 1} || Word <- Text] end, 
    Reducer = fun (Word, Counts) -> [{Word, lists:sum(Counts)}] end,
    mapreduce(Mapper, 2, Reducer, 10, [{a, ["sotiris", "dimitrakoulakos", "stockholm", "university"]}, {b, ["paradis", "assignment", "4", "erlang"]}]).

test_distributed() ->
    Mapper = fun (_Key, Text) -> [{Word, 1} || Word <- Text] end, 
    Reducer = fun (Word, Counts) -> [{Word, lists:sum(Counts)}] end,
    LocalHost = net_adm:localhost(),
    {ok, LocalHosts} = net_adm:names(),
    Nodes = [list_to_atom(N ++ "@" ++ LocalHost) || {N, _P} <- LocalHosts],
    mapreduce(Nodes, Mapper, 10, Reducer, 10, [{a, ["a", "b", "c", "d"]}, {b, ["b", "b", "b", "b"]}, {c, ["c", "b", "d", "c"]}, {d, ["b", "b", "a", "a"]}, {e, ["a", "b", "c", "e"]}]).