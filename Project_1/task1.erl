-module(task1).
-export([eval/1, eval/2, map/2, filter/2, split/2, groupby/2, groupby/4]).
-compile(export_all).




%Problem 1
eval({Op, E1, E2}) when is_number(E1) andalso is_number(E2)->
    case Op of
        add -> {ok, E1+E2};
        mul -> {ok, E1*E2};
        'div' -> {ok, E1/E2};
        sub -> {ok, E1-E2};
        _ -> error
    end;

eval({Op, {Op2, E3, E4}, E2}) when is_number(E2)->
    case Op of
        add -> {ok, element(2, eval({Op2, E3, E4}))+E2};
        mul -> {ok, element(2, eval({Op2, E3, E4}))*E2};
        'div' -> {ok, element(2, eval({Op2, E3, E4}))/E2};
        sub -> {ok, element(2, eval({Op2, E3, E4}))-E2};
        _ -> error
    end;

eval({Op, E1, {Op2, E3, E4}}) when is_number(E1)->
    case Op of 
        add -> {ok, element(2, eval({Op2, E3, E4}))+E1};
        mul -> {ok, element(2, eval({Op2, E3, E4}))*E1};
        'div' -> {ok, E1/element(2, eval({Op2, E3, E4}))};
        sub -> {ok, E1-element(2, eval({Op2, E3, E4}))};
        _ -> error
    end;

eval({Op, {Op1, E1, E2}, {Op2, E3, E4}}) ->
    case Op of 
        add -> {ok, element(2, eval({Op2, E3, E4}))+element(2, eval({Op1, E1, E2}))};
        mul -> {ok, element(2, eval({Op2, E3, E4}))*element(2, eval({Op1, E1, E2}))};
        'div' -> {ok, element(2, eval({Op1, E1, E2}))/element(2, eval({Op2, E3, E4}))};
        sub -> {ok, element(2, eval({Op1, E1, E2}))-element(2, eval({Op2, E3, E4}))};
        _ -> error
    end;

eval(Tuple) when is_tuple(Tuple), tuple_size(Tuple) =/= 3 -> error;
eval({_, _, _}) -> error;
eval(_) -> error.


%Problem 2
eval({Op, E1, E2}, LookupTable) ->
    case {eval(E1, LookupTable), eval(E2, LookupTable)} of
        {{ok, V1}, {ok, V2}} ->
            Result = case Op of
                add -> V1 + V2;
                mul -> V1 * V2;
                'div' -> V1 / V2;
                sub -> V1 - V2;
                _ -> {error, unknownerror}
            end,
            {ok, Result};
        {_, _} -> {error, variable_not_found}
    end;
eval(Number,_) when is_number(Number) -> {ok, Number};
eval(Atom, LookupTable) when is_atom(Atom) ->
    case maps:find(Atom, LookupTable) of
        {ok, Value} -> {ok, Value};
        error -> {error, variable_not_found}
    end.


%Problem 3
map(_F, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

filter(_P, []) -> [];
filter(P, [H | T]) ->
    case P(H) of
        true -> [H | filter(P, T)];
        false -> filter(P, T)
    end.


split(_P, []) -> {[], []};
split(P, [H | T]) ->
    case P(H) of
        true ->  {True, False} = split(P, T), {[H | True], False};
        false -> {True, False} = split(P, T), {True, [H | False]}
    end.


groupby(_F, []) -> #{}; 
groupby(F, List) -> groupby(F, List, 1, #{}).
groupby(_F, [], _Index, ResultMap) -> 
    lists:foldl(fun({Key, Indices}, Acc) -> 
        maps:put(Key, lists:reverse(Indices), Acc) 
    end, 
    #{}, maps:to_list(ResultMap));
groupby(F, [H | T], Index, ResultMap) ->
    K = F(H),
    UpdatedMap = case maps:find(K, ResultMap) of
                    {ok, Indices} -> maps:put(K, [Index | Indices], ResultMap);
                    error ->maps:put(K, [Index], ResultMap)
                 end,
    groupby(F, T, Index + 1, UpdatedMap).







