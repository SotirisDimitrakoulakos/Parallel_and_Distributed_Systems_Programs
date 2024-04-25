-module(bank).
-behaviour(gen_server).
-export([start/0, terminate/2, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, balance/2, deposit/3, withdraw/3, lend/4]).
-record(state, {bank = #{}}).

start() -> {ok, Pid} = gen_server:start_link(?MODULE, [], []), Pid.

init(_) -> {ok, #state{}}.

handle_call({balance, Who}, _From, State = #state{bank = Bank}) ->
    Response = case maps:is_key(Who, Bank) of
        true ->
            maps:get(Who, Bank);
        false ->
            no_account
        end,
    {reply, Response, State};

handle_call({deposit, {Who, X}}, _From, State = #state{bank = Bank}) ->
    New_Bank = maps:update_with(Who,fun(V) -> V + X end, X, Bank),
    Response = maps:get(Who, New_Bank),
    {reply, Response, State#state{bank = New_Bank}};

handle_call({withdraw, {Who, X}}, _From, State = #state{bank = Bank}) ->
    Response = case maps:is_key(Who, Bank) of
                    true -> case (Amount = maps:get(Who, Bank)) >= X of 
                                true -> New_Bank = maps:update(Who, Amount - X, Bank), {ok, Amount - X};
                                false -> New_Bank = Bank, insufficient_funds
                            end;
                    false -> New_Bank = Bank, no_account
                end,
    {reply, Response, State#state{bank = New_Bank}}; 

handle_call({lend, {From, To, X}}, _From, State = #state{bank = Bank}) ->
    Response = case {maps:is_key(From, Bank), maps:is_key(To, Bank)} of
                        {false, false} -> New_Bank = Bank, {no_account, both};
                        {false, true} -> New_Bank = Bank, {no_account, From};
                        {true, false} -> New_Bank = Bank, {no_account, To};
                        _ -> case (Amount_from = maps:get(From, Bank)) >= X of
                                true -> Temp = maps:update(From, Amount_from - X, Bank), 
                                        New_Bank = maps:update_with(To, fun(V) -> V + X end, X, Temp),
                                        ok;
                                false -> New_Bank = Bank, insufficient_funds
                            end
                    end,
    {reply, Response, State#state{bank = New_Bank}, 1000}.

handle_cast(_, _) -> ok.

handle_info(Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

balance(Pid, Who) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {balance, Who})
    catch
        exit:_ -> bank_down
    end.

deposit(Pid, Who, X) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {deposit, {Who, X}})
    catch
        exit:_ -> bank_down
    end.

withdraw(Pid, Who, X) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {withdraw, {Who, X}})
    catch
        exit:_ -> bank_down
    end.

lend(Pid, From, To, X) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {lend, {From, To, X}})
    catch
        exit:_ -> bank_down
    end.

stop(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid).