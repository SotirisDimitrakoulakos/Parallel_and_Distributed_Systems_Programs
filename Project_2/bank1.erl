-module(bank).
-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).

start() -> spawn(fun() -> loop(#{}) end).

loop(Accounts) ->
    receive
        {balance, pid, Ref, Who} ->
            case maps:is_key(Who, Accounts) of
                true -> pid ! {Ref,{ok,maps:get(Who, Accounts)}};
                false -> pid ! {Ref, no_account}
            end,
            NewAccounts = Accounts;
        {deposit, pid, Ref, Who, X} ->
            NewAccounts = maps:update_with(Who,fun(V) -> V + X end, X, Accounts),
            pid ! {Ref, {ok, maps:get(Who, NewAccounts)}};
        {withdraw, pid, Ref, Who, X} ->
            Res = case maps:is_key(Who, Accounts) of
                    true -> case (Amount = maps:get(Who, Accounts)) >= X of 
                                true -> NewAccounts = maps:update(Who, Amount - X, Accounts), {ok, Amount - X};
                                false -> NewAccounts = Accounts, insufficient_funds
                            end;
                    false -> NewAccounts = Accounts, no_account
                  end,
            pid ! {Ref, Res};
        {lend, pid, Ref, From, To, X} ->
            Res = case {maps:in_key(From, Accounts), maps:is_key(To, Accounts)} of
                    {false, false} -> NewAccounts = Accounts, {no_account, both};
                    {false, true} -> NewAccounts = Accounts, {no_account, From};
                    {true, false} -> NewAccounts = Accounts, {no_account, To};
                    _ -> case (Amount_from = maps:get(From, Accounts)) >= X of
                            true -> Temp = maps:update(From, Amount_from - X, Accounts), 
                                    NewAccounts = maps:update_with(To, fun(V) -> V = X end, X, Temp),
                                    ok;
                            false -> NewAccounts = Accounts, insufficient_funds
                         end
                  end,
                  pid ! {Ref, Res}
    end,
    loop(NewAccounts).

balance(pid, Who) ->
    Ref = make_ref(),
    try
        pid ! {balance, self(), Ref, Who}, monitor_bank(pid)
    catch
        error:badarg -> no_bank
    end.

deposit(pid, Who, X) ->
    Ref = make_ref(),
    try
        pid ! {deposit, self(), Ref,  Who, X}, monitor_bank(pid)
    catch
        error:badarg -> no_bank
    end.

withdraw(pid, Who, X) ->
    Ref = make_ref(),
    try
        pid ! {withdraw, self(), Ref, Who, X}, monitor_bank(pid)
    catch
        error:badarg -> no_bank
    end.

lend(pid, From, To, X) ->
    Ref = make_ref(),
    try
        pid ! {lend, self(), Ref, From, To, X}, monitor_bank(pid)
    catch
        error:badarg -> no_bank
    end.

monitor_bank(pid) ->
    MRef = monitor(process, pid),
    receive
        {_Ref, Result} -> Result;
        {'DOWN', MRef, process, _P, noproc} -> demonitor(MRef), no_bank
    end.