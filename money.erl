-module(money).
-export([start/1]).

start(Args) ->
    CustomerFile = lists:nth(1, Args),
    BankFile = lists:nth(2, Args),

    {ok, CustomerInfo} = file:consult(CustomerFile),
    {ok, BankInfo} = file:consult(BankFile),

    MapC = createMap(CustomerInfo),
    MapB = createMap(BankInfo),
    ListB = maps:keys(MapB),

    FinalCustomerDetails = #{},
    FinalBankDetails = #{},
	
	printStart(),
	
    runProcesses(MapB, MapC, ListB),
    printLog(FinalCustomerDetails, FinalBankDetails, MapC, MapB),
	
	printEnd().

printEnd() ->
	io:fwrite("\n\nThe financial market is closing for the day...\n\n").
		
printStart()->
	io:fwrite("\n\n** The financial market is opening for the day **\n"),
    io:fwrite("\nStarting transaction log...\n\n").

createMap(Info)->
	maps:from_list(Info).

% Function to run Bank and Customer processes
runProcesses(MapB, MapC, ListB) ->
	lists:foreach(
        fun({BankName, Money}) ->            
            register(BankName, spawn(bank, bankProcess, [BankName, Money, self()]))
        end,
        maps:to_list(MapB)
    ),
	CurrentLoan = 0,
    lists:foreach(
        fun({CustomerName, LoanTarget}) ->            
            register(CustomerName, spawn(customer, startCustomerProcess, [CustomerName, LoanTarget, CurrentLoan, ListB, self()]))
        end,
        maps:to_list(MapC)
    ).

% Function that returns sum of values in a map
sumValues(Map) ->
    lists:sum(maps:values(Map)).

% Function to add key value pairs in map
addDetails(Key, Value, Map) ->
    UpdatedMap = maps:put(Key, Value, Map),
    UpdatedMap.

% Function to print log for the bank transactions and  loan requests
printLog(FinalCustomerDetails, FinalBankDetails, MapC, MapB) ->
    case maps:size(FinalCustomerDetails) == maps:size(MapC) of
        true ->
            TotalObjective = sumValues(MapC),
            TotalReceived = sumValues(FinalCustomerDetails),
            io:format("\n\n** Banking Report **\n\n"),
            io:format("Customers:\n"),
            lists:foreach(fun(Key) ->			
                Objective = maps:get(Key, MapC),
                Received = maps:get(Key, FinalCustomerDetails),
                io:format("  ~p: objective ~p, received ~p\n", [Key, Objective, Received])
            end, maps:keys(FinalCustomerDetails)),
			io:fwrite("  -----\n"),
            io:format("  Total: objective ~p, received ~p\n\n", [TotalObjective, TotalReceived]),
            TotalOriginal = sumValues(MapB),
            TotalBalance = sumValues(FinalBankDetails),
            io:format("Banks:\n"),
            lists:foreach(fun(Key) ->
                Original = maps:get(Key, MapB),
                Balance = maps:get(Key, FinalBankDetails),
                io:format("  ~p: original ~p, balance ~p\n", [Key, Original, Balance])
            end, maps:keys(FinalBankDetails)),
			io:fwrite("  -----\n"),
			TotalLoaned = TotalOriginal - TotalBalance,
            io:format("  Total: original ~p, loaned ~p\n", [TotalOriginal, TotalLoaned]);
        false ->
            receive
                {CustomerName, BankName, LoanAmount} ->
                    io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank\n", [CustomerName, LoanAmount, BankName]),
                    printLog(FinalCustomerDetails, FinalBankDetails, MapC, MapB);
                {CustomerResponse, BankName, CustomerName, LoanAmount, MoneyLeft} ->
                    io:format("$ The ~p bank ~p a loan of ~p dollar(s) to ~p\n", [BankName, CustomerResponse, LoanAmount, CustomerName]),
                    UpdatedBankDetails = addDetails(BankName, MoneyLeft, FinalBankDetails),
                    printLog(FinalCustomerDetails, UpdatedBankDetails, MapC, MapB);
                {CustomerName, CurrentLoan} ->
                    UpdatedCustomerDetails = addDetails(CustomerName, CurrentLoan, FinalCustomerDetails),
                    printLog(UpdatedCustomerDetails, FinalBankDetails, MapC, MapB)
            end
    end.
