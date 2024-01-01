-module(customer).
-export([startCustomerProcess/5]).

% Function to start customer process and sleep for 200ms
startCustomerProcess(CustomerName, LoanTarget, CurrentLoan, ListB, MainThread) ->
	timer:sleep(200),
	customerProcess(CustomerName, LoanTarget, CurrentLoan, ListB, MainThread).

% Customer process that will run recursively	
customerProcess(CustomerName, LoanTarget, CurrentLoan, ListB, MainThread)->
    if
        LoanTarget > CurrentLoan, length(ListB) > 0 ->
            RandomSleepTime = rand:uniform(91) + 9,           
            LoanAmount = erlang:min(rand:uniform(50), LoanTarget - CurrentLoan),
            RandomIndex = rand:uniform(length(ListB)),
            SelectedBankName = lists:nth(RandomIndex, ListB),
			Bank = whereis(SelectedBankName),
			timer:sleep(RandomSleepTime),
            MainThread ! {CustomerName, SelectedBankName, LoanAmount},
            Bank ! {CustomerName, LoanAmount},
            receive
                {BankResponse, BankName} ->
                    case BankResponse of
                        'approves' ->
                            customerProcess(CustomerName, LoanTarget, CurrentLoan + LoanAmount, ListB, MainThread);
                        'denies' ->
                            NewListB = lists:delete(BankName, ListB),
                            customerProcess(CustomerName, LoanTarget, CurrentLoan, NewListB, MainThread)
                    end
            end;
        true ->
            MainThread ! {CustomerName, CurrentLoan}
    end.
