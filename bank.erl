-module(bank).
-export([bankProcess/3]).

% Bank process that will run recursively
bankProcess(BankName, Money, MainThread) ->
    receive
        {CustomerName, LoanAmount} ->
            if
                Money >= LoanAmount ->
                    BankResponse = 'approves',
                    sendMessages(BankResponse, BankName, CustomerName, LoanAmount, Money - LoanAmount, MainThread),
                    bankProcess(BankName, Money - LoanAmount, MainThread);
                true ->
                    BankResponse = 'denies',
                    sendMessages(BankResponse, BankName, CustomerName, LoanAmount, Money, MainThread),
                    bankProcess(BankName, Money, MainThread)
            end
    end.

%Function to send messages
sendMessages(BankResponse, BankName, CustomerName, LoanAmount, Money, MainThread) ->
	Customer = whereis(CustomerName),
	Customer ! {BankResponse, BankName},
    MainThread ! {BankResponse, BankName, CustomerName, LoanAmount, Money}.
