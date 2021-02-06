-module(lab5).
-compile(export_all).

-import(rand, [uniform/1]).

 
car_diagnostics() ->
    receive
        velocity -> io:format("80 km/h ~n");
        number_of_kilometeres -> io:format("9654 km ~n");
        _ -> io:format("Diagnostic doe not recognized ~n")
    end.

car_diagnostics_with_reply_message() ->
    receive
        {From, velocity} -> 
            From ! "80 km/h";
        {From, number_of_kilometers} -> 
            From ! "9654 km";
        _ -> io:format("Diagnostic code not recognized ~n")
    end.

car_diagnostics_with_reply_message_rec() ->
    receive
        {From, velocity} -> 
            From ! "80 km/h",
            car_diagnostics_with_reply_message_rec();
        {From, number_of_kilometers} -> 
            From ! "9654 km",
            car_diagnostics_with_reply_message_rec();
        {From, end_of_diagnostic} -> 
            From ! "End of diagnostic process ~n";
        _ -> io:format("Diagnostic code not recognized ~n"),
             car_diagnostics_with_reply_message_rec()
    end.

fridge(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge(lists:delete(Food, FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge(FoodList)
            end;
        terminate ->
            ok
    end.



process_stream(N) ->
    Consumer = spawn(lab5, consumer, []),
    Middle = spawn(lab5, middle, [Consumer]),
    producent(N, Middle).

consumer() ->
    receive
		{Msg} -> 
			io:format("~w~n", [Msg]),
			consumer()
	end.

middle(Consumer) ->
    receive
        {Msg} ->
            Consumer ! {Msg},
        middle(Consumer)   
    end.

producent(0, _) -> true;
producent(N, Middle) ->
    Middle ! {uniform(1000)},
    producent(N - 1, Middle).

% nie wiem o co chodzi w drugim
start(N) ->
    L = [spawn(lab5, loop, [X]) || X <- lists:seq(1,N)], 
    [H|T] = L,
    H!{[1,2,3,4]}.                                 


loop(Nr) ->
    receive
      {List} ->
          io:format("Nr procesu: ~p ~n",[Nr]),
          L = lists:reverse(List),
          io:format("ListRev : ~w ",[L]),
          exit(normal)
    end.


main(_) -> 
    io:format(":  ~w = ~w\n", [1, process_stream(10)]),
    start(5).

% generacja drzewa z listy -> z krotki