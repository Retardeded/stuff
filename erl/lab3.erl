-module(lab3).
-compile(export_all).

pole({kwadrat,X}) -> X*X;
pole({trojkat,A, H}) -> A*H*0.5;
pole({trapez,A,B,H}) -> ((A+B)/2)*H;
pole({stozek,R,L}) -> 3,14*R*R + 3,14*R*L;
pole({kula,X}) -> 4*X*X*3.14;
pole({szescian,X}) -> X*X*6;
pole({kolo,X}) -> 3.14*X*X.

objetosc({stozek,R,L}) -> 3.14*R*R*L/3;
objetosc({szescian,X}) -> X*X*X;
objetosc({kula,X}) -> 4/3*3.14*X*X*X.

len([]) -> 0;
len([_|T]) -> 1 + len(T).

minimum([]) -> io:format("can not find minimum of empty list~n");

minimum([H|T])  ->
        minimum(H, T).

minimum(Min, [H|T]) ->
        case Min < H of
                true -> minimum(Min, T);
                false -> minimum(H, T)
        end;

minimum(Min, []) -> Min.

maximum([])     -> io:format("can not find max from empty list~n");
maximum([H|T])  ->
                maximum(H, T).

maximum(Max, [H|T])     ->
                        case Max > H of
                        true    -> maximum(Max, T);
                        false   -> maximum(H, T)
                        end;
maximum(Max, [])        -> Max.


min_max([H|T]) -> min_max(T, H, H).

min_max([], Min, Max) -> {Min, Max};
min_max([H|T], Min, Max) ->
    if 
      H < Min -> min_max(T, H, Max);
      H > Max -> min_max(T, Min, H);
      true -> min_max(T, Min, Max)
    end.

min_maxL([H|T]) -> min_maxL(T, H, H).

min_maxL([], Min, Max) -> [Min, Max];
min_maxL([H|T], Min, Max) ->
    if 
      H < Min -> min_maxL(T, H, Max);
      H > Max -> min_maxL(T, Min, H);
      true -> min_maxL(T, Min, Max)
    end.


list_dec(N) -> lists:reverse(lists:seq(1,N)).

convert(Temperature, T) ->
case Temperature of
{Type, N} when T == Type ->
{Type, N};
{celsius, N} when T == fahrenheit ->
{fahrenheit, (N * 9/5) + 32};
{celsius, N} when T == kelvin ->
{kelvin, (N) + 273.15};
{celsius, N} when T == rankine ->
{rankine, (N + 273.15) * 9/5};
{kelvin, N} when T == celsius ->
convert({celsius, N - 273.15}, celsius);
{kelvin, N} when T == fahrenheit ->
convert({celsius, N - 273.15}, fahrenheit);
{kelvin, N} when T == rankine ->
convert({celsius, N - 273.15}, rankine);
{fahrenheit, N} when T == celsius ->
convert({celsius, (N - 32) * 5/9}, celsius);
{fahrenheit, N} when T == kelvin ->
convert({celsius, (N - 32) * 5/9}, kelvin);
{fahrenheit, N} when T == rankine ->
convert({celsius, (N - 32) * 5/9}, rankine);
{rankine, N} when T == celsius ->
convert({celsius, (N - 491.67) * 5/9}, celsius);
{rankine, N} when T == fahrenheit ->
convert({celsius, (N - 491.67) * 5/9}, fahrenheit);
{rankine, N} when T == kelvin ->
convert({celsius, (N - 491.67) * 5/9}, kelvin);
_ ->
'wrong'
end.

listN_N(N,Term) ->
listN_N(N,Term,[]).
 
listN_N(0,_,List) ->
List;
listN_N(N,Term,List) when N > 0 ->
listN_N(N-1, Term, [Term|List]).

merge_sort([L]) -> [L]; 
merge_sort(L)   ->
    {L1,L2} = lists:split(length(L) div 2, L),
    merge(merge_sort(L1), merge_sort(L2)).

merge(L1, L2)    -> merge(L1, L2, []).
merge([], L2, A) -> A++L2;
merge(L1, [], A) -> A++L1;
merge([H1|T1], [H2|T2], A) when H2>=H1 -> merge(T1, [H2|T2], A++[H1]);
merge([H1|T1], [H2|T2], A) when H1>H2  -> merge([H1|T1], T2, A++[H2]).

bubble_sort(L) when length(L) =< 1 ->
    L;
bubble_sort(L) ->
    SL = bubble_sort_p(L),
    bubble_sort(lists:sublist(SL,1,length(SL)-1)) ++ [lists:last(SL)].

bubble_sort_p([])  ->
    [];
bubble_sort_p([F]) ->
    [F];
bubble_sort_p([F,G|T]) when F > G ->
    [G|bubble_sort_p([F|T])];
bubble_sort_p([F,G|T]) ->
    [F|bubble_sort_p([G|T])].


% wyświetlamy wyniki funkcji
main(_) ->
    io:format("pole szescian ~w = ~w\n", [5, pole({szescian,5})]),
    io:format("objetosc kula ~w = ~w\n", [5, objetosc({kula,5})]),

    io:format("len lista ~w = ~w \n", [[1,2,3], len([1,2,3])]),

    io:format("min ~w = ~w \n", [[1,2,3], minimum([1,2,3])]),

    io:format("min_maxL ~w = ~w \n", [[1,2,3], min_maxL([1,2,3])]),

    io:format("list_dec ~w = ~w \n", [5, list_dec(5)]),
    
    io:format("convert  ~w = ~w \n", [{kelvin, 1}, convert({kelvin, 1}, rankine)]),

    io:format("list N  ~w = ~w \n", [5, listN_N(5,2)]),

    io:format("merge  ~w = ~w \n", [[6,7,3,4,2,1], merge_sort([6,7,3,4,2,1])]),

    io:format("bubble  ~w = ~w \n", [[6,7,3,4,2,1], bubble_sort([6,7,3,4,2,1])]).

