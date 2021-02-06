-module(tree).
-export([empty/0, insert/3, lookup/2]).
-import(rand,[]).
 
empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
{node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
{node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
{node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
{node, {Key, Val, Smaller, Larger}}.

lookup(_, {node, 'nil'}) ->
undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
{ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
lookup(Key, Larger).

randomTree(N) -> Tree = empty(),
           randomTree(N, Tree).

randomTree(0, Tree) -> Tree;
randomTree(N, Tree) ->
        K = rand:uniform(),
        V = [rand:uniform(10) || _ <- lists:seq(1, 1)],
        NewTree = insert(K,V,Tree),
        randomTree(N-1, NewTree).

treeFromTuple(L) -> Tree = empty(),
                treeFromTuple(L, Tree).

treeFromTuple([], Tree) -> Tree;
treeFromTuple([H|T], Tree) ->
    {K, V} = H,
    NewTree = insert(K,V,Tree),
    treeFromTuple(T, NewTree).


tupleFromTree(L, {node, 'nil'}) -> L;
tupleFromTree(H, {node, {Key, Val, LT, RT}}) ->
    T = {Key, Val},
    tupleFromTree([H|T], LT),
    tupleFromTree([H|T], RT).


process_stream(N) ->
    Consumer = spawn(lab5, consumer, []),
    Middle = spawn(lab5, middle, [Consumer]),
    producent(N, Middle).

consumer() ->
    receive
        {_, Msg} -> io:format("message: ~p", Msg) 
    end.

middle(Consumer) ->
    receive
        {Msg} ->
            io:format("message middle: ~p ~n", Msg),
            Consumer ! Msg
    end.

producent(0, _) -> io:format("message: ~s", "E");
producent(N, Middle) ->
    Middle ! uniform(10),
    producent(N - 1, Middle).

process_list(L, 0) -> [];
process_list(N) ->
    ListP = list_P(N),
    [Process | Tail] = ListP,
    

list_P(0) -> [];
list_P(N) ->
    [spawn(lab5, loop, [])],
    list_P(N-1).


loop() ->
    io:format("Pid ~p ~n",[self()]).


server(Buffer, Capacity, CountPid) ->
  receive
%%    PRODUCER
    {Pid, produce, InputList} ->
      NumberProduce = lists:flatlength(InputList),
      case canProduce(Buffer, NumberProduce, Capacity) of
        true ->
          NewBuffer = append(InputList, Buffer),
          CountPid ! lists:flatlength(InputList),
          Pid ! ok,
          server(NewBuffer,Capacity, CountPid);
        false ->
          Pid ! tryagain,
          server(Buffer, Capacity, CountPid)
      end;

%%    CONSUMER
    {Pid, consume, Number} ->
      case canConsume(Buffer, Number) of
        true ->
          Data = lists:sublist(Buffer, Number),
          NewBuffer = lists:subtract(Buffer, Data),
          Pid ! {ok, Data},
          server(NewBuffer, Capacity,CountPid);
        false ->
          Pid ! tryagain,
          server(Buffer, Capacity, CountPid)

      end
  end.


producer(ServerPid) ->
  X = rand:uniform(9),
  ToProduce = [rand:uniform(500) || _ <- lists:seq(1, X)],
  ServerPid ! {self(),produce,ToProduce},

  producer(ServerPid).

consumer(ServerPid) ->
  X = rand:uniform(9),
  ServerPid ! {self(),consume,X},

  consumer(ServerPid).

spawnProducers(Number, ServerPid) ->
  case Number of
    0 -> io:format("Spawned producers");
    N ->
      spawn(zad2,producer,[ServerPid]),
      spawnProducers(N - 1,ServerPid)
  end.

spawnConsumers(Number, ServerPid) ->
  case Number of
    0 -> io:format("Spawned producers");
    N ->
      spawn(zad2,consumer,[ServerPid]),
      spawnProducers(N - 1,ServerPid)
  end.

start(ProdsNumber, ConsNumber) ->
  CountPid = spawn(zad2, count, [0,0]),

  ServerPid = spawn(zad2,server,[[],20, CountPid]),

  spawnProducers(ProdsNumber, ServerPid),
  spawnConsumers(ConsNumber, ServerPid).

canProduce(Buffer, Number, Capacity) ->
  lists:flatlength(Buffer) + Number =< Capacity.

canConsume(Buffer, Number) ->
  lists:flatlength(Buffer) >= Number.


append([H|T], Tail) ->
  [H|append(T, Tail)];
append([], Tail) ->
  Tail.

producer(Buffer) ->
    % create a new item
    Item = [rand:uniform(10) || _ <- lists:seq(1, 10)],
    buffer:put(Buffer, Item),
    producer(Buffer).

consumer(Buffer) ->
    Item = buffer:get(Buffer),
    % do something with ‘item’
    io:format("message: ~p", Item),
    consumer(Buffer).


buffer(Content, Count, Bound) ->
    receive
    % serve gets when buffer not empty
    {get, From, Ref} when Count > 0 ->
    [First|Rest] = Content, % match first item
    From ! {item, Ref, First}, % send it out
    buffer(Rest, Count-1, Bound); % remove it from buffer
    % serve puts when buffer not full
    {put, From, Ref, Item} when Count < Bound ->
    From ! {done, Ref}, % send ack
    buffer(Content ++ [Item], Count+1, Bound) % add item to end
    end.

get(Buffer) ->
    Ref = make_ref(),
    Buffer ! {get, self(), Ref},
    receive {item, Ref, Item} -> Item end.

put(Buffer, Item) ->
    Ref = make_ref(),
    Buffer ! {put, self(), Ref, Item},
    receive {done, Ref} -> done end.


main(_) -> 
    Tree = empty(),
    NewTree = insert("Jim Woodland", "jim.woodland@gmail.com", Tree),
    io:format("drzewo ~w = ~p\n", [1, NewTree]),
    NewTree2 = insert("Mark Anderson", "i.am.a@hotmail.com", NewTree),
    io:format("drzewo ~w = ~p\n", [2, NewTree2]),
    Addresses = insert("Anita Bath", "abath@someuni.edu", insert("Kevin Robert", "myfairy@yahoo.com", insert("Wilson Longbrow", "longwil@gmail.com", NewTree2))),
    io:format("drzewo ~w = ~p\n", [5, Addresses]),
    Look = lookup("Anita Bath", Addresses),
    io:format("Look ~w = ~p\n", [5, Look]),
    io:format("drzewo random ~w = ~p\n", [3, randomTree(3)]),
    A = [{1,2},{3,6},{4,8},{2,4},{5,10}],
    NewTree3 = treeFromTuple(A),
    io:format("drzewo z krotki ~w = ~p\n", [A, NewTree3]),
    io:format("look ~w = ~p\n", [2, lookup(2,NewTree3)]),
    L = tupleFromTree([], NewTree3),
    io:format("krotka z drzewa ~w = ~p\n", [NewTree3, L]),
    io:format("krotka z drzewa ~w = ~p\n", [5, L]),
    io:format("drzewo z krotki ~w = ~p\n", [L, NewTree3]).

% generacja drzewa z listy -> z krotki