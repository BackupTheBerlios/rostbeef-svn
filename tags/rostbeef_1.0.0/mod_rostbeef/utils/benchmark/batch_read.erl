%%% Presence Tracker
-module(batch_read).
-author('markus.knofe@zweitgeist.com').
-vsn('1.0').

-export([test/0]).

-record(presence_tracker_data, {us, presence_list=[]}).
-include_lib("stdlib/include/qlc.hrl").

gtd(Number) -> gtdr(Number).
gtdr(Number) ->
    L = if  Number == 0 ->
              [{0,0}];
            Number > 0 ->
              [{Number,Number} | gtdr(Number-1)]
        end,
    L.

generate_testdata(L) ->
    T1 = erlang:now(),
    F = fun() ->
      lists:map(fun(T) ->
                    catch mnesia:write( presence_tracker_data,
                    #presence_tracker_data{us=T,presence_list = [{" ", " ", " ", " "}]},
                    write)
                end, L)
    end,
    mnesia:transaction(F),
    timer:now_diff(erlang:now(),T1).

read_testdata1() ->
  T1 = erlang:now(),
  mnesia:transaction(fun()-> mnesia:table(presence_tracker_data) end),
  timer:now_diff(erlang:now(),T1).

read_testdata2(U) ->
  T1 = erlang:now(),
  F = fun(X) -> lists:member(X#presence_tracker_data.us,U) end,
  %io:format("~p~n", [
      mnesia:transaction( fun() ->
          qlc:e(
                qlc:q(
                    [ X || X <- mnesia:table(presence_tracker_data), F(X) ]
                    )
               )
          end
        )
  %])
  ,
  timer:now_diff(erlang:now(),T1).

read_testdata3(U) ->
  T1 = erlang:now(),
  lists:flatmap(
    fun(Elem) ->
      F = fun(Elem2) ->  mnesia:read(presence_tracker_data, Elem2) end,
      V = case mnesia:transaction(F) of
        { atomic, Val } -> Val;
        { aborted, _ } -> [{presence_tracker_data, Elem, []}]
      end,
      V
    end,
    U
  ),
  timer:now_diff(erlang:now(),T1).

read_testdata4(U) ->
  T1 = erlang:now(),
  lists:flatmap(
    fun(Elem) ->
      mnesia:dirty_read(presence_tracker_data, Elem)
    end,
    U
  ),
  timer:now_diff(erlang:now(),T1).


h_one( [H|T] ) -> [mnesia:dirty_read(presence_tracker_data, H)] ++ h_one(T);
h_one( [] )    -> [].

read_testdata5(U) ->
  T1 = erlang:now(),
  h_one(U),
  timer:now_diff(erlang:now(),T1).

read_testdata6(U) ->
  T1 = erlang:now(),
  lists:foldl( fun(Elem, A) ->
                  F = fun() ->
                        %Result = mnesia:read(presence_tracker_data, {"User","Server"}, write),
                        Data = #presence_tracker_data{us = Elem , presence_list = '$1'},
                        catch mnesia:select(presence_tracker_data, [{Data, [], ['$1'] }])
                      end,
                  mnesia:transaction(F)
                end,
                [],
                U),
  timer:now_diff(erlang:now(),T1).

test() ->
    mnesia:start(),
    case mnesia:create_table( presence_tracker_data,
              [ {ram_copies, [ node() ]}, {attributes, record_info(fields, presence_tracker_data)} ] ) of
        {atomic, ok} ->
              io:format("~p: Created Ram-Table~n", [?MODULE]);
        {aborted, {already_exists, _}} ->
              io:format("~p: Ram-Table already exsists~n",[?MODULE]);
        {aborted, Reason} ->
              io:format("~p: Faild to create Ram-Table: ~p~n",[?MODULE, Reason])
    end,
    L = gtd(200000),
    U = gtd(300000),
    io:format("gen ~p microsecond~n", [generate_testdata(L)]),
    io:format("read ~p microsecond~n", [read_testdata1()]),
    io:format("read2 ~p microsecond\n", [read_testdata2(U)]),
    io:format("read3 ~p microsecond\n", [read_testdata3(U)]),
    io:format("read6 ~p microsecond\n", [read_testdata6(U)]),
    io:format("read4 ~p microsecond\n", [read_testdata4(U)]),
    io:format("read5 ~p microsecond\n", [read_testdata5(U)]),
    ok.
