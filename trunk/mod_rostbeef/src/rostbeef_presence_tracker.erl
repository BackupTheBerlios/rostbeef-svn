%%% Presence Tracker
-module(rostbeef_presence_tracker).
-author('flatline@users.berlios.de').
-vsn('0.1').

%% export modul-interface
-export([ %test/0, %% trest disbaled
          init_presence_database/0,
          write_presence/6, write_presence/5, write_presence/4,
          delete_presence/3,
          read_presence/2,
          dirty_read_presence/2]).

-define(ejabberd_debug, false).
-include("ejabberd.hrl").

%% define Name
%-define(PROCNAME, ejabberd_mod_presence_tracker).

%% define record for nmesia ram-table
-record(presence_tracker_data, {us, presence_list=[]}).

%test() ->
%  mnesia:start(),
%  init_presence_database(),
%  write_presence("User1","Server","1","available"),
%  write_presence("User1","Server","2","away", "SomeState"),
%  write_presence("User1","Server","3","xa", "SomeState", 42),
%  io:format ("{~p,~p} ~p~n", ["User1","Server", read_presence("User1","Server")]),
%  io:format ("{~p,~p} ~p~n~n", ["User2","Server", read_presence("User2","Server")]),
%  write_presence("User2","Server","A","chat", "SomeState", 42),
%  write_presence("User2","Server","B","away", "SomeState", 42),
%  write_presence("User2","Server","C","dnd", "SomeState", 42),
%  delete_presence("User1","Server","1"),
%  delete_presence("User1","Server","2"),
%  delete_presence("User1","Server","3"),
%  io:format ("{~p,~p} ~p~n", ["User1","Server", read_presence("User1","Server")]),
%  io:format ("{~p,~p} ~p~n", ["User2","Server", read_presence("User2","Server")]),
%  delete_presence("U","S","Resource").

%% Starts the Modul
init_presence_database() ->
    %% create ram-table
    case  mnesia:create_table( presence_tracker_data,
            [ {ram_copies, [ node()| nodes() ]}, {attributes, record_info(fields, presence_tracker_data)} ] ) of
      {atomic, ok} ->
            ?INFO_MSG("~p: Created Ram-Table~n", [?MODULE]);
      {aborted, {already_exists, _}} ->
            ?INFO_MSG("~p: Ram-Table already exsists~n",[?MODULE]);
      {aborted, Reason} ->
            ?ERROR_MSG("~p: Faild to create Ram-Table: ~p~n",[?MODULE, Reason])
    end.

%% called when a resource change is state
%% User     - string (must be lowcase)
%% Server   - string (must be lowcase)
%% Resource - string (must be lowcase)
%% Show     - string (must be one of [away, chat, dnd, xa, available, unavailable] )
%% Status   - string
%% Priority - int
write_presence(User, Server, Resource, Show, State, Priority) ->
    F = fun() ->
                List =  case mnesia:wread({presence_tracker_data, {User, Server}}) of
                            [#presence_tracker_data{presence_list=OldList}] ->
                                lists:keydelete(Resource, 1, OldList);
                            _ ->
                                []
                        end,
                mnesia:write(#presence_tracker_data{us={User, Server},
                                                    presence_list = List ++
                                                    [{Resource, Show, State, Priority}]})
        end,
    mnesia:transaction(F).
%% callable with no Priority (and State)
write_presence(User, Server, Resource, Show) -> write_presence(User, Server, Resource, Show, " ", 0).
write_presence(User, Server, Resource, Show, State) -> write_presence(User, Server, Resource, Show, State, 0).


%% called to remove a resource
%% called when a resource change is state
%% User     - string (must be lowcase)
%% Server   - string (must be lowcase)
%% Resource - string (must be lowcase)
delete_presence(User, Server, Resource) ->
    F = fun() ->
                List =  case mnesia:wread({presence_tracker_data, {User, Server}}) of
                            [#presence_tracker_data{presence_list=OldList}] ->
                                lists:keydelete(Resource, 1, OldList);
                            _ ->
                                mthie
                        end,
                case List of
                    mthie -> void;
                    [] ->
                        mnesia:delete(presence_tracker_data, {User,Server}, write);
                    _ ->
                        mnesia:write(#presence_tracker_data{us={User, Server},presence_list=List})
                end
        end,
    mnesia:transaction(F).

%% called to recive all resource-stats for a User, Server- Tuple
%% User     - string (must be lowcase)
%% Server   - string (must be lowcase)
read_presence(User, Server) ->
    F = fun() ->
                Data = #presence_tracker_data{us = {User, Server} , presence_list = '$1'},
                mnesia:select(presence_tracker_data, [{Data, [], ['$1'] }])
        end,
    case mnesia:transaction(F) of
        {atomic, [Result]} -> Result;
        _ ->                [{"", "unavailable", "", 0}]
    end.

dirty_read_presence(User, Server) ->
    case catch mnesia:dirty_read(presence_tracker_data, {User, Server}) of
      [{presence_tracker_data, _, Result}]  -> Result;
      _                                     -> [{"", "unavailable", "", 0}]
    end.
