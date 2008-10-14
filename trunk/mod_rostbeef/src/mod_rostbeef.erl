%%%----------------------------------------------------------------------
%%% File    : mod_xmlrpc_ext.erl
%%% Author  : Rafael <ruzarowski@zweitgeist.com>
%%% Purpose : XML-RPC server extension
%%% Created : 15 Nov 2007 by Rafael <ruzarowski@zweitgeist.com>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_rostbeef).
-author('ruzarowski@zweitgeist.com').
-vsn('1.0').

-behaviour(gen_mod).

-export([start/2,
         handler/2,
         loop/1,
         stop/1]).

-export([presence_set/4, presence_unset/4]).

-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").

-ifdef(ejabberd_debug).
-export([link_contacts/4, unlink_contacts/2]).
-endif.


-define(PROCNAME, ejabberd_mod_rostbeef).

%% -----------------------------
%% Module interface
%% -----------------------------

start(_Host, Opts) ->
    case whereis(?PROCNAME) of
    undefined ->
    %% init presencetraker
    rostbeef_presence_tracker:init_presence_database(),

    %% Add Hooks in order to track state per user/resource in mnesia db
      ejabberd_hooks:add(set_presence_hook, _Host,
           ?MODULE, presence_set, 50),
      ejabberd_hooks:add(unset_presence_hook, _Host,
           ?MODULE, presence_unset, 50),

      Port = gen_mod:get_opt(port, Opts, 4560),
      MaxSessions = 10,
      Timeout = 50,
      Handler = {mod_rostbeef, handler},
      State = started,
      Ip = gen_mod:get_opt(ip, Opts, all),
      {ok, Pid} = xmlrpc:start_link(Ip, Port, MaxSessions, Timeout, Handler, State), %% start process
      ?INFO_MSG("Started ~p on node ~p with pid ~p~n", [?MODULE, node(), Pid]), %% info
      register(?PROCNAME, spawn(?MODULE, loop, [Pid]));
    _ ->
      ?INFO_MSG("~p already running on node ~p~n", [?MODULE, node()]),
      ok
    end.

loop(Pid) ->
    receive
      stop ->
        ?INFO_MSG("~p on node ~p received stop message~n", [?MODULE, node()]),
        xmlrpc:stop(Pid)
    end.

stop(_Host) ->
    %% remove Hooks
    ejabberd_hooks:delete(set_presence_hook, _Host,
           ?MODULE, presence_set, 50),
    ejabberd_hooks:delete(unset_presence_hook, _Host,
           ?MODULE, presence_unset, 50),

    ?PROCNAME ! stop.


%% -----------------------------
%% Handlers
%% -----------------------------

%% Call:           Arguments:                                      Returns:

% link_contacts  struct[{jid1, String}, {nick1, String},
%                       {jid2, String}, {nick2, String}]           Integer
handler(_State, {call, link_contacts, [{struct, AttrL}]}) ->
    [Jid1, Nick1, Jid2, Nick2] = get_attrs([jid1, nick1, jid2, nick2], AttrL),
    R = case catch link_contacts(Jid1, Nick1, Jid2, Nick2) of
            ok ->
                0;
            {error, Reason} ->
                lists:flatten(io_lib:format("Can't add roster item to user ~p on node ~p: ~p~n",
                                            [Jid1, node(), Reason])),
                -1;
            {'EXIT',_Err} ->
                ?ERROR_MSG("~p", [_Err]),
                lists:flatten(io_lib:format("Can't add roster item to user ~p on node ~p: Internal Server Error~n",
                                            [Jid1, node()])),
                -1

        end,
    {false, {response, [R]}};

% unlink_contacts  struct[{jid1, String}, {jid2, String}]           Integer
handler(_State, {call, unlink_contacts, [{struct, AttrL}]}) ->
    [Jid1, Jid2] = get_attrs([jid1, jid2], AttrL),
    R = case catch unlink_contacts(Jid1, Jid2) of
            ok ->
                0;
            {error, Reason} ->
                lists:flatten(io_lib:format("Can't add roster item to user ~p on node ~p: ~p~n",
                                            [Jid1, node(), Reason])),
                -1;
            {'EXIT',_Err} ->
                ?ERROR_MSG("~p", [_Err]),
                lists:flatten(io_lib:format("Can't add roster item to user ~p on node ~p: Internal Server Error~n",
                                            [Jid1, node()])),
                -1

        end,
    {false, {response, [R]}};

%% get_resources  struct[{user, String}, {server, String}          Array
handler(_State, {call, get_resources, [{struct, AttrL}]}) ->
    [User, Server] = get_attrs([user, server], AttrL),
    ?DEBUG("XMLRPC::get_resources for ~p@~p~n", [User, Server]),
    case ejabberd_sm:get_user_resources(User, Server) of
      Resources when is_list(Resources) ->
        {false, {response, [{array,Resources}]}};
      _Foo ->
        ?DEBUG("resources not as list: ~p", [_Foo]),
        {false, {response, [{array, []}]}}
    end;

%% Array of Struct(String jid, String show, String status)
handler(_State, {call, get_presence, [{struct, AttrL}]}) ->
    [User, Server] = get_attrs([user, server], AttrL),
    Presence =  rostbeef_presence_tracker:dirty_read_presence(User, Server),
    {false, {response, [{array, rostbeef_utils:make_presence_xmlrpc_with_jid(User, Server, Presence)}] } };

%% get_roster  struct[{user, String}, {server, String}]
%%                       array[struct[{jid, String}, {nick, String}, {Group, String}]]
handler(_State, {call, get_roster, [{struct, AttrL}]}) ->
    [User, Server] = get_attrs([user, server], AttrL),
    Node = node(),
    R = case get_roster(User, Server) of
      {ok, Roster} ->
          ?DEBUG("XMLRPC::get_roster for ~p@~p", [User, Server]),
          RosterXMLRPC = make_roster_xmlrpc(Roster),
          {array, RosterXMLRPC};
      {error, Reason} ->
          ?ERROR_MSG("Can't get roster of user ~p@~p on node ~p: ~p",
          [User, Server, Node, Reason]),
          1;
      {badrpc, Reason} ->
          ?ERROR_MSG("Can't get roster of user ~p@~p on node ~p: ~p",
          [User, Server, Node, Reason]),
          1
    end,
    {false, {response, [R]}};

%% get_roster_with_presence
%% Array of Struct(String jid, String resource, String group, String nick, String subscription, String pending, String show, String status, Integer priority)
handler(_State, {call, get_roster_with_presence, [{struct, AttrL}]}) ->
    [User, Server] = get_attrs([user, server], AttrL),
    R = case get_roster(User, Server) of
      {ok, Roster} ->
          ?DEBUG("XMLRPC::get_roster for ~p@~p", [User, Server]),
          RosterXMLRPC = make_roster_with_presence_xmlrpc(Roster),
          {array, RosterXMLRPC};
      {error, Reason} ->
          ?ERROR_MSG("Can't get roster of user ~p@~p on node ~p: ~p", [User, Server, node(), Reason]),
          1
    end,
    { false, {response, [R]} };

%% send_stanza  struct[{from, String}, {to, String}, {stanza, String}]  Integer
handler(_State, {call, send_stanza, [{struct, AttrL}]}) ->
    [FromJIDString, ToJIDString, Stanza] = get_attrs([from, to, stanza], AttrL),
    FromJID = jlib:string_to_jid(FromJIDString),
    ToJID = jlib:string_to_jid(ToJIDString),
    StanzaPacket = parse_stanza(Stanza),
    R = case StanzaPacket of
        false ->
            ?ERROR_MSG("XMLRPC::send_stanza: Failed to parse stanza: ~p", [Stanza]),
            {fault, -1, lists:flatten(io_lib:format("Failed to parse stanza: ~p", [Stanza]))};
        _ ->
            ?DEBUG("XMLRPC::send_stanza: from=[~p] to=[~p] stanza=[~p]", [FromJIDString, ToJIDString, Stanza]),
            send_stanza(FromJID, ToJID, StanzaPacket),
            0
    end,
    {false, {response, [R]}};

handler(_State, {call, echothis, [A]}) ->
{false, {response, ["echoed: " ++ A]}};

handler(_State, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    ?INFO_MSG("Unknown call: ~p", [Payload]),
    {false, {response, {fault, -1, FaultString}}}.

%% -----------------------------
%% Utils

%% Stanza

%% @TODO
%% EVENTUELL wäre es hier besser anstatt die packete mittels
%% ejabberd_router:route zu verschicken, sie mittels ejabberd_sm:route zu verschicken
%% dann aber nicht an jede Resource direkt sondern nur einmal auf Basis des Users
%% ohne Resource (foo@localhost)
send_stanza(FromJID, ToJID, Stanza) ->
    ToUser = ToJID#jid.user,
    ToServer = ToJID#jid.server,
    case ToJID#jid.resource of
        "" -> send_stanza(FromJID, ToUser, ToServer, Stanza);
        ToResource -> send_stanza(FromJID, ToUser, ToServer, ToResource, Stanza)
    end.

send_stanza(FromJID, ToUser, ToServer, Stanza) ->
    case ejabberd_sm:get_user_resources(ToUser, ToServer) of
        [] -> send_stanza(FromJID, ToUser, ToServer, "", Stanza);
        Resources ->
            lists:foreach(
                fun(ToResource) ->
                  send_stanza(FromJID, ToUser, ToServer, ToResource, Stanza)
                end,
                Resources
            )
    end.

send_stanza(FromJID, ToUser, ToServer, ToResource, Stanza) ->
    ToJID = jlib:make_jid(ToUser, ToServer, ToResource),
    ejabberd_router:route(FromJID, ToJID, Stanza).

parse_stanza(Stanza) ->
    StanzaPacket = xml_stream:parse_element(Stanza),
    case StanzaPacket of
        {xmlelement, _, _, _} -> StanzaPacket;
        _ -> false
    end.


%% -----------------------------------------------------------------------------
%% Roster
get_roster(User, Server) ->

  %% @TODO
  %% should use hook to get roster here - no hassling with modules:
  %% case catch ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US]) of
  %%	Items when is_list(Items) ->

    Modules = gen_mod:loaded_modules(Server),
    Roster = case lists:member(mod_roster, Modules) of
        true ->
            mod_roster:get_user_roster([], {User, Server});
        false ->
            case lists:member(mod_roster_odbc, Modules) of
                true ->
                    mod_roster_odbc:get_user_roster([], {User, Server});
                false ->
                    {error, "Neither mod_roster or mod_roster_odbc are enabled"}
            end
    end,
    {ok, Roster}.

make_roster_xmlrpc(Roster) ->
    lists:foldl(
      fun(Item, Res) ->
        JIDS = jlib:jid_to_string(Item#roster.jid),
        Nick = Item#roster.name,
        Groups = case Item#roster.groups of
         [] -> [""];
         Gs -> Gs
           end,
        ItemsX = [{struct, [{jid, JIDS}, {nick, Nick}, {group, Group}]}
      || Group <- Groups],
        ItemsX ++ Res
      end,
      [],
      Roster).


%% Array of Struct(String jid, String group, String nick, String subscription,
%%        String pending,
%%        Array of Struct(String resource, String show, String status, Integer priority))
make_roster_with_presence_xmlrpc(Roster) ->
?DEBUG("~p~n",[Roster]),
    lists:foldl(
      fun(Item, Aggregation) ->
        {roster, _,  _, Jid_r, Name, Subscription, Ask, Groups, _, _} = Item,
        Jid = jlib:make_jid(Jid_r),
        Jid_s = jlib:jid_to_string(Jid),
        User = Jid#jid.luser,
        Server = Jid#jid.lserver,
        GroupList = case Groups of
          [] -> [""];
          Gs -> Gs
        end,
        Presence = rostbeef_presence_tracker:dirty_read_presence(User, Server),
        P = rostbeef_utils:make_presence_xmlrpc_with_resource(Presence),
        ItemsX = [{struct, [
          {jid, Jid_s},
          {group, Group},
          {nick, Name},
          {subscription, erlang:atom_to_list(Subscription) },
          {pending, erlang:atom_to_list(Ask) },
          {presence, {array, P} }
          ]} || Group <- GroupList],
        ItemsX ++ Aggregation
      end,
      [],
      Roster).

%% -----------------------------------------------------------------------------
%% Presence hooks

%% called by set_presence
presence_set(User, Server, Resource, {xmlelement, _, _Attrs , _SubEls }=Packet) ->
    {Show, Status, Priority} = rostbeef_utils:extract_presence_state(Packet),
    ?INFO_MSG("beforeSET: ~p@~p~p", [User, Server, rostbeef_presence_tracker:read_presence(User, Server)]),
    rostbeef_presence_tracker:write_presence(User, Server, Resource , Show, Status, Priority),
    ?INFO_MSG("afterSET: ~p@~p~p", [User, Server, rostbeef_presence_tracker:read_presence(User, Server)]),
    none.

%% called by unset_presence
presence_unset(User, Server, Resource, Status) when is_list(Status) ->
    ?INFO_MSG("beforeUNSET: ~p@~p~p", [User, Server, rostbeef_presence_tracker:read_presence(User, Server)]),
    rostbeef_presence_tracker:delete_presence(User, Server, Resource),
    ?INFO_MSG("afterUNSET: ~p@~p~p", [User, Server, rostbeef_presence_tracker:read_presence(User, Server)]),
    none.

%% -----------------------------------------------------------------------------
%% Lists
get_attrs(Attribute_names, L) ->
    [get_attr(A, L) || A <- Attribute_names].

get_attr(A, L) ->
    case lists:keysearch(A, 1, L) of
  {value, {A, Value}} -> Value;
  false ->
      %% Report the error and then force a crash
      ?ERROR_MSG("Attribute '~p' not found on the list of attributes provided on the call:~n ~p", [A, L]),
      attribute_not_found = A
    end.

%%% ---------------------
%%% SUBSCRIPTION HANDLING
%%% ---------------------

link_contacts(Jid1_s, Nick1, Jid2_s, Nick2) ->
    Jid1 = jlib:string_to_jid(Jid1_s),
    Jid2 = jlib:string_to_jid(Jid2_s),
    case subscribe(Jid1, Jid2, Nick2, both, []) of
        ok ->
            push_item(Jid1,
                      {xmlelement, "item",
                       [{"jid", Jid2_s},
                        {"name", Nick2},
                        {"subscription", "both"}],
                       []}),
            case subscribe(Jid2, Jid1, Nick1, both, []) of
                ok ->
                    push_item(Jid2,
                              {xmlelement,"item",
                               [{"jid",Jid1_s},
                                {"name",Nick1},
                                {"subscription", "both"}],
                               []}),
                    ok;
                Error2 ->
                    {error, Error2}
            end;
        Error1 ->
            {error, Error1}
    end.

subscribe(Jid1, Jid2, Name, SubscriptionType, Groups) ->
    case ejabberd_auth:is_user_exists(Jid1#jid.luser, Jid1#jid.lserver) of
        true ->
            mnesia:dirty_write(roster, #roster{usj={Jid1#jid.luser, Jid1#jid.lserver,
                                                    {Jid2#jid.luser, Jid2#jid.lserver, Jid2#jid.lresource}},
                                               us={Jid1#jid.luser, Jid1#jid.lserver},
                                               jid={Jid2#jid.luser, Jid2#jid.lserver, Jid2#jid.lresource},
                                               name=Name,
                                               subscription=SubscriptionType,
                                               groups=Groups});
        _ ->
            not_exists
    end.

unlink_contacts(Jid1_s, Jid2_s) ->
    Jid1 = jlib:string_to_jid(Jid1_s),
    Jid2 = jlib:string_to_jid(Jid2_s),
    case unsubscribe(Jid1, Jid2) of
        ok ->
            push_item(Jid1,
                      {xmlelement,"item",
                       [{"jid",Jid2_s},
                        {"subscription", "remove"}],
                       []}),

            case unsubscribe(Jid2, Jid1) of
                ok ->
                    push_item(Jid2,
                              {xmlelement,"item",
                               [{"jid",Jid1_s},
                                {"subscription", "remove"}],
                               []}),

                    ok;
                Error2 ->
                    {error, Error2}
            end;
        Error1 ->
            {error, Error1}
    end.

unsubscribe(Jid1, Jid2) ->
    case ejabberd_auth:is_user_exists(Jid1#jid.luser, Jid1#jid.lserver) of
        true ->
            mnesia:dirty_delete(roster, {Jid1#jid.luser, Jid1#jid.lserver,
                                         {Jid2#jid.luser, Jid2#jid.lserver, Jid2#jid.lresource}});
        _ ->
            not_exists
    end.

push_item(Jid, Item) ->
    BareJid = jlib:jid_remove_resource(Jid),
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
                id = "push",
                sub_el = [{xmlelement, "query",
                           [{"xmlns", ?NS_ROSTER}],
                           [Item]}]},
    ejabberd_router:route(
      BareJid,
      BareJid,
      jlib:iq_to_xml(ResIQ)).
