% {xmlelement,
%  "presence",
%  [],
%  [{xmlelement,
%       "show",
%       [],
%       [{xmlcdata,<<"away">>}]},
%   {xmlelement,
%       "status",
%       [],
%       [{xmlcdata,
%            <<"I'm not here right now">>}]},
%   {xmlelement,
%       "c",
%       [{"xmlns",
%         "http://jabber.org/protocol/caps"},
%        {"node","http://pidgin.im/caps"},
%        {"ver","2.5.1"},
%        {"ext",
%         "mood moodn nick nickn tune tunen avatarmeta avatardata avatar"}],
%       []}]}
%%% Presence Tracker
-module(rostbeef_utils).
-author('markus.knofe@zweitgeist.com').
-vsn('0.1').

%% export modul-interface
-export([extract_presence_state/1]).

-include("ejabberd.hrl").

%% extracts the Packetinfomations
%% return {Show, Status, Priority}
extract_presence_state(Packet) ->
    Show = case xml:get_subtag(Packet, "show") of
                false ->  "available";
                _ ->      xml:get_subtag_cdata(Packet, "show")
           end,
    Status = case xml:get_subtag(Packet, "status") of
                false ->  "";
                _ ->      xml:get_subtag_cdata(Packet, "status")
           end,
    Priority = case xml:get_subtag(Packet, "priority") of
                false ->  0;
                _ ->      xml:get_subtag_cdata(Packet, "priority")
           end,
    {Show, Status, Priority}.