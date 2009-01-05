-module(rss_module_test).

-export([start/0]).

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/3, loaded_ok/2]).

-include("rss.hrl").

start() ->
    plan(15),
    loaded_ok(rss, "the rss module has loaded ok"),
    can_ok(rss, process_rss, 1),
    can_ok(rss, process_rss, 2),
    test_channels(2),
    test_items(2),
    test_channels(0.92),
    test_items(0.92),
    test_channels(0.91),
    test_items(0.91),
    etap:end_tests().

test_channels(Vsn) ->
    etap:diag(io_lib:format("Testing channel retrieval for version: ~w", [Vsn])),
    C = rss:process_rss(rss_fixture(Vsn), channels),
    ok(is_list(C), "we got back a list"),
    ok(lists:all(fun(Item) -> is_record(Item, channel) end, C),
        "all the items were channel records").

test_items(Vsn) ->
    etap:diag(io_lib:format("Testing items retrieval for version: ~w", [Vsn])),
    I = rss:process_rss(rss_fixture(Vsn), items),
    ok(is_list(I), "we got back another list"),
    ok(lists:all(fun(Item) -> is_record(Item, rssitem) end, I),
        "all the items were rssitem records").

rss_fixture(Vsn) ->
    case Vsn of
        2 ->
            {ok, Result} = file:read_file("../priv/sample-rss-2.xml");
        0.92 ->
            {ok, Result} = file:read_file("../priv/sample-rss-092.xml");
        0.91 ->
            {ok, Result} = file:read_file("../priv/sample-rss-091.xml")
    end,
    Result.
            

