-module(rss_module_test).

-export([start/0]).

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/3, loaded_ok/2]).

-include("rss.hrl").

start() ->
    plan(3),
    loaded_ok(rss, "the rss module has loaded ok"),
    can_ok(rss, process_rss, 1),
    can_ok(rss, process_rss, 2),
    test_channels(),
    test_items(),
    etap:end_tests().

test_channels() ->
    C = rss:process_rss(rss_fixture(2), channels),
    ok(is_list(C), "we got back a list"),
    is(length(C), 1, "there is only one element in the list"),
    [I | T] = C,
    ok(is_record(I, channel), "the element is an channel").

test_items() ->
    I = rss:process_rss(rss_fixture(2), items),
    ok(is_list(I), "we got back another list"),
    is(length(I), 4, "there were 4 elements in the list"),
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
            

