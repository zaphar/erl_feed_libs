-module(rss_module_test).

-export([start/0]).

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/3, loaded_ok/2]).

start() ->
    plan(3),
    loaded_ok(rss, "the rss module has loaded ok"),
    can_ok(rss, process_rss, 1),
    can_ok(rss, parse_rss_channels, 1),
    etap:end_tests().
