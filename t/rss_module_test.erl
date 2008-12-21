-module(rss_module_test).

-export([start/0]).

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/3, loaded_ok/2]).

start() ->
    plan(1),
    loaded_ok(rss, "the rss module has loaded ok"),
    etap:end_tests().
