-module(rss).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").

-import(xmerl).
-import(xmerl_scan).
-import(xmerl_xpath).

-include_lib("xmerl/include/xmerl.hrl").

-include("rss.hrl").

-export([process_rss/1, process_rss/2]).

%% @doc parse out an rss feed
%% @spec process_rss(S) -> [I]
%%  S = list() | binary()
%%  I = rssitem()
process_rss(S) when is_list(S) ->
    process_rss(S, channels).

process_rss(S, Type) when is_binary(S) ->
    process_rss(binary_to_list(S), Type);
process_rss(S, channels) when is_list(S) ->
  Doc = xml_doc(S),
  lists:flatten([ parse_rss_channels(I) || I <- xmerl_xpath:string("//channel", Doc) ]);
process_rss(S, items) when is_list(S) ->
  Doc = xml_doc(S),
  lists:flatten([ parse_rss_item(I) || I <- xmerl_xpath:string("//item", Doc) ]).

%% @doc parse out an rss feeds channels
%% @spec parse_rss_channels(Rss) -> [C]
%%  Rss = string() | #xmlElement{}
%%  C = channel()
parse_rss_channels(L) when is_list(L) ->
  [parse_rss_channels(C) || C <- L]; 
parse_rss_channels(C) when is_record(C, xmlElement) ->
  Title  = xpath_text_content("//title", C),
  Desc   = xpath_text_content("//description", C),
  Uri   = xpath_text_content("//link", C),
  Itemlist = xmerl_xpath:string("//item", C),
  #channel{ uri=Uri, title=Title, desc=Desc, items=[parse_rss_item(I) || I <- Itemlist] }.

%% @private
parse_rss_item(I) when is_record(I, xmlElement) ->
  Link = xpath_text_content("//link", I), 
  Title = xpath_text_content("//title", I), 
  Desc = xpath_text_content("//description", I), 
  Date = xpath_text_content("//pubDate", I),
  Guid = xpath_text_content("//guid", I),
  #rssitem{uri=Link, title=Title, desc=Desc, pub_date=Date, guid=Guid}.

%% @private
xpath_first_item(Str, R) when is_record(R, xmlElement) ->
  case xmerl_xpath:string(Str, R) of
  [H |_] ->
    H;
  [] ->
    #xmlElement{}
  end.

%% @private
xpath_text_content(Str, R) when is_record(R, xmlElement) ->
  Text = xpath_content(Str, R),
  lists:flatten([T#xmlText.value || T <- Text]).

%% @private
xpath_content(Str, R) when is_record(R, xmlElement) ->
  Node  = xpath_first_item(Str, R),
  Node#xmlElement.content.

%% @private
xml_doc(S) when is_list(S) ->
  {Doc, _} = xmerl_scan:string(S),
  Doc.

