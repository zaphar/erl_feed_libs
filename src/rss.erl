%% @doc RSS (Really Simple Syndication) parsing library for erlang.
%%  This library does not parse RDF which is sometimes mistaken for RSS.<br />
%%  <br /> 
%%  To get the record specifications include the include/rss.hrl header
%%  file in your code.
%% @author "Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>"
-module(rss).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").

-import(xmerl).
-import(xmerl_scan).
-import(xmerl_xpath).

-include_lib("xmerl/include/xmerl.hrl").
-include("rss.hrl").
%% @headerfile "../include/rss.hrl" 

-export([process_rss/1, process_rss/2]).

%% @doc parse out an rss feed
%% @spec process_rss(S) -> [I]
%%  S = list() | binary()
%%  I = [channel()]
process_rss(S) when is_list(S) ->
    process_rss(S, channels).

%% @doc parse out an rss feed.
%% You can request either a list of the channels in an rss document,
%% or you can request a list of all the items in an rss document
%% @spec process_rss(S, Type) -> [I]
%%  S = list() | binary()
%%  Type = channels | items
%%  I = rssitem() | channel()
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
  Author = xpath_text_content("//author", I),
  Category = xpath_content_list("//category", I),
  Comments = xpath_text_content("//comments", I),
  Enclosure = xpath_content_list("//enclosure", I),
  Source = xpath_content_list("//source", I),
  %% fun to turn xmlElments into category records
  ToCatRecord = fun(C) ->
    Value = text_content(C#xmlElement.content),
    Attrs = C#xmlElement.attributes,
    Domain = get_attribute(Attrs, domain),
    #rsscategory{value=Value, domain=Domain}
  end,
  
  ToEnclosureRecord = fun(E) ->
    Attrs = E#xmlElement.attributes,
    Url = get_attribute(Attrs, url), 
    Length = get_attribute(Attrs, length), 
    Type = get_attribute(Attrs, type),
    #rssenclosure{url=Url, length=Length, type=Type}
  end,
  
  ToSourceRecord = fun(S) ->
    Value = text_content(S#xmlElement.content),
    Attrs = S#xmlElement.attributes,
    Url = get_attribute(Attrs, url),
    #rsssource{value=Value, url=Url}
  end,

  CatList = [ToCatRecord(C) || C <- Category ],

  ToRecord = fun(Fun, Default, List) ->
    case [Fun(E) || E <- List] of
      [] ->
          Default;
      [H | _] ->
          H
    end
  end,
  
  EnclosureRecord = ToRecord(ToEnclosureRecord, #rssenclosure{}, Enclosure), 
 
  SourceRecord = ToRecord(ToSourceRecord, #rsssource{}, Source), 

  #rssitem{uri=Link, title=Title, desc=Desc, author=Author,
           pub_date=Date, guid=Guid,
           category=CatList, comments=Comments,
           enclosure=EnclosureRecord, source=SourceRecord}.

%% @private
xpath_first_item(Str, R) when is_record(R, xmlElement) ->
  case xmerl_xpath:string(Str, R) of
  [H |_] ->
    H;
  [] ->
    #xmlElement{}
  end.

text_content(R) ->
  lists:flatten([T#xmlText.value || T <- R, is_record(T, xmlText)]).
    
%% @private
xpath_text_content(Str, R) when is_record(R, xmlElement) ->
  text_content(xpath_content(Str, R)).

%% @private
xpath_content(Str, R) when is_record(R, xmlElement) ->
  Node  = xpath_first_item(Str, R),
  Node#xmlElement.content.

%% @private
xpath_content_list(Str, R) when is_record(R, xmlElement) ->
    case xmerl_xpath:string(Str, R) of
        List when is_list(List) ->
            List;
        [] ->
            [#xmlElement{}]
    end.

%% @private
get_attribute(List, Name) when is_list(List), is_atom(Name) ->
        Attributes = lists:filter(
            fun(A) ->
                A#xmlAttribute.name == Name 
             end,
        List),
        case Attributes of
            [] ->
                "";
            [H | _] ->
                H#xmlAttribute.value
        end.

%% @private
xml_doc(S) when is_list(S) ->
  {Doc, _} = xmerl_scan:string(S),
  Doc.

