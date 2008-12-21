-module(rss).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").

-import(xmerl).
-import(xmerl_scan).
-import(xmerl_xpath).

-include_lib("xmerl/include/xmerl.hrl").

-export([process_rss/1, parse_rss_channels/1]).

%% @type channel() = #channel{
%%              uri=string(), title=string(), desc=string(), 
%%              items=Items,
%%              language=string(), copyright=string(), 
%%              managing_editor=string(), web_master=string(), 
%%              pub_date=string(), last_build_date=string(), category=rss_cat(),
%%              generator=string(), 
%%              docs=string(), cloud=rss_cloud(), ttl=number(),
%%              image=rss_image(), rating=string(), text_input=rss_text_input(), 
%%              skip_hours=rss_number_series(), skip_days=rss_number_series()}
%%  Items = [ rssitem() ].

%% @type rssitem() = #rssitem{
%%              uri=string(), title=string(), desc=string(), author=string(),
%%              pub_date=string(), enclosure=Enclosure, comments=string(),
%%              category=Category, guid=Guid, source=Source}
%%  Enclosure = rss_enclosure()
%%  Category = rss_cat()
%%  Guid = rss_guid().

%% @type rss_enclosure() = #rssenclosure{url=string(), length=string(), type=string()}.

%% @type rss_cat() = #rsscategory{domain=string(), value=string()}.

%% @type rss_guid() = #rssguid{isPermaLink=bool(), value=string()}.

%% @type rss_number_series() = [number()].

-record(channel, {  %% required elements
                    uri, title, desc, items, 
                    %% optional elements
                    language, copyright, managing_editor, web_master, 
                    pub_date, last_build_date, category, generator, 
                    docs, cloud, ttl, image, rating, text_input, 
                    skip_hours, skip_days}).

-record(rssitem, {  %% required elements
                    uri, title, desc, 
                    %% optional elements
                    author, category,  comments,
                    enclosure, guid, pub_date,  source}).

%% @doc parse out an rss feed
%% @spec process_rss(S::string()) -> [I]
%%  I = rssitem()
process_rss(S) when is_list(S) ->
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
  #rssitem{uri=Link, title=Title, desc=Desc, pub_date=Date}.

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

