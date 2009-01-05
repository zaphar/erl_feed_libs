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

