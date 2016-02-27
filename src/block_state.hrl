%%% @doc 
%%% Block State Type Definition  
%%% Block State is used in many places, 
%%% Define it once here.
%%% @end 

-author("Mark Sebald").

-type block_state() :: { 
                         BlockName :: atom(), 
                         BlockModule :: module(), 
                         Config :: list(), 
                         Inputs :: list(), 
                         Outputs :: list(), 
                         Private :: list() 
                       }.
                       
%%
%% specifies an empty input value link
%% useful for initializing block inputs
%%
-define( EMPTY_LINK, {fixed, null, null}).             
