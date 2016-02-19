%%
%% @author Mark Sebald
%% @doc Block State Type Definition  
%% Description:  Block State is used in many places, 
%%               Define it once here.
%% 

-type block_state() :: { 
                         BlockName :: atom(), 
                         BlockModule :: module(), 
                         Configs :: list(), 
                         Inputs :: list(), 
                         Outputs :: list(), 
                         Internals :: list() 
                       }.

%%