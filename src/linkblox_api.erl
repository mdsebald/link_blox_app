%%% @doc 
%%% API Server.  gen_server behavior to handle requests from UI clients
%%%
%%% @end

-module(linkblox_api).

-author("Mark Sebald").

-include("block_state.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          start/0,
          stop/1,
          create_block/4,
          create_block/2,
          copy_block/4,
          delete_block/2,
          get_block/2,
          get_value/3,
          get_block_names/1,
          get_types_info/1,
          get_type_info/2,
          get_exec_links/1,
          set_value/4,
          add_link/4,
          del_link/4,
          add_exec_link/3,
          del_exec_link/3,
          get_blocks/1,
          save_blocks/2,
          save_blocks/3,
          load_block_file/2,
          load_block_data/2,
          execute_block/3,
          is_block_name/2, 
          is_block_type/2
]). 


%% Start the LinkBlox API Server
start()->
  gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).


%% Stop the LinkBlox API server
stop(Node) ->
  gen_server:call({?MODULE, Node}, stop).


%%
%% Process API calls
%%

%% Create a block named BlockNameStr of type BlockModuleStr, 
-spec create_block(Node :: node(),
                   BlockModuleStr :: string(),
                   BlockNameStr :: string(),
                   Description :: string()) -> term().

create_block(Node, BlockModuleStr, BlockNameStr, Description) ->
  gen_server:call({?MODULE, Node}, 
                  {create_block, BlockModuleStr, BlockNameStr, Description}).


%% create a block from a set of existing block values
-spec create_block(Node :: node(),
                   BlockDefn :: block_defn()) -> term().

create_block(Node, BlockDefn) ->
  gen_server:call({?MODULE, Node}, 
                  {create_block, BlockDefn}).


%% Create a copy of a block
-spec copy_block(Node :: node(),
                 BlockNameStr :: string(),
                 BlockDefn :: block_defn(),
                 InitAttribs :: list()) -> term().

copy_block(Node, BlockNameStr, BlockDefn, InitAttribs) ->
  gen_server:call({?MODULE, Node}, 
                  {copy_block, BlockNameStr, BlockDefn, InitAttribs}).


%% Delete a block
-spec delete_block(Node :: node(),
                   BlockNameStr :: string()) -> term().

delete_block(Node, BlockNameStr) ->
  gen_server:call({?MODULE, Node}, 
                  {delete_block, BlockNameStr}).


%% Get block values
-spec get_block(Node :: node(),
                BlockNameStr :: string()) -> term().

get_block(Node, BlockNameStr) ->
  gen_server:call({?MODULE, Node}, {get_block, BlockNameStr}).


%% Get a block value
-spec get_value(Node :: node(),
                BlockNameStr :: string(),
                ValueIdStr :: string()) -> term().

get_value(Node, BlockNameStr, ValueIdStr) ->
  gen_server:call({?MODULE, Node}, {get_value, BlockNameStr, ValueIdStr}).


%% Get list of block names
-spec get_block_names(Node :: node()) -> [string()].

get_block_names(Node) ->
  gen_server:call({?MODULE, Node}, get_block_names).


%% Get list of block types information
-spec get_types_info(Node :: node()) -> term().

get_types_info(Node) ->
  gen_server:call({?MODULE, Node}, get_types_info).


%% Get the block type information for the given block
-spec get_type_info(Node :: node(),
                    BlockNameStr :: string()) -> term().

get_type_info(Node, BlockNameStr) ->
  gen_server:call({?MODULE, Node}, {get_type_info, BlockNameStr}).


%% Get a list of the exec_links on this node
-spec get_exec_links(Node :: node()) -> term().

get_exec_links(Node) ->
  gen_server:call({?MODULE, Node}, get_exec_links).


%% Set a block value
-spec set_value(Node :: node(),
                BlockNameStr :: string(),
                ValueIdStr :: string(),
                Value :: value()) -> ok | {error, atom()}.

set_value(Node, BlockNameStr, ValueIdStr, Value) ->
  gen_server:call({?MODULE, Node}, {set_value, BlockNameStr, ValueIdStr, Value}).


%% Link a block output to a block input
-spec add_link(Node :: node(),
               OutputBlockNameStr :: string(),
               OutputValueIdStr :: string(),
               LinkStrs :: [string()]) -> term().

add_link(Node, OutputBlockNameStr, OutputValueIdStr, LinkStrs) ->
  gen_server:call({?MODULE, Node}, {add_link, OutputBlockNameStr, OutputValueIdStr, LinkStrs}).


%% Unlink a block output from a block input
-spec del_link(Node :: node(),
               OutputBlockNameStr :: string(),
               OutputValueIdStr :: string(),
               LinkStr :: [string()]) -> term().

del_link(Node, OutputBlockNameStr, OutputValueIdStr, LinkStrs) ->
  gen_server:call({?MODULE, Node}, {del_link, OutputBlockNameStr, OutputValueIdStr, LinkStrs}).


%% Create execution link from Executor to Executee block
-spec add_exec_link(Node :: node(),
                    ExecutorBlockNameStr :: string(),
                    ExecuteeBlockNameStr :: string()) -> term().

add_exec_link(Node, ExecutorBlockNameStr, ExecuteeBlockNameStr) ->
  gen_server:call({?MODULE, Node}, {add_exec_link, ExecutorBlockNameStr, ExecuteeBlockNameStr}).


%% Delete execution link from Executor to Executee block
-spec del_exec_link(Node :: node(),
                    ExecutorBlockNameStr :: string(),
                    ExecuteeBlockNameStr :: string()) -> term().

del_exec_link(Node, ExecutorBlockNameStr, ExecuteeBlockNameStr) ->
  gen_server:call({?MODULE, Node}, {del_exec_link, ExecutorBlockNameStr, ExecuteeBlockNameStr}).


%% Get all of the current created blocks on this node 
-spec get_blocks(Node :: node()) -> term().
 
get_blocks(Node) ->
  gen_server:call({?MODULE, Node}, get_blocks).


%% Save the current created blocks on this node, to FileName on this node 
-spec save_blocks(Node :: node(),
                  FileName :: string()) -> term().
 
save_blocks(Node, FileName) ->
  gen_server:call({?MODULE, Node}, {save_blocks, FileName}).


%% Save the Block Data, to FileName on this node 
-spec save_blocks(Node :: node(),
                  FileName :: string(),
                  BlockData :: term()) -> term().
 
save_blocks(Node, FileName, BlockData) ->
  gen_server:call({?MODULE, Node}, {save_blocks, FileName, BlockData}).


%% Load the blocks stored in FileName on this node  
-spec load_block_file(Node :: node(),
                      FileName :: string()) -> term().
 
load_block_file(Node, FileName) ->
  gen_server:call({?MODULE, Node}, {load_block_file, FileName}).


%% Load this Block Data onto this node 
-spec load_block_data(Node :: node(),
                      BlockData :: term()) -> term().
 
load_block_data(Node, BlockData) ->
  gen_server:call({?MODULE, Node}, {load_block_data, BlockData}).


%% Execute the block
-spec execute_block(Node :: node(),
                    BlockNameStr :: string(),
                    Reason :: exec_method()) -> term().

execute_block(Node, BlockNameStr, Reason) ->
  gen_server:call({?MODULE, Node}, {execute_block, BlockNameStr, Reason}).


%% Is BlockName a valid block name?
-spec is_block_name(Node :: node(),
                    BlockNameStr :: string()) -> term().

is_block_name(Node, BlockNameStr) ->
  gen_server:call({?MODULE,Node}, {is_block_name, BlockNameStr}).

% TODO: Not Used? 
%% Is BlockTypeStr a valid block type name?
-spec is_block_type(Node :: node(),
                    BlockTypeStr :: string()) -> term().

is_block_type(Node, BlockTypeStr) ->
  gen_server:call({?MODULE, Node}, {is_block_type, BlockTypeStr}).

%command(Command, Args)->
 % gen_server:call(?MODULE, {comand, Command, Args}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
  

%% ==================================================================== 
%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
  Result :: {ok, State}
      | {ok, State, Timeout}
      | {ok, State, hibernate}
      | {stop, Reason :: term()}
      | ignore,
  State :: term(),
  Timeout :: non_neg_integer() | infinity.

init(null) ->
  logger:info(starting_linkblox_API_server),
  {ok, []}.

%% ====================================================================
%% handle_call/3
%% ====================================================================
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
  Result :: {reply, Reply, NewState}
      | {reply, Reply, NewState, Timeout}
      | {reply, Reply, NewState, hibernate}
      | {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason, Reply, NewState}
      | {stop, Reason, NewState},
  Reply :: term(),
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().


%% =====================================================================
%% Stop the API server
%% =====================================================================
handle_call(stop, _From, State) ->  
  logger:info(stopping_linkblox_API_server),

  {stop, normal, ok, State};

%% =====================================================================
%% Create a block from block type and block name
%% =====================================================================    
handle_call({create_block, BlockModuleStr, BlockNameStr, Description}, From, State) ->
  logger:debug("API, create_block, From: ~p, Type: ~s, Name: ~s, Descr: ~s", 
               [From, BlockModuleStr, BlockNameStr, Description]),
  BlockName = list_to_atom(BlockNameStr),

  Result = case block_supervisor:is_block(BlockName) of
    false ->
      BlockModule = type_utils:type_module(BlockModuleStr),
      % Check if a module of this name exists
      case type_utils:module_exists(BlockModule) of
        true ->
          try BlockModule:create(BlockName, Description) of
            BlockDefn -> block_utils:create_block(BlockDefn),
            ok
          catch
            error:{undef, Function} -> 
              logger:error("Invalid block module: ~p, ", [Function]),
              {error, invalid_block_type}
          end;

        false -> {error, invalid_block_type}
      end;
    true -> {error, block_exists}
  end,
  {reply, Result, State};


%% =====================================================================
%% Create a block from a set of existing block values
%% =====================================================================    
handle_call({create_block, BlockDefn}, _From, State) ->
  Result = block_utils:create_block(BlockDefn),
  {reply, Result, State};


%% =====================================================================
%% Copy a block
%% =====================================================================    
% TODO: Set initial attribute values
handle_call({copy_block, BlockNameStr, BlockState, _InitAttribs}, _From, State) ->
  % Make sure the block values to be copied are the correct form
  case BlockState of
    {Config, Inputs, Outputs} ->
      % Get the block module from the copied config values 
      case attrib_utils:get_value(Config, block_module) of
        {ok, BlockModule} ->
          % Make sure the block type to be copied, exists on this node
          case lists:member(BlockModule, type_utils:modules()) of
            true ->
              % Make sure the block name is not already used
              BlockName = list_to_atom(BlockNameStr),
              case block_supervisor:is_block(BlockName) of
                false ->
                  % Create the block, but ignore the created block state
                  % Use the source block state for the new block state
                  BlockModule:create(BlockName, "Default Comment"),
                  % Change the name in the copied block values, to the new block name
                  {ok, NewConfig} = attrib_utils:set_value(Config, block_name, BlockName),
                  % Set the copied inputs, to their default values
                  DefaultInputs = input_utils:set_to_defaults(Inputs),
                  % Set all output values of this block to 'empty'. Status is created
                  EmptyOutputs = output_utils:update_all_outputs(Outputs, empty, created),
                  % Start the new block with the set of copied values, 
                  case block_supervisor:start_block({NewConfig, DefaultInputs, EmptyOutputs}) of
                    {ok, _Pid} -> 
                      Result = ok;
                    {error, Reason} -> 
                      Result = {error, Reason}
                  end;
                _ ->
                  Result = {error, block_exists}
              end;
            _ ->
              Result = {error, invalid_block_type}
          end;
        _ ->
          Result = {error, invalid_config_valus}  
      end;
    _ ->
      Result = {error, invalid_block_values}
  end,  
  {reply, Result, State};

%% =====================================================================
%% Delete a block
%% =====================================================================    
handle_call({delete_block, BlockNameStr}, _From, State) ->
  BlockName = list_to_atom(BlockNameStr),
  Result = case block_supervisor:is_block(BlockName) of
    true -> block_supervisor:delete_block(BlockName);
  
    false -> {error, block_not_found}
  end,
  {reply, Result, State};


%% =====================================================================
%% Get all block values
%% =====================================================================    
handle_call({get_block, BlockNameStr}, _From, State) ->
  BlockName = list_to_atom(BlockNameStr),
  case block_supervisor:is_block(BlockName) of
    true ->
      {Config, Inputs, Outputs, _Private} = block_server:get_block(BlockName),
      % Strip private values,
      Result = {ok, {Config, Inputs, Outputs}};
    false -> 
      Result = {error, block_not_found}
  end,
  {reply, Result, State};


%% =====================================================================
%% Get a block value
%% =====================================================================    
handle_call({get_value, BlockNameStr, ValueIdStr}, _From, State) ->
  BlockName = list_to_atom(BlockNameStr),
  Result = case block_supervisor:is_block(BlockName) of
    true ->
      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId} ->
          block_server:get_value(BlockName, ValueId);

        {error, Reason} -> {error, Reason}
      end;
    false -> {error, block_not_found}
  end,

  {reply, Result, State};


%% =====================================================================
%% Get a list of all block names
%% =====================================================================    
handle_call(get_block_names, From, State) ->
  logger:debug("API, get_block_names, From: ~p", [From]),
  BlockNames = block_supervisor:block_names(),
  % Convert blocknames to strings
  Result = lists:map(fun(BlockName) -> atom_to_list(BlockName) end, BlockNames),
  {reply, Result, State};


%% =====================================================================
%% Get a list of all block types information 
%% =====================================================================    
handle_call(get_types_info, _From, State) ->
  Result = type_utils:types_info(),
  {reply, Result, State};


%% =====================================================================
%% Get the type info for the given block name 
%% =====================================================================    
handle_call({get_type_info, BlockNameStr}, _From, State) ->
  % Get the block_module (i.e. block code), for the given block
  % Block type info is in there.
  BlockName = list_to_atom(BlockNameStr),
  case block_server:get_value(BlockName, block_module) of
    {ok, BlockModule} ->
      Result = type_utils:type_info(BlockModule);

    {error, Reason} ->
      Result = {error, Reason}  
  end,
  {reply, Result, State};


%% =====================================================================
%% Get the list of exec_links on this node
%% =====================================================================    
handle_call(get_exec_links, _From, State) ->

  Result = block_supervisor:get_exec_links(), 
  {reply, Result, State};


%% =====================================================================
%% Set a block value 
%% =====================================================================    
handle_call({set_value, BlockNameStr, ValueIdStr, Value}, _From, State) ->
  BlockName = list_to_atom(BlockNameStr),
  Result = case block_supervisor:is_block(BlockName) of
    true ->
      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId} ->
          case read_only_attrib(ValueId) of
            false -> block_server:set_value(BlockName, ValueId, Value);

            true -> {error, read_only}
          end;

        {error, Reason} -> {error, Reason}
      end;
    false -> {error, block_not_found}
  end,
  {reply, Result, State};  


%% =====================================================================
%% Add a link to an input value from a block output
%% =====================================================================    
handle_call({add_link, OutputBlockNameStr, OutputValueIdStr, LinkStrs}, _From, State) ->
OutputBlockName = list_to_atom(OutputBlockNameStr),
Result = case block_supervisor:is_block(OutputBlockName) of
    true ->
      case parse_link(LinkStrs) of
        {ok, Link} ->
          case link_utils:validate_link(Link) of
            ok ->
              case attrib_utils:str_to_value_id(OutputValueIdStr) of
                {ok, OutputValueId} ->
                  block_server:add_link(OutputBlockName, OutputValueId, Link);

                {error, Reason} -> {error, Reason}
              end;

            {error, Reason} -> {error, Reason}
          end;

        {error, Reason} -> {error, Reason}
      end;

    false -> {error, block_not_found}
  end,      
  {reply, Result, State};


%% =====================================================================
%% Delete a link to an input value from a block output
%% =====================================================================    
handle_call({del_link, OutputBlockNameStr, OutputValueIdStr, LinkStrs}, _From, State) ->
  OutputBlockName = list_to_atom(OutputBlockNameStr),
  Result = case block_supervisor:is_block(OutputBlockName) of
    true ->
      case parse_link(LinkStrs) of
        {ok, Link} ->
          case link_utils:validate_link(Link) of
            ok -> 
              case attrib_utils:str_to_value_id(OutputValueIdStr) of
                {ok, OutputValueId} ->
                  block_server:del_link(OutputBlockName, OutputValueId, Link);

                {error, Reason} -> {error, Reason}
              end;

            {error, Reason} -> {error, Reason}
          end;

        {error, Reason} -> {error, Reason}
      end;
      
    false -> {error, block_not_found}        
  end,
  {reply, Result, State};  


%% =====================================================================
%% Create execution link from Executor to Executee block
%% =====================================================================    
handle_call({add_exec_link, ExecutorBlockNameStr, ExecuteeBlockNameStr}, _From, State) ->
  ExecutorBlockName = list_to_atom(ExecutorBlockNameStr),
  Result = case block_supervisor:is_block(ExecutorBlockName) of
    true ->
      ExecuteeBlockName = list_to_atom(ExecuteeBlockNameStr),
      % Execution links are hard coded from exec_out output to exec_in input attributes
      case link_utils:validate_link({ExecuteeBlockName, exec_in}) of
        ok ->
          block_server:add_exec_link(ExecutorBlockName, ExecuteeBlockName);

        {error, Reason} -> {error, Reason}
      end;
    false -> {error, block_not_found}
  end,      
  {reply, Result, State};


%% =====================================================================
%% Delete execution link from Executor to Executee block
%% =====================================================================    
handle_call({del_exec_link, ExecutorBlockNameStr, ExecuteeBlockNameStr}, _From, State) ->
  ExecutorBlockName = list_to_atom(ExecutorBlockNameStr),
  Result = case block_supervisor:is_block(ExecutorBlockName) of
    true ->
      ExecuteeBlockName = list_to_atom(ExecuteeBlockNameStr),
      % Execution links are hard coded from exec_out output to exec_in input attributes
      case link_utils:validate_link({ExecuteeBlockName, exec_in}) of
        ok ->
          block_server:del_exec_link(ExecutorBlockName, ExecuteeBlockName);

        {error, Reason} -> {error, Reason}
      end;
    false -> {error, block_not_found}
  end,      
  {reply, Result, State};


%% =====================================================================
%% Execute block
%% =====================================================================    
handle_call({execute_block, BlockNameStr, Reason}, _From, State) ->
  BlockName = list_to_atom(BlockNameStr),
  Result = case block_supervisor:is_block(BlockName) of
    true ->
      block_server:execute(BlockName, Reason),
      ok;

    false -> {error, block_not_found}
  end,

  {reply, Result, State};


%% =====================================================================
%% Is BlockNameStr a valid block name?
%% =====================================================================    
handle_call({is_block_name, BlockNameStr}, _From, State) ->
  BlockName = list_to_atom(BlockNameStr),
  Result = block_supervisor:is_block(BlockName),
  {reply, Result, State};


%% =====================================================================
%% Is BlockTypeStr a valid block type?
%% =====================================================================    
handle_call({is_block_type, BlockTypeStr}, _From, State) ->
  Result = case ui_utils:get_block_type_strings(BlockTypeStr) of
    {_Module, BlockTypeStr, _Description} -> true;
                                    false -> false
  end,
  {reply, Result, State};


%% =====================================================================
%% Get current values for all of the blocks on this node
%% ===================================================================== 
   
handle_call(get_blocks, _From, State) ->
  Result = block_utils:get_blocks_to_save(),
  {reply, Result, State};


%% =====================================================================
%% Save current values for all of the blocks on this node,
%%  to a file on this node.
%% ===================================================================== 
   
handle_call({save_blocks, FileName}, _From, State) ->
  BlockData = block_utils:get_blocks_to_save(),
  Result = block_utils:save_blocks_to_file(FileName, BlockData),
  {reply, Result, State};

%% =====================================================================
%% Save the Block Data to a file on this node.
%% ===================================================================== 
   
handle_call({save_blocks, FileName, BlockData}, _From, State) ->
  Result = block_utils:save_blocks_to_file(FileName, BlockData),
  {reply, Result, State};


%% =====================================================================
%% Load the block data in Filename, on this node, onto this node
%% ===================================================================== 
   
handle_call({load_block_file, FileName}, _From, State) ->
  Result = block_utils:load_blocks_from_file(FileName),
  {reply, Result, State};


%% =====================================================================
%% Load the Block Data onto this node.
%% ===================================================================== 
   
handle_call({load_block_data, BlockData}, _From, State) ->
  Result = block_utils:load_blocks(BlockData),
  {reply, Result, State};


%% =====================================================================
%% Unknown Call message
%% =====================================================================      
handle_call(Request, From, State) ->
  logger:warning(linkblox_api_unknown_call_msg_from, [Request, From]),
  {reply, ok, State}.


%% ====================================================================
%% handle_cast/2
%% ====================================================================
-spec handle_cast(Request :: term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
     | {noreply, NewState, Timeout}
     | {noreply, NewState, hibernate}
     | {stop, Reason, NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().


%% =====================================================================
%% Unknown Cast message
%% =====================================================================      
handle_cast(Msg, State) ->
  logger:warning(linkblox_api_unknown_cast_msg, [Msg]),
  {noreply, State}.


%% ====================================================================
%% handle_info/2
%% ====================================================================
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.


%% =====================================================================
%% Unknown Info message
%% =====================================================================
handle_info(Info, State) ->
  logger:warning(linkblox_api_unknown_info_msg, [Info]),
  {noreply, State}.


%% ====================================================================
%% terminate/2
%% ====================================================================
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
      | shutdown
      | {shutdown, term()}
      | term().
      
terminate(normal, _State) ->
  ok;
    
terminate(Reason, _State) ->
  logger:error(linkblox_API_server_abnormal_termination, [Reason]),
  ok.


%% ====================================================================
%% code_change/3
%% ====================================================================
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
  Result :: {ok, NewState :: term()} | {error, Reason :: term()},
  OldVsn :: Vsn | {down, Vsn},
  Vsn :: term().
  
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


%%
%% Is ValueId a read only attribute?
%%
-spec read_only_attrib(ValueId :: value_id()) -> boolean().

read_only_attrib(ValueId) ->
  case ValueId of
    % So far no array values are read only
    {_ValueName, _Index} ->
      false;
    ValueName ->
      case ValueName of
        block_name   -> true;
        block_module -> true;
        version      -> true;
                   _ -> false
      end
  end.

%%
%% Parse a list of strings into a Link tuple
%%
parse_link(LinkStrs) ->

  % Links should consist of a Block Name and Value ID
  case LinkStrs of
    [BlockNameStr, ValueIdStr] ->
      BlockName = list_to_atom(BlockNameStr),

      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId}   -> {ok, {BlockName, ValueId}};
        {error, Reason} -> {error, Reason}
      end;

    _Invalid -> {error, invalid}
  end.


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-ifdef(UNDER_DEVELOPMENT).

linkblox_api_test_() ->
  {
    setup,
    fun start_linkblox_api/0,
    fun stop_linkblox_api/1,
    fun() ->
      {
        inorder,
         [
          create_block_test(),
          delete_block_test()
        ]
      }
    end
  }.

start_linkblox_api() ->
  % Need to start LinkBlox Supervisor
  % That will start the API Server, (i.e. this module) and the block supervisor
  % which is needed for some calls
  % TODO: May not be able to get this to work.  
  %   node() returns 'nonode@nohost'. May not be valid  
  linkblox_supervisor:start_link("NoFile"). 
 
stop_linkblox_api(_) ->
  stop(node()).

create_block_test() ->
  create_block(node(), "template", "test_block", []).

delete_block_test() ->
  delete_block(node(), "test_block"). 

-endif.
-endif.