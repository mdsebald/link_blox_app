%%% @doc 
%%% BLOCKTYPE
%%% Receive block values from other nodes
%%% DESCRIPTION
%%% Receive block values from other LinkBlox nodes,
%%% transmitted by send_values blocks.
%%% Receive block name must have the same name as the sending block.
%%% LINKS              
%%% @end 

-module(lblx_receive_values).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).
-export([handle_cast/2]).

groups() -> [input, comm].

version() -> "0.1.0".


-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_values, {1}} %| int | 1 | 1..99 |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
       {receive_values, [{null, []}]} %| any array | null | N/A |
    ]). 


%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: block_name(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: block_name(),
             Description :: string(),  
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs(), 
             InitOutputs :: output_attribs()) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs) ->

  % Update Default Config, Input, Output, and Private attribute values 
  % with the initial values passed into this function.
  %
  % If any of the intial attributes do not already exist in the 
  % default attribute lists, merge_attribute_lists() will create them.
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Upgrade block attribute values, when block code and block data versions are different
%% 
-spec upgrade(BlockDefn :: block_defn()) -> {ok, block_defn()} | {error, atom()}.

upgrade({Config, Inputs, Outputs}) ->
  
  ModuleVer = version(),
  {BlockName, BlockModule, ConfigVer} = config_utils:name_module_version(Config),
  BlockType = type_utils:type_name(BlockModule),

  case attrib_utils:set_value(Config, version, version()) of
    {ok, UpdConfig} ->
      logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  % Add a private attribute, to hold values published by send_values block types.
  Private1 = attrib_utils:add_attribute(Private, {publish_values, {[]}}),

  % Check the config values
  case config_utils:get_integer_range(Config, num_of_values, 1, 99) of
    {ok, NumOfValues} ->              
      % Create array of outputs to import values from other nodes
      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                    receive_values, NumOfValues, {null, []}),
      Value = true, Status = normal;
      
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, num_of_values, Reason),
      Outputs1 = Outputs
  end,
       
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % This is the block state
  {Config, Inputs, Outputs2, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, ExecMethod) ->
  % Only update outputs if we get a publish_values message
  case ExecMethod of
    message ->
      % Received updated values, reset the exec_interval timer, if set
      BlockName = config_utils:name(Config),
      {_Status, Private1} = block_common:update_execution_timer(BlockName, Inputs, Private), 
    
      % Received a publish message from send_values block type
      % Write the values to the recieve_values[x] outputs
      {ok, Values} = attrib_utils:get_value(Private, publish_values),
      {ok, Private2} = attrib_utils:set_value(Private1, publish_values, []),

      case length(Values) of
        0 ->  % Nothing to write
          Outputs2 = Outputs;

        ValuesQty ->
          {ok, BlockQty} = config_utils:get_pos_integer(Config, num_of_values),
      
          case BlockQty < ValuesQty of
            true -> % Too many values, truncate Values list
              {TruncValues, _Remainder} = lists:split(BlockQty, Values), 
              % Update the output values
              case attrib_utils:set_values(Outputs, TruncValues) of
                {ok, Outputs1} -> 
                  % Update the status and main output
                  Outputs2 = output_utils:set_value_status(Outputs1, true, normal);

                {error, Reason} ->
                  attrib_utils:log_error(Config, receive_values, invalid, Reason),
                  Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
              end;
              
            false ->
              % Update the output values
              case attrib_utils:set_values(Outputs, Values) of
                {ok, Outputs1} -> 
                  % Update the status and main output
                  Outputs2 = output_utils:set_value_status(Outputs1, true, normal);

                {error, Reason} ->
                  attrib_utils:log_error(Config, receive_values, invalid, Reason),
                  Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
              end
          end
      end,
      % Return updated block state
      {Config, Inputs, Outputs2, Private2};

    % Exec timer timed out before receiving updated values, set output values to null
    timer ->
      % Set array of output values to null
      {ok, NumOfValues} = config_utils:get_integer(Config, num_of_values),
      NullVals = lists:duplicate(NumOfValues, null),
      Outputs1 = output_utils:set_array_values(Outputs, receive_values, NullVals),

      Outputs2 = output_utils:set_value_status(Outputs1, false, timeout),

      % Return updated block state
      {Config, Inputs, Outputs2, Private};

    % Nothing to do, if block is executed for any other reason
    _DontCare ->
      % Return block state unchanged
      {Config, Inputs, Outputs, Private}
  end.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  
  {Config, Inputs, Outputs}.


%% 
%% Handle cast messages 
%%
-spec handle_cast(Msg :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

%% 
%% Handle publish_values message from a send_values block type.
%%
handle_cast({publish_values, Values}, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  logger:debug("Rx publish_values message: ~p Values: ~p~n", 
                      [BlockName, Values]),

  % Save values in private attribute space and execute the block 
  %   to update the block receive_values[x] output attributes
  {ok, NewPrivate} = attrib_utils:set_value(Private, publish_values, Values),
  NewBlockState = block_common:execute({Config, Inputs, Outputs, NewPrivate}, message),

  {noreply, NewBlockState};

%%
%% Unkown cast message
%%
handle_cast(Msg, BlockState) ->

  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  logger:warning(block_type_name_unknown_cast_msg, [BlockModule, BlockName, Msg]),
  {noreply, BlockState}.
    

%% ====================================================================
%% Internal functions
%% ====================================================================




%% ====================================================================
%% Tests
%% ====================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    {[{status, normal}]}
  ].

-endif.
