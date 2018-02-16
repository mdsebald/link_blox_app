%%% @doc 
%%% BLOCKTYPE
%%% Execution Counter
%%% DESCRIPTION 
%%% Increment/decrement count value output every time block is executed.
%%% On initialize or Reset input is true, set output value to initial count value
%%% On block execution, count up/down to final value
%%% If Rollover config parameter is true, on next execution,
%%% set Carry output value to true, and reset output value to initial count 
%%% Carry output value is false for every other case 
%%% LINKS
%%% @end 

-module(lblx_exec_count).

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [math].

version() -> "0.1.0".   


%% Merge the block type specific, Config, Input, Output, and Private attributes
%% with the common Config, Input, Output, and Private attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {rollover, {true}} 
    ]).
                            
                             
-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {reset, {false, {false}}},
      {initial_value, {0, {0}}},
      {final_value, {9, {9}}}
    ]). 

-spec default_outputs() -> output_attribs().
                           
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {carry, {null, []}}
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
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  % No config values to check  
  Outputs1 = output_utils:set_value_status(Outputs, null, initialed),
    
  {Config, Inputs, Outputs1, Private}.


%%
%%  On each block execution increment/decrement count output
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  case input_utils:get_boolean(Inputs, reset) of
    {ok, Reset} ->
      
      case  input_utils:get_integer(Inputs, initial_value) of
        {ok, InitialValue} ->
        
          case input_utils:get_integer(Inputs, final_value) of
            {ok, FinalValue} ->
               
              case config_utils:get_boolean(Config, rollover) of
                {ok, Rollover} ->
                  % Initial and Final values must be integers, can't be empty or null
                  if (not is_integer(InitialValue)) orelse (not is_integer(FinalValue)) ->
                    Value = null, Status = no_input, Carry = null;
                  true ->
                    % Input and Config values are good
                    {ok, CurrentValue} = attrib_utils:get_value(Outputs, value),
                    % if Current output value has not been set 
                    % to a normal integer value yet,set it to the initial value, 
                    % because we have good initial and final input values at this point    
                    if not is_integer(CurrentValue) ->
                      Value = InitialValue, Status = normal, Carry = false;
                    true ->
                      if is_boolean(Reset) andalso Reset ->  
                        % Reset input is true, set output value to initial value,
                        Value = InitialValue, Status = normal, Carry = false;
                      true -> % Reset input is false or missing, 
                        % Check for counter rollover and reset output value to initial value
                        % Need to compare current value with initial and final values each execution 
                        % Because initial and final values are inputs that can change values
                        if (((InitialValue =< FinalValue) andalso (FinalValue =< CurrentValue)) orelse
                            ((CurrentValue =< FinalValue) andalso (FinalValue =< InitialValue))) ->
                          if is_boolean(Rollover) andalso Rollover -> 
                            Value = InitialValue, Status = normal, Carry = true;

                          true -> % Reset input is false and rollover config is false
                                  % Hold count value output at final value
                            Value = FinalValue, Status = normal, Carry = false 
                          end;

                        true -> % Count has not reached final value, 
                          % Determine if count should be incremented or decremented
                          if (InitialValue < FinalValue) -> % Count up
                            Value = CurrentValue + 1, Status = normal, Carry = false;
                          true ->
                            if (FinalValue < InitialValue) -> % Count down
                              Value = CurrentValue - 1, Status = normal, Carry = false;
                            true -> % Inital value and final value are equal
                              % Hold count at current value
                              Value = CurrentValue, Status = normal, Carry = false
                            end
                          end
                        end
                      end
                    end    
                  end; 
                    
                {error, Reason} ->
                  {Value, Status} = config_utils:log_error(Config, rollover, Reason),
                  Carry = null
              end;
            {error, Reason} ->
              {Value, Status} = input_utils:log_error(Config, final_value, Reason),
              Carry = null
          end;
        {error, Reason} -> 
          {Value, Status} = input_utils:log_error(Config, initial_value, Reason),
          Carry = null
      end;
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, reset, Reason),
      Carry = null
  end,
  
  % Update outputs        
  {ok, Outputs1} = attrib_utils:set_values(Outputs,
                  [{value, Value}, {status, Status}, {carry, Carry}]),
    
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Test fixture for rollover config value true (default)
block_rollover_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun rollover_setup/0, 
      fun cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_rollover_io(BlockState)
        ]}
      end} 
  }.

rollover_setup() ->
  unit_test_utils:block_setup(?MODULE).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_rollover_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, rollover_test_sets()).

rollover_test_sets() ->
  [
    % Test bad input values
    {[{reset, "bad"}],                           [{status, input_err}, {value, null}, {carry, null}]},
    {[{reset, true}, {initial_value, "bad"}],    [{status, input_err}, {value, null}, {carry, null}]},
    {[{initial_value, 0}, {final_value, "bad"}], [{status, input_err}, {value, null}, {carry, null}]},
    % Test count up
    {[{initial_value, 0}, {final_value, 5}], [{status, normal}, {value, 0}, {carry, false}]},
    {[{reset, false}], [{status, normal}, {value, 1}, {carry, false}]},
    {[], [{status, normal}, {value, 2}, {carry, false}]},
    {[], [{status, normal}, {value, 3}, {carry, false}]},
    {[], [{status, normal}, {value, 4}, {carry, false}]},
    {[], [{status, normal}, {value, 5}, {carry, false}]},
    {[], [{status, normal}, {value, 0}, {carry, true}]},
    % Test count down
    {[{initial_value, 5}, {final_value, 1}], [{status, normal}, {value, 5}, {carry, true}]},
    {[], [{status, normal}, {value, 4}, {carry, false}]},
    {[], [{status, normal}, {value, 3}, {carry, false}]},
    {[], [{status, normal}, {value, 2}, {carry, false}]},
    {[], [{status, normal}, {value, 1}, {carry, false}]},
    {[], [{status, normal}, {value, 5}, {carry, true}]}

  ].

% Test fixture for rollover config value false
block_no_rollover_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun no_rollover_setup/0, 
      fun cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_no_rollover_io(BlockState)
        ]}
      end} 
  }.

no_rollover_setup() ->
  unit_test_utils:block_setup(?MODULE, [{rollover, false}]).

test_no_rollover_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, no_rollover_test_sets()).

no_rollover_test_sets() ->
  [
    % Test count up
    {[{reset, true}, {initial_value, 0}, {final_value, 5}], [{status, normal}, {value, 0}, {carry, false}]},
    {[{reset, false}], [{status, normal}, {value, 1}, {carry, false}]},
    {[], [{status, normal}, {value, 2}, {carry, false}]},
    {[], [{status, normal}, {value, 3}, {carry, false}]},
    {[], [{status, normal}, {value, 4}, {carry, false}]},
    {[], [{status, normal}, {value, 5}, {carry, false}]},
    {[], [{status, normal}, {value, 5}, {carry, false}]},
    % Test count down
    {[{initial_value, 5}, {final_value, 1}], [{status, normal}, {value, 4}, {carry, false}]},
    {[], [{status, normal}, {value, 3}, {carry, false}]},
    {[], [{status, normal}, {value, 2}, {carry, false}]},
    {[], [{status, normal}, {value, 1}, {carry, false}]},
    {[], [{status, normal}, {value, 1}, {carry, false}]},
    {[], [{status, normal}, {value, 1}, {carry, false}]}

  ].


-endif.