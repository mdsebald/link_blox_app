%%% @doc 
%%% BLOCKTYPE
%%% Limit input value
%%% DESCRIPTION
%%% Set the block output value to never exceed the high and low limit input values
%%% LINKS              
%%% @end 

-module(lblx_math_limit).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [math].

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {high_limit, {empty, {empty}}}, %| number | empty | +/- max number |
      {input, {empty, {empty}}}, %| number | empty | +/- max number |
      {low_limit, {empty, {empty}}} %| number | empty | +/- max number |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
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
   % No config values to check
  
  Outputs1 = output_utils:set_value_status(Outputs, null, initialed),  

  % This is the block state
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  case input_utils:get_number(Inputs, input) of
    {ok, null} ->
      Value = null, Status = no_input;

    {ok, InputVal} ->
      case input_utils:get_number(Inputs, high_limit) of
        {ok, null} ->
          Value = null, Status = no_input;

        {ok, HighLimit} ->
          case input_utils:get_number(Inputs, low_limit) of
            {ok, null} ->
              Value = null, Status = no_input;

            {ok, LowLimit} ->
              % Got good input, high limit, and low limit values
              case (LowLimit < HighLimit) of
                true -> 
                  case (InputVal < LowLimit) of
                    true ->
                      Value = LowLimit, Status = normal;
                    
                    false ->
                      case (HighLimit < InputVal) of
                        true ->
                          Value = HighLimit, Status = normal;
                        
                        false ->
                          Value = InputVal, Status = normal
                      end
                  end;
                false ->
                  % Error, low limit value >= high limit
                  Value = null, Status = input_err
              end;

            {error, Reason} ->
              {Value, Status} = input_utils:log_error(Config, low_limit, Reason)
          end;

        {error, Reason} ->
          {Value, Status} = input_utils:log_error(Config, high_limit, Reason)
      end;

    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason)
  end,
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % Return updated block state
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

block_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun setup/0, 
      fun cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_io(BlockState)
        ]}
      end} 
  }.

setup() ->
  unit_test_utils:block_setup(?MODULE).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

test_sets() ->
  [
    {[], [{status, no_input}, {value, null}]},
    {[{high_limit, null}, {low_limit, null}, {input, 23.3}], [{status, no_input}, {value, null}]},
    {[{high_limit, bad_value}, {low_limit, -50}, {input, 23.3}], [{status, input_err}, {value, null}]},
    {[{high_limit, 98.6}, {low_limit, -40}, {input, 23.3}], [{status, normal}, {value, 23.3}]},
    {[{high_limit, 98.6}, {low_limit, -40}, {input, -56.7}], [{status, normal}, {value, -40}]},
    {[{high_limit, 98.6}, {low_limit, -40}, {input, 100}], [{status, normal}, {value, 98.6}]},
    {[{high_limit, -40}, {low_limit, -40}, {input, 100}], [{status, input_err}, {value, null}]}
  ].

-endif.