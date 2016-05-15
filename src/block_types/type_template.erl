% INSTRUCTIONS: Copy this module and modify as appropriate 
%               for the function this block will perform.
%               Comments marked "INSTRUCTIONS:" may be deleted 

%%% @doc 
%%% Block Type:  
%%% Description:   
%%%               
%%% @end 

-module(type_template).  % INSTRUCTIONS: Modify to match new module name
                         % INSTRUCTIONS: Add module name to the list of 
                         %  block module names in the block_types module

-author("Your Name").

 % INSTRUCTIONS: Adjust path to hrl file as needed
-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


% INSTRUCTIONS: String naming the block type. 
%   Usually the module name minus "type_"
type_name() -> "template".

% INSTRUCTIONS: Major.Minor.Patch, 
%   Major version change implies a breaking change, 
%   i.e. Block module code is not compatible with a 
%   block definition created with block code with a different major revison 
version() -> "0.1.0".

% INSTRUCTIONS String describing block function
description() -> "Short description of block function".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: atom(),
                      Description :: string()) -> list().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      % INTRUCTIONS: Insert block type specific config attribute tuples here
      % Config attribute tuples consist of a value name and a value
      % Example: {gpio_pin, {0}}
      % Config values are set once on block creation and never modified.                   
    ]). 


-spec default_inputs() -> list().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      % INTRUCTIONS: Insert block type specific input attribute tuples here
      % Input attribute tuples consist of a value name, a value, and a link
      % Example: {hi_limit, {100, ?EMPTY_LINK}}
      % Inputs may be fixed values, or linked to a block output value 
    ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      % INTRUCTIONS: Insert block type specific output attribute tuples here
      % Output attribute tuples consist of a value name, a calculated value, 
      % and a list of blocks that reference (have links to) this output value
      % Output values are always set to 'not_actve' and empty reference list on creation
      % Example: {dwell, {not_active, []}}
    ]). 

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: atom(),
             Description :: string(),  
             InitConfig :: list(), 
             InitInputs :: list()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: atom(),
             Description :: string(), 
             InitConfig :: list(), 
             InitInputs :: list(), 
             InitOutputs :: list()) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs)->

  %% Update Default Config, Input, Output, and Private attribute values 
  %% with the initial values passed into this function.
  %%
  %% If any of the intial attributes do not already exist in the 
  %% default attribute lists, merge_attribute_lists() will create them.
  %% (This is useful for block types where the number of attributes is not fixed)
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.

%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  % Perform block type specific initializations here
  % Add and intialize private attributes here
  Outputs1 = Outputs,
  Private1 = Private,

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  % INSTRUCTIONS: Perform block type specific actions here, 
  % read input value(s) calculate new outut value(s)
  % set block output status value
  Outputs1 = Outputs,
  Private1 = Private,

  % Return updated block state
  {Config, Inputs, Outputs1, Private1}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> ok.

delete({_Config, _Inputs, _Outputs, _Private}) -> 
  % INSTRUCTIONS: Perform any block type specific delete functionality here
  ok.



%% ====================================================================
%% Internal functions
%% ====================================================================

