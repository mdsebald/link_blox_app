%%% @doc 
%%% Block Type:  Single Digig Seven Segment Display Driver
%%% Description: Unpack byte input to drive a 7 segment plus 
%%%              decimal point single digit LED display
%%%
%%% -------------------------------------------------------
%%% LED Segment ON:  a  |  b |  c | d  |  e |  f |  g | dp  
%%% Segments Value: 0x01|0x02|0x04|0x08|0x10|0x20|0x40|0x80
%%% --------------------------------------------------------
%%%               
%%% @end 

-module(lblx_one_digit_7seg). 

-author("Mark Sebald").

-include("../block_state.hrl").  

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [conversion].

description() -> "Single digit 7 segment LED driver".

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
      {display_on, {true, {true}}},
      {segments, {empty, {empty}}}
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {seg_a, {null, []}},
      {seg_b, {null, []}},
      {seg_c, {null, []}},
      {seg_d, {null, []}},
      {seg_e, {null, []}},
      {seg_f, {null, []}},
      {seg_g, {null, []}},
      {seg_dp, {null, []}}
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
             InitOutputs :: list()) -> block_defn().

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

  % Turn off all segments, set output value to "  ", and status to initialed
  {ok, NewOutputs} = attrib_utils:set_values(Outputs, 
    [
      {value, "  "}, {status, initialed},  
      {seg_a, false}, {seg_b, false}, {seg_c, false}, {seg_d, false},
      {seg_e, false}, {seg_f, false}, {seg_g, false}, {seg_dp, false}
    ]),

  {Config, Inputs, NewOutputs, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  case input_utils:get_boolean(Inputs, display_on) of
    {error, Reason} ->
      Value = null, Status = input_err,
      SegA = null, SegB = null, SegC = null, SegD = null, 
      SegE = null, SegF = null, SegG = null, SegDp = null,
      input_utils:log_error(Config, display_on, Reason);
      
    {ok, DisplayState} ->
      case DisplayState of
        false ->  % Display is off or blank
          Value = 0, Status = normal,
          SegA = false, SegB = false, SegC = false, SegD = false,
          SegE = false, SegF = false, SegG = false, SegDp = false;
            
        true -> % Display is on  
          case input_utils:get_integer(Inputs, segments) of
            {error, Reason} ->
              Value = null, Status = input_err,
              SegA = null, SegB = null, SegC = null, SegD = null, 
              SegE = null, SegF = null, SegG = null, SegDp = null,
               input_utils:log_error(Config, segments, Reason);

            {ok, Segments} ->
              case Segments of 
                null ->
                  Value = null, Status = normal,
                  SegA = null, SegB = null, SegC = null, SegD = null, 
                  SegE = null, SegF = null, SegG = null, SegDp = null;
                  
                Segments ->
                  Value = Segments, Status = normal,

                  % Each bit of the Segments input byte controls one of the segment outputs
                  if (Segments band 16#01) == 16#01 -> SegA = true;
                    true -> SegA = false end,

                  if (Segments band 16#02) == 16#02 -> SegB = true;
                      true -> SegB = false end,

                  if (Segments band 16#04) == 16#04 -> SegC = true;
                    true -> SegC = false end,

                  if (Segments band 16#08) == 16#08 -> SegD = true;
                    true -> SegD = false end,

                  if (Segments band 16#10) == 16#10 -> SegE = true;
                    true -> SegE = false end,

                  if (Segments band 16#20) == 16#20 -> SegF = true;
                    true -> SegF = false end,
    
                  if (Segments band 16#40) == 16#40 -> SegG = true;
                    true -> SegG = false end,

                  if (Segments band 16#80) == 16#80 -> SegDp = true;
                    true -> SegDp = false end
              end
          end
      end
  end,         

  % update the outputs
  {ok, Outputs1} = attrib_utils:set_values(Outputs, 
    [
      {value, Value}, {status, Status},  
      {seg_a, SegA}, {seg_b, SegB}, {seg_c, SegC}, {seg_d, SegD},
      {seg_e, SegE}, {seg_f, SegF}, {seg_g, SegG}, {seg_dp, SegDp}
    ]),
 
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

% Perform minimum block unit test

block_test() ->
  unit_test_utils:min_block_test(?MODULE).

-endif.