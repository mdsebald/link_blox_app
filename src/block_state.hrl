%%% @doc 
%%% Block Data Type Definitions   
%%% @end 

-author("Mark Sebald").


% Block definition.  Used for creating or saving a block 

-type block_defn() :: {
                        Config :: config_attribs(),
                        Inputs :: input_attribs(),
                        Outputs :: output_attribs()
                      }.


% Block state. Used for retaining state between block execution

-type block_state() :: { 
                         Config :: config_attribs(),
                         Inputs :: input_attribs(),
                         Outputs :: output_attribs(),
                         Private :: private_attribs()
                       }.

% Types used for storing block values

-type attribs() :: config_attribs() | input_attribs() | output_attribs() | private_attribs().
-type config_attribs()  :: [config_attrib()].
-type input_attribs()   :: [input_attrib()].
-type output_attribs()  :: [output_attrib()].
-type private_attribs() :: [private_attrib()].

-type attrib() :: config_attrib() | input_attrib() | output_attrib() | private_attrib().
-type config_attrib()  :: {value_name(), config_value()  | config_value_array()}.
-type input_attrib()   :: {value_name(), input_value()   | input_value_array()}.
-type output_attrib()  :: {value_name(), output_value()  | output_value_array()}.                 
-type private_attrib() :: {value_name(), private_value() | private_value_array()}.

-type attrib_value_array() :: [attrib_value()].
-type attrib_value() :: config_value() | input_value() | output_value() | private_value().

-type config_value_array() :: [config_value()].
-type config_value() :: {value()}.

-type input_value_array() :: [input_value()].
-type input_value() ::  {value(), {value()}}.  % Second value() is the default value

-type output_value_array() :: [output_value()].
-type output_value() :: {value(), link_defs()}.

-type private_value_array() :: [private_value()].
-type private_value() :: {any()}.

-type link_defs() :: [link_def()].
-type link_def() :: {block_name(), value_id()}.
-type empty_link() :: {}.
-define (EMPTY_LINK, {}).

-type block_values() :: [block_value()].
-type block_value() :: {value_id(), value()}.

-type value_id() :: value_name() |
                    {value_name(), ArrayIndex :: pos_integer()}.
                        
-type value_name() :: atom().
-type block_name() :: atom().
-type type_name() :: atom().

-type attrib_result_value() :: {ok, value()} | attrib_errors().

-type attrib_errors() :: {error, not_found | invalid_value | invalid_index }.                          

-type value() :: atom() | integer() | float() | boolean() | 
                 string() | tuple() | list() | reference() | pid() | node().

% Used to read block input values

-type input_errors() :: {error, not_found | bad_link | range | bad_type | not_input}.

-type generic_input_value() :: {ok, value()}   | {ok, null} | input_errors().
-type integer_input_value() :: {ok, integer()} | {ok, null} | input_errors().
-type float_input_value()   :: {ok, float()}   | {ok, null} | input_errors().
-type boolean_input_value() :: {ok, boolean()} | {ok, null} | input_errors().
-type string_input_value()  :: {ok, string()}  | {ok, null} | input_errors().


% Used to read block configuration values
       
-type config_errors() :: {error, not_found | range | bad_type | not_config}.
                                           
-type generic_config_value() :: {ok, value()}   | config_errors().
-type integer_config_value() :: {ok, integer()} | config_errors().
-type float_config_value()   :: {ok, float()}   | config_errors().
-type boolean_config_value() :: {ok, boolean()} | config_errors().
-type string_config_value()  :: {ok, string()}  | config_errors().

                      
%%
%% Execute method defines the possible reasons for a block to be executed
%% "Execute" means to read block input values and update block output values
%%
%%  manual:     Manually invoked via UI or external application 
%%  input_cos:  One or more input values have changed (i.e. Data Flow)
%%  timer:      Execution interval timer has timed out
%%  exec_out:   Exec Input is linked to the Exec Output of 
%%              a block that has been executed.  (i.e. Control Flow)
%%  hardware:   Block is connected to HW that can trigger execution
%%              i.e. GPIO interrupt
%%  message:    Block has received a message from a subsystem, 

-type exec_method() :: manual | input_cos | timer | exec_out | hardware | message.
   
   
%%
%% Block status defines the possible values for the block status output value
%%
%%  created:    Block attributes have been instantiated
%%  initialed:  Block has been initialized, pre execution prep has been completed
%%  normal:     Block has executed normally
%%  disabled:   Block disable input is true/on. All block outputs are set to null 
%%  timeout:    Block values did not get updated from external source, before exec timer expired.
%%  error:      Block has encountered some error when attempting to execute
%%  input_err:  One or more of the block input values is incompatible with the block's code
%%  config_err: One or more of the block configuration values is incompatible with the block's code
%%  proc_err:   There is an error outside of the block code that is preventing the block from executing
%%  no_input:   One or more input values are missing, so the block cannot calculate an output
%%  override:   One or more block output values have been set manually, instead of being calculated 
%%  empty:      Block is about to be deleted 
   
-type block_status() :: created | initialed | normal |  disabled | timeout |
                        error | input_err | config_err | proc_err | no_input | override | empty.


%%
%% Define block type groups, to assist in classifying and organizing block types
%% A block type can be assigned to more than one group
%%

-type type_group() :: none | math | logic | string | conversion | control | input | output | 
                      digital | analog | sensor | actuator | composite | i2c | spi | gpio. 

