%%% @doc 
%%% Block Data Type Definitions   
%%% @end 

-author("Mark Sebald").


% Block definition.  Used for creating or saving a block 

-type block_defn() :: {
                        Config :: list(config_attr()),
                        Inputs :: list(input_attr()),
                        Outputs :: list(output_attr())
                      }.


% Block state. Used for retaining state between block execution

-type block_state() :: { 
                         Config :: list(config_attr()),
                         Inputs :: list(input_attr()),
                         Outputs :: list(output_attr()),
                         Private :: list(private_attr())
                       }.


-type attribute() :: config_attr() | input_attr() | output_attr() | private_attr().


-type config_attr() :: { 
                          ValueName :: atom(),
                          ConfigValue :: {value()} | [{value()},...]
                       }.


 -type input_attr() :: { 
                          ValueName :: atom(),
                          InputValue :: {value(), input_link()} | [{value(), input_link()},...]
                       }.


 -type output_attr() :: { 
                          ValueName :: atom(),
                          OutputValue :: {value(), link_refs()} | [{value(), link_refs()},...]
                       }.                      


-type private_attr() :: {
                          ValueName :: atom(),
                          PrivateValue :: {any()} | [{any()},...]
                        }.


-type input_link() :: { 
                        NodeName :: atom() | null,
                        BlockName :: atom() | null,
                        ValueName :: atom() | null,
                        Index :: integer()
                      }.
 
 
-type link_refs() :: [pid(), ...].


-type value() :: empty | not_active | integer() | float() | boolean() | 
                 string() | tuple() | list().

-type attrib_errors() :: {error, not_found | invalid_value | invalid_index | negative_index}.
                          
-type attrib_value() :: {ok, value()} | attrib_errors().


%%
%% Define block input value types
%%

-type input_errors() :: {error, not_found | bad_link | range | bad_type | not_input}.
                          
-type generic_input_value() :: {ok, term()} | {ok, not_active} | input_errors().

-type integer_input_value() :: {ok, integer()} | {ok, not_active} | input_errors().

-type float_input_value() :: {ok, float()} | {ok, not_active} | input_errors().

-type boolean_input_value() :: {ok, boolean()} | {ok, not_active} | input_errors().


%%
%% Define block configuration value types
%%
       
-type config_errors() :: {error, not_found | range | bad_type | not_config}.
                                           
-type generic_config_value() :: {ok, term()} | config_errors().

-type integer_config_value() :: {ok, integer()} |  config_errors().

-type float_config_value() :: {ok, float()} | config_errors().

-type boolean_config_value() :: {ok, boolean()} | config_errors().
                      
%%
%% specifies an empty input value link
%% useful for initializing block inputs and testing for non-empty links
%%
-define(EMPTY_LINK, {}).

    
                      
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

-type exec_method() :: manual | input_cos | timer | exec_out | hardware.
   
   
%%
%% Block status defines the possible values for the block status output value
%%
%%  created:    Block attributes have been instantiated
%%  initialed:  Block has been initialized, pre execution prep has been completed
%%  normal:     Block is has executed normally
%%  disabled:   Block disable input is true/on. All block outputs are set to not_active 
%%  frozen:     Block freeze input is true/on.  All block outputs are frozen at last calculated value
%%  error:      Block has encountered some error when attempting to executed
%%  input_err:  One or more of the block input values is incompatible with the block's code
%%  config_err: One or more of the block configuration values is incompatible with the block's code
%%  proc_err:   There is an error outside of the block code that is preventing the block from executing
%%  no_input:   One or more input values are missing, so the block cannot calculate an output
%%  override:   One or more block output values have been set manually, instead of being calculated 
%%   
   
-type block_status() :: created | initialed | normal |  disabled | frozen | 
                        error | input_err | config_err | proc_err | no_input | override.
