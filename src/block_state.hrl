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

% Types used for storing block values

-type attribute() :: config_attr() | input_attr() | output_attr() | private_attr().

-type config_attr() :: {value_name(), config_value() | config_value_array()}.
                        
 -type input_attr() :: {value_name(), input_value() | input_value_array()}.

 -type output_attr() :: {value_name(), output_value() | output_value_array()}.                 

-type private_attr() :: {value_name(), private_value() | private_value_array()}.


-type value_name() :: atom().
-type block_name() :: atom().

-type attr_value() :: config_value() | input_value() | output_value().
-type attr_value_array() ::  config_value_array() | input_value_array() | output_value_array().

-type config_value_array() :: list(config_value()).
-type config_value() :: {value()}.

-type input_value_array() :: list(input_value()).
-type input_value() ::  {value(), input_link()}.

-type output_value_array() :: list(output_value()).
-type output_value() :: {value(), link_refs()}.

-type private_value_array() :: list(private_value()).
-type private_value() :: {any()}.


-type input_link() :: empty_link() | self_link() | local_link() | global_link(). 
                      
-type empty_link() :: {}.
-define (EMPTY_LINK, {}).

-ifdef(INCLUDE_OBSOLETE). 
% TODO: Delete after we finalize link structure 
-type self_link() :: {ValueName :: value_name(), 
                      ArrayInex :: integer()}.
                       
-type local_link() :: {BlockName :: block_name(),
                       ValueName :: value_name(), 
                       ArrayInex :: integer()}.
                        
-type global_link() :: {NodeName :: node(),
                        BlockName :: block_name(),
                        ValueName :: value_name(), 
                        ArrayInex :: integer()}.
-endif.

-type self_link() :: {value_id()}.
                       
-type local_link() :: {block_name(), value_id()}.
                        
-type global_link() :: {node(), block_name(), value_id()}.
                        
-type value_id() :: value_name() |
                    {value_name(), ArrayIndex :: pos_integer()}.
                        

-type link_refs() :: list(pid()).

-type value() :: empty | not_active | integer() | float() | boolean() | 
                 string() | tuple() | list().

-type attrib_errors() :: {error, not_found | invalid_value | invalid_index }.
                          
-type attrib_value() :: {ok, value()} | attrib_errors().


% Used to read block input values

-type input_errors() :: {error, not_found | bad_link | range | bad_type | not_input}.
                          
-type generic_input_value() :: {ok, value()} | {ok, not_active} | input_errors().

-type integer_input_value() :: {ok, integer()} | {ok, not_active} | input_errors().

-type float_input_value() :: {ok, float()} | {ok, not_active} | input_errors().

-type boolean_input_value() :: {ok, boolean()} | {ok, not_active} | input_errors().


% Used to read block configuration values
       
-type config_errors() :: {error, not_found | range | bad_type | not_config}.
                                           
-type generic_config_value() :: {ok, value()} | config_errors().

-type integer_config_value() :: {ok, integer()} |  config_errors().

-type float_config_value() :: {ok, float()} | config_errors().

-type boolean_config_value() :: {ok, boolean()} | config_errors().
                      

                      
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
%%  normal:     Block has executed normally
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
