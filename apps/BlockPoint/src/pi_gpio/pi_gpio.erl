%% -----------------------------------------------------------------------------
%% Read and Write Raspberry Pi GPIO pins using sysfs virtual file system
%% See: https://www.kernel.org/doc/Documentation/gpio/sysfs.txt for documentation
%% ------------------------------------------------------------------------------

-module(pi_gpio).

% Currently, pins are "export"ed via a shell script on restart, 
% This is to avoid a problem with accessing the gpio pin files when they are owned by root
% The startup script, exports the desired pins, and changes the owner of the gpio pin files to "pi"
% so they may be read and written to without raising the priveledge of the running program

%-export([reserve_pin/1, release_pin/1]).
-export([set_pin_direction/2, read_pin_direction/1]). 
-export([set_pin_value/2, read_pin_value/1]).
-export([set_pin_edge/2, read_pin_edge/1]).
-export([set_pin_active_low/2, read_pin_active_low/1]).
-export([get_value_file/1]).


-define(GPIO_PATH, "/sys/class/gpio/").
-define(GPIO, "gpio").


%% For all functions, the Pin parameter is a string representation of an integer value

%% Reserve Pin for use by this process
% This is done in a shell script on startup now.

%reserve_pin(Pin) ->
   %ExportFile = string:concat(?GPIO_PATH, "export"),
   %file:write_file(ExportFile, Pin).


%% Release Pin for use by other processes

%release_pin(Pin) -> 
   %UnexportFile = string:concat(?GPIO_PATH, "unexport"),
   %file:write_file(UnexportFile, Pin).


%% Valid values: "in", "out", "low, or "high" 
%% Writing "out" defaults output value to low.
%% Writing "low" or "high" sets the Pin to an output, and the initial value

set_pin_direction(Pin, Direction) ->
	FileName = direction_file(Pin),
	case filelib:is_file(FileName) of
		true  -> file:write_file(FileName, Direction);
		false -> io:format("~p file not found~n", [FileName])
	end.

read_pin_direction(Pin) ->
	FileName = direction_file(Pin),
	case filelib:is_file(FileName) of
		true  -> file:read_file(direction_file(Pin));
		false -> io:format("~p file not found~n", [FileName])
	end.


%% Valid values: "0" or "1"

set_pin_value(Pin, Value) ->
	FileName = value_file(Pin),
	case filelib:is_file(FileName) of
		true  -> file:write_file(FileName, Value);
		false -> io:format("~p file not found~n", [FileName])
	end.

read_pin_value(Pin) ->
	FileName = value_file(Pin),
	case filelib:is_file(FileName) of
		true ->
			case file:read_file(FileName) of
				{ok, Value} ->
					Value;
				{error, Reason} ->
					io:format("~p error reading file: ~p~n", [Reason, FileName]),
					{error, Reason}
			end;
		false ->
			io:format("~p file not found~n", [FileName])
	end.

%% Valid values: "none", "rising", "falling", or "both"

set_pin_edge(Pin, Edge) ->
	FileName = edge_file(Pin),
	case filelib:is_file(FileName) of
		true -> file:write_file(FileName, Edge);
		false -> io:format("~p file not found~n", [FileName])
	end.

read_pin_edge(Pin) ->
	FileName = edge_file(Pin),
	case filelib:is_file(FileName) of
		true -> file:read_file(FileName);
		false -> io:format("~p file not found~n", [FileName])
	end.


%% Valid values: "0" or  "1"

set_pin_active_low(Pin, ActiveLow) ->
	FileName = active_low_file(Pin),
	case filelib:is_file(FileName) of 
		true -> file:write_file(FileName, ActiveLow);
		false -> io:format("~p file not found~n", [FileName])
	end.

read_pin_active_low(Pin) ->
	FileName = active_low_file(Pin),
	case filelib:is_file(FileName) of 
		true -> file:read_file(FileName);
		false -> io:format("~p file not found~n", [FileName])
	end.
		

get_value_file(Pin)->
   value_file(Pin).

%% ----------------------------------------------------
%%  Helper functions
%% ----------------------------------------------------
direction_file(Pin) ->
   string:concat(pin_path(Pin), "direction").

value_file(Pin) ->
   string:concat(pin_path(Pin), "value").

edge_file(Pin) ->
   string:concat(pin_path(Pin), "edge").

active_low_file(Pin) ->
   string:concat(pin_path(Pin), "active_low").

pin_path(Pin) ->
   PinFolder = string:concat(string:concat(?GPIO, Pin), "/"),
   string:concat(?GPIO_PATH, PinFolder).
