# LinkBloxApp #

#### An application to implement a block programming language targeting embedded and IoT devices ####

[![Build Status](https://travis-ci.org/mdsebald/link_blox_app.png?branch=master)](https://travis-ci.org/mdsebald/link_blox_app)

LinkBloxApp is an Erlang application. 

The purpose of LinkBlox is to abstract away programming complexity into discrete blocks of functionality that can be created and linked to other blocks to create a device with desired functionality.

LinkBlox leverages Erlang's built-in support for Concurrency, Distribution, and Messaging.  Each block runs in its own process, and blocks are linked (i.e. values are sent between blocks) via Erlang's messaging. Passing block values between distributed nodes is via special purpose send and receive block types

#### Block Definition ####

Blocks are defined by code and data.

The code for each type of block is contained in an Erlang module.  The name of the module, corresponds to the block type.  All block code modules, regardless of type, export the same four functions: create(), initialize(), execute(), and delete().

- create(): Create a new set of block data, from the values passed into the create() function or with a set of default values defined in the block's code module.
- initialize(): Perform actions required before block execution can begin, (i.e. check configuration, gather resources, etc)
- execute(): Read the block's input values, and update the block's output values. By default a block's execute funtion is called on an input value change (Data Flow).  Blocks may also be executed on a timer, or executed on command from another block (Control Flow).
- delete(): Release resources used, and unlinks from any connected blocks.

Block data is defined as a tuple of 3 lists:
 - Config: A list of key value pair tuples. Config values are normally set at block creation.  Modifying block config values causes the block to be re-initialized.  Equivalent to deleting and recreating the block.
 - Inputs: A list of key, current value, and default value tuples. Inputs may be set to a fixed value or obtain a value via a link to the output value of a block.  
 - Outputs: A list of key, value, and list of links tuples. Output values contain the results of the block execution.  The list of links indicate which block and input value this current block ouput value should be sent to.
 
 LinkBlox also allows arrays of Config, Inputs, and Outputs to be created.  Arrays of attributes are specified by a key and an index, from 1 to the size of the array.
 
#### Block Types ####

Current list of block types may be found here: http://www.linkblox.org/BlockTypes.html

#### Building and Running ####

LinkBloxApp is used for development and testing.  LinkBloxApp is a dependancy of LinkBlox.  

Use LinkBlox to build the actual application releases to either run on Linux, or in an embedded environment like [Nerves](http://nerves-project.org/ "Nerves Project").

To build LinkBloxApp, Elixir 1.6 or greater and Erlang OTP 20 or later must be installed

'$ git clone 
$ cd link_blox_app
$ mix deps.get
$ mix compile
$ mix run --no-halt'

To run unit tests

'$ mix eunit'

To run Dialyzer 

'$ mix dialyzer'

#### User Interface ####

On starting the application, LinkBloxApp spins up an Erlang SSH CLI Server on port 1111.  SSH into this port from another Linux prompt to use LinkBlox's command line interface. From the CLI, blocks may be created, edited, linked, executed, deleted, monitored, etc. From the commmand line you can connect to other LinkBlox nodes, and manipulate them the same way. Connecting requires your /etc/hosts file to contain the host name and IP address of the node running LinkBlox, that you wish to connect to.  

NOTE: I use the Erlang short name for connecting.  Example node name:  LinkBlox@<hostname>,  no domain specified.  This requires your /etc/hosts file to include the short name also.  Example: for the node name LinkBlox@raspi-3,  you would need a line in your /etc/hosts file like this:  192.168.1.140  raspi-3,  
NOTE: To use SSH requires setting up a Public / Private key pair, assumed to be in ~/.ssh 
Example command line:  "ssh  -p 1111  \<hostname\>
