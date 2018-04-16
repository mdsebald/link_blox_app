# LinkBlox #

#### An application to implement a block programming language targeting embedded and IoT devices ####

[![Build Status](https://travis-ci.org/mdsebald/LinkBlox.png?branch=master)](https://travis-ci.org/mdsebald/LinkBlox)

LinkBlox is an Erlang application that can either be run on Linux<sup>[1](#fn01)</sup>, or built using the Nerves project<sup>[2](#fn02)</sup> to create standalone embedded systems.  See [Nerves](http://nerves-project.org/ "Nerves Project").

The purpose of LinkBlox is to abstract away programming complexity into discrete blocks of functionality that can be created and linked to other blocks to create a device with desired functionality.

LinkBlox leverages Erlang's built-in support for Concurrency, Distribution, and Messaging.  Each block runs in its own process, and blocks are linked (i.e. values are sent between blocks) via Erlang's messaging. Distributed blocks can be linked across nodes on a network, just as easily as linking to other blocks on the same node.

#### Block Definition ####

Blocks are defined by code and data.

The code for each type of block is contained in an Erlang module.  The name of the module, corresponds to the block type.  All block code modules, regardless of type, export the same four functions: create(), initialize(), execute(), and delete().

- create(): Create a new set of block data, from the values passed into the create() function or with a set of default values defined in the block's code module.
- initialize(): Perform actions required before block execution can begin, (i.e. check configuration, gather resources, etc)
- execute(): Read the block's input values, and update the block's output values. By default a block's execute funtion is called on an input value change (Data Flow).  Blocks may also be executed on a timer, or executed on command from another block (Control Flow).
- delete(): Release resources used, and unlinks from any connected blocks.

Block data is defined as a tuple of 3 lists:<sup>[3](#fn03)</sup>
 - Config: A list of key ID value pair tuples. Config values are normally set at block creation.  Modifying block config values causes the block to be re-initialized.  Equivalent to deleting and recreating the block.
 - Inputs: A list of key ID, value, and link tuples. Inputs may be set to fixed values or "linked" to an output value of another block, or a block on another node.  A link is specified by a block name and output value ID. An optional node name, may be prefixed, to link to a block value on another LinkBlox node.
 - Outputs: A list of key ID, value, and reference tuples. Output values contain the results of the block execution.  The reference element of the output tuple contains the names of blocks (prefixed by node name if needed) that have input values linked to this block output.
 
 LinkBlox also allows arrays of Config, Inputs, and Outputs to be defined.  Arrays of attributes are specified by a key ID and an index, from 1 to the size of the array.
 
#### Block Types ####

Current list of block types may be found here: http://www.linkblox.org/BlockTypes.html
 
#### User Interface ####

On starting the application, LinkBlox spins up an Erlang SSH CLI Server on port 1111.  SSH into this port from another Linux prompt to use LinkBlox's command line interface. From the CLI, blocks may be created, edited, linked, executed, deleted, monitored, etc. From the commmand line you can connect to other LinkBlox nodes, and manipulate them the same way. Connecting requires your /etc/hosts file to contain the host name and IP address of the node running LinkBlox, that you wish to connect to.  

NOTE: I use the Erlang short name for connecting.  Example node name:  LinkBlox@<hostname>,  no domain specified.  This requires your /etc/hosts file to include the short name also.  Example: for the node name LinkBlox@raspi-3,  you would need a line in your /etc/hosts file like this:  192.168.1.140  raspi-3,  
NOTE: To use SSH requires setting up a Public / Private key pair, assumed to be in ~/.ssh 
Example command line:  "ssh  -p 1111  \<host name\>

#### Building and Running ####

Clone this repo, on a Linux box, type make, and run the the resulting LinkBlox binary.  (Binary may be found in ~/LinkBlox/_rel/LinkBlox/bin/LinkBlox)
Requires Erlang and relx to be installed to build

<a name="fn01">1</a>: I'm assuming the code can be compiled and run on Windows or a Mac, but I haven't tried it myself.

<a name="fn02">2</a>: See my repo: [nerves_link_blox] https://github.com/mdsebald/nerves_link_blox

<a name="fn03">3</a>: In practice, the block data tuple also contains a list of private block data.
