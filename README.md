# BlockPoint
Test Erlang project

Might be some useful information here:
https://coelhorjc.wordpress.com/2015/02/25/how-to-build-and-package-erlang-otp-applications-using-rebar/

Attempting to integrate with nerves project using linux to build.
Under linux, follow the instructions for nerves-system-br to build with the appropriate defconfig (i.e. nerves_rpi_defconfig) for your platform 

Set environment variables so your project can connect with the nerves project

$ source .\nerves-env.sh.

after cloning project, 

$ chmod +x relhelper.sh   # make it executable

Erlang ALE Makefile calls erlang.mk for make rules

  erlang.mk is not looking in the correct erl_interface directory,  
  nerves-env defines ERL_EI_LIBDIR and ERL_EI_INCLUDE environment variables
  Currently set to:
  ERL_EI_LIBDIR=/home/vagrant/nerves-system-br/buildroot/output/staging/usr/lib/erlang/lib/erl_interface-3.8/lib
  ERL_EI_INCLUDE_DIR=/home/vagrant/nerves-system-br/buildroot/output/staging/usr/lib/erlang/lib/erl_interface-3.8/include
  erlang.mk is not using these.

needed to Add these environment vars to nerves.mk, actually  as export commands in "scripts/nerves-env-helper.sh" 
ERTS_INCLUDE_DIR="$(ERTS_DIR)/include"
ERL_INTERFACE_LIB_DIR="$(ERL_EI_LIBDIR)"
ERL_INTERFACE_INCLUDE_DIR="$(ERL_EI_INCLUDE_DIR)"  

Need to plug in actual export lines here.  make pull request to nerves project?

