# BlockPoint
Test Erlang project

Might be some useful information here:
https://coelhorjc.wordpress.com/2015/02/25/how-to-build-and-package-erlang-otp-applications-using-rebar/

Attempting to integrate with nerves project using linux to build.
Under linux, follow the instructions for nerves-system-br to build with the appropriate defconfig (i.e. nerves_rpi_defconfig) for your platform 

$ source .\nerves-env.sh.

after cloning project, 
$ chmod +x relhelper.sh   # make it executable

Makefile calls erlang.mk for make rules
  Does not appear to use the correct compiler flags to compile erlang_ale/c_src, because linker errors-out, File format not recognized.
  

