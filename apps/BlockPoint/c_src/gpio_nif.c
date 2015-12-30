
/*
 * Erlang NIF to monitor for interrupts from a given GPIO Input Pin.
 * This routine assumes pin is already exported, pin is an input, 
 * and desired pin interrupt behavior is already set
 */
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <poll.h>
#include "erl_nif.h"

void read_and_send_value(ErlNifEnv* env, int fd);
void* wait_for_interrupt(void* obj);

#define MAX_PIN_VALUE_FILE_LEN 64    // i.e. "/sys/class/gpio/gpioXX/value"

ErlNifTid Tid;
ErlNifPid Dst_pid;
char Pin_value_file[MAX_PIN_VALUE_FILE_LEN];

void read_and_send_value(ErlNifEnv* env, int fd)
{
	ERL_NIF_TERM msg;
	char buf[4];
	
	// read the current value, and make sure it is a one character null terminated string
	if (pread(fd, buf, 1, 0) == 1)
	{
		buf[1] = 0;  
		msg = enif_make_tuple2(env, enif_make_atom(env, "input"), enif_make_string(env, buf, ERL_NIF_LATIN1));
	}
	else // something went wrong, attempt to send the error back to the Erlang VM
	{
		msg = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, errno));
	}
			
	enif_send(NULL, &Dst_pid, env, msg);
	// need to clear the nif env after every enif_send call
	enif_clear_env(env);
}

void* wait_for_interrupt(void* obj)
{
    ErlNifEnv* env = enif_alloc_env();
	
    struct pollfd fdset[1];

    fdset[0].events = POLLPRI;
    fdset[0].fd = open(Pin_value_file, O_RDONLY);

    if (fdset[0].fd < 0)
        err(EXIT_FAILURE, "open");
		
	// read the current value from the value file, to initialize the input value on the Erlang side
	// read_and_send_value(env, fdset[0].fd);

    while(1)
    {	
        fdset[0].revents = 0;
        if(poll(fdset, 1, -1) < 0)
        {
            // Retry if EINTR
            if (errno == EINTR)
                continue;
                     
            err(EXIT_FAILURE, "poll");
        }
                                       
        if (fdset[0].revents & POLLPRI)
		{
			read_and_send_value(env, fdset[0].fd);
		}
    }
}


static ERL_NIF_TERM init_interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_get_local_pid(env, argv[0], &Dst_pid);
    enif_get_string(env, argv[1], Pin_value_file, sizeof(Pin_value_file), ERL_NIF_LATIN1);
    enif_thread_create("wait_for_interrupt", &Tid, wait_for_interrupt, NULL, NULL);
	
    return enif_make_atom(env, "ok");
}



static ErlNifFunc nif_funcs[] =
{
     {"init_interrupt", 2, init_interrupt}
};

ERL_NIF_INIT(gpio_nif, nif_funcs, NULL, NULL, NULL, NULL);


