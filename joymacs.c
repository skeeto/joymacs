#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <linux/joystick.h>
#include "emacs-module.h"

/* Frequently-used symbols. */
static emacs_value nil;
static emacs_value t;
static emacs_value button;
static emacs_value axis;

static void
fin_close(void *fdptr)
{
    int fd = (intptr_t)fdptr;
    if (fd != -1)
        close(fd);
}

#define JOYMACS_OPEN                                    \
    "(joymacs-open N)\n"                                \
    "\n"                                                \
    "Create a handle for the Nth joystick."

static emacs_value
joymacs_open(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)ptr;
    (void)n;
    int id = env->extract_integer(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return nil;
    char buf[64];
    int buflen = sprintf(buf, "/dev/input/js%d", id);
    int fd = open(buf, O_RDONLY | O_NONBLOCK);
    if (fd == -1) {
        emacs_value signal = env->intern(env, "file-error");
        emacs_value message = env->make_string(env, buf, buflen);
        env->non_local_exit_signal(env, signal, message);
        return nil;
    }
    return env->make_user_ptr(env, fin_close, (void *)(intptr_t)fd);
}

#define JOYMACS_CLOSE                                                    \
    "(joymacs-close JOYSTICK)\n"                                         \
    "\n"                                                                 \
    "Immediately destroy JOYSTICK handle.\n"                             \
    "\n"                                                                 \
    "Handles close automatically through garbage collection, but this\n" \
    "releases the resources immediately."

static emacs_value
joymacs_close(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)ptr;
    (void)n;
    int fd = (intptr_t)env->get_user_ptr(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return nil;
    if (fd != -1) {
        close(fd);
        env->set_user_ptr(env, args[0], (void *)(intptr_t)-1);
    }
    return nil;
}

#define JOYMACS_READ                                                    \
    "(joymacs-read JOYSTICK EVENT)\n"                                   \
    "\n"                                                                \
    "Fill 5-element vector EVENT with a single joystick event.\n"       \
    "\n"                                                                \
    "Elements of EVENT are [time type value number init-p],\n"          \
    "where \"type\" is :button or :axis. Returns EVENT on success,\n"   \
    "or if no events are available."

static emacs_value
joymacs_read(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)n;
    (void)ptr;
    int fd = (intptr_t)env->get_user_ptr(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return nil;
    struct js_event e;
    int r = read(fd, &e, sizeof(e));
    if (r == -1 && errno == EAGAIN) {
        /* No more events. */
        return nil;
    } if (r == -1) {
        /* An actual read error (joystick unplugged, etc.). */
        emacs_value signal = env->intern(env, "file-error");
        const char *error = strerror(errno);
        emacs_value message = env->make_string(env, error, strlen(error));
        env->non_local_exit_signal(env, signal, message);
        return nil;
    } else {
        /* Fill out event vector. */
        emacs_value v = args[1];
        emacs_value type = e.type & JS_EVENT_BUTTON ? button : axis;
        emacs_value value;
        if (type == button)
            value = e.value ? t : nil;
        else
            value =  env->make_float(env, e.value / (double)INT16_MAX);
        env->vec_set(env, v, 0, env->make_integer(env, e.time));
        env->vec_set(env, v, 1, type);
        env->vec_set(env, v, 2, value);
        env->vec_set(env, v, 3, env->make_integer(env, e.number));
        env->vec_set(env, v, 4, e.type & JS_EVENT_INIT ? t : nil);
        return args[1];
    }
}

int
emacs_module_init(struct emacs_runtime *ert)
{
    emacs_env *env = ert->get_environment(ert);

    /* Gather symbols. */
    nil = env->intern(env, "nil");
    t = env->intern(env, "t");
    button = env->make_global_ref(env, env->intern(env, ":button"));
    axis = env->make_global_ref(env, env->intern(env, ":axis"));

    /* Bind functions. */
    emacs_value fset = env->intern(env, "fset");
    emacs_value args[2];
    args[0] = env->intern(env, "joymacs-open");
    args[1] = env->make_function(env, 1, 1, joymacs_open, JOYMACS_OPEN, 0);
    env->funcall(env, fset, 2, args);
    args[0] = env->intern(env, "joymacs-close");
    args[1] = env->make_function(env, 1, 1, joymacs_close, JOYMACS_CLOSE, 0);
    env->funcall(env, fset, 2, args);
    args[0] = env->intern(env, "joymacs-read");
    args[1] = env->make_function(env, 2, 2, joymacs_read, JOYMACS_READ, 0);
    env->funcall(env, fset, 2, args);

    /* (provide 'joymacs) */
    emacs_value provide = env->intern(env, "provide");
    emacs_value joymacs = env->intern(env, "joymacs");
    env->funcall(env, provide, 1, &joymacs);
    return 0;
}

int plugin_is_GPL_compatible;
