#include "emacs-module.h"

#define S(s) (env->intern(env, s))

#define JOYMACS_OPEN                                                     \
    "(joymacs-open N)\n"                                                 \
    "\n"                                                                 \
    "Create a handle for the Nth joystick."

#define JOYMACS_CLOSE                                                    \
    "(joymacs-close JOYSTICK)\n"                                         \
    "\n"                                                                 \
    "Immediately destroy JOYSTICK handle.\n"                             \
    "\n"                                                                 \
    "Handles close automatically through garbage collection, but this\n" \
    "releases the resources immediately."

#define JOYMACS_READ                                                     \
    "(joymacs-read JOYSTICK EVENT)\n"                                    \
    "\n"                                                                 \
    "Fill 5-element vector EVENT with a single joystick event.\n"        \
    "\n"                                                                 \
    "Elements of EVENT are [time type value number init-p],\n"           \
    "where \"type\" is :button or :axis. Returns EVENT on success,\n"    \
    "or if no events are available."

int plugin_is_GPL_compatible;

#ifdef __unix__
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <linux/joystick.h>

static void
fin_close(void *fdptr)
{
    int fd = (intptr_t)fdptr;
    if (fd != -1)
        close(fd);
}

static emacs_value
joymacs_open(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)ptr;
    (void)n;
    int id = env->extract_integer(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return S("nil");
    char buf[64];
    int buflen = sprintf(buf, "/dev/input/js%d", id);
    int fd = open(buf, O_RDONLY | O_NONBLOCK);
    if (fd == -1) {
        emacs_value signal = env->intern(env, "file-error");
        emacs_value message = env->make_string(env, buf, buflen);
        env->non_local_exit_signal(env, signal, message);
        return S("nil");
    }
    return env->make_user_ptr(env, fin_close, (void *)(intptr_t)fd);
}


static emacs_value
joymacs_close(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)ptr;
    (void)n;
    int fd = (intptr_t)env->get_user_ptr(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return S("nil");
    if (fd != -1) {
        close(fd);
        env->set_user_ptr(env, args[0], (void *)(intptr_t)-1);
    }
    return S("nil");
}


static emacs_value
joymacs_read(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)n;
    (void)ptr;
    int fd = (intptr_t)env->get_user_ptr(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return S("nil");
    struct js_event e;
    int r = read(fd, &e, sizeof(e));
    if (r == -1 && errno == EAGAIN) {
        /* No more events. */
        return S("nil");
    } if (r == -1) {
        /* An actual read error (joystick unplugged, etc.). */
        emacs_value signal = env->intern(env, "file-error");
        const char *error = strerror(errno);
        emacs_value message = env->make_string(env, error, strlen(error));
        env->non_local_exit_signal(env, signal, message);
        return S("nil");
    } else {
        /* Fill out event vector. */
        emacs_value v = args[1];
        emacs_value button = S(":button");
        emacs_value type = e.type & JS_EVENT_BUTTON ? button : S(":axis");
        emacs_value value;
        if (type == button)
            value = e.value ? S("t") : S("nil");
        else
            value =  env->make_float(env, e.value / (double)INT16_MAX);
        env->vec_set(env, v, 0, env->make_integer(env, e.time));
        env->vec_set(env, v, 1, type);
        env->vec_set(env, v, 2, value);
        env->vec_set(env, v, 3, env->make_integer(env, e.number));
        env->vec_set(env, v, 4, e.type & JS_EVENT_INIT ? S("t") : S("nil"));
        return args[1];
    }
}

#elif defined(__WIN32__)
#define WIN32_LEAN_AND_MEAN
#include <stdlib.h>
#include <stdint.h>
#include <windows.h>
#include <xinput.h>

struct handle {
    XINPUT_STATE prev;
    XINPUT_STATE next;
    uint64_t time;
    DWORD id;
    int report;
    int init;
};

static int handle_count = 0;

static void
fin_close(void *ptr)
{
    if (ptr) {
        free(ptr);
        if (!--handle_count)
            XInputEnable(FALSE);
    }
}

uint64_t
uepoch(void)
{
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    uint64_t tt = ft.dwHighDateTime;
    tt <<= 32;
    tt |= ft.dwLowDateTime;
    tt /=10;
    tt -= UINT64_C(11644473600000000);
    return tt;
}

static emacs_value
joymacs_open(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)n;
    (void)ptr;
    DWORD id = env->extract_integer(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return S("nil");
    if (!handle_count++)
        XInputEnable(TRUE);
    XINPUT_STATE state;
    if (XInputGetState(id, &state) != ERROR_SUCCESS) {
        emacs_value signal = env->intern(env, "file-error");
        const char str[] = "Invalid joystick";
        emacs_value message = env->make_string(env, str, sizeof(str) - 1);
        env->non_local_exit_signal(env, signal, message);
        if (!--handle_count)
            XInputEnable(FALSE);
        return S("nil");
    } else {
        struct handle *h = malloc(sizeof(*h));
        h->prev = state;
        h->next = state;
        unsigned char *bytes = (unsigned char *)&h->prev;
        for (unsigned char *p = bytes; p < bytes + sizeof(h->prev); p++)
            *p ^= (unsigned char)-1;
        h->time = uepoch();
        h->id = id;
        h->report = 0;
        h->init = 0;
        return env->make_user_ptr(env, fin_close, h);
    }
}


static double
normalize(SHORT state, SHORT min, SHORT max, int dead)
{
    if (abs(state) < dead)
        return 0.0f;
    else if (state < 0)
        return (state + dead) / (float)(min + dead);
    else
        return -(state - dead) / (float)(max - dead);
}

static inline emacs_value
vecf(emacs_env *env, struct handle *h, emacs_value v, double value)
{
    env->vec_set(env, v, 0, env->make_integer(env, h->time));
    env->vec_set(env, v, 1, S(":axis"));
    env->vec_set(env, v, 2, env->make_float(env, value));
    env->vec_set(env, v, 3, env->make_integer(env, h->report - 1));
    env->vec_set(env, v, 4, h->init ? S("nil") : S("t"));
    return v;
}

static inline emacs_value
vecb(emacs_env *env, struct handle *h, emacs_value v, emacs_value value)
{
    env->vec_set(env, v, 0, env->make_integer(env, h->time));
    env->vec_set(env, v, 1, S(":button"));
    env->vec_set(env, v, 2, value);
    env->vec_set(env, v, 3, env->make_integer(env, h->report - 9));
    env->vec_set(env, v, 4, h->init ? S("nil") : S("t"));
    return v;
}

static emacs_value
joymacs_read(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)n;
    (void)ptr;
    struct handle *h = env->get_user_ptr(env, args[0]);
    if (!h)
        return S("nil");
    emacs_value b;
    emacs_value t = S("t");
    emacs_value nil = S("nil");
    emacs_value v = args[1];
    XINPUT_GAMEPAD *g0 = &h->prev.Gamepad;
    XINPUT_GAMEPAD *g1 = &h->next.Gamepad;
    for (;;) {
        switch (h->report) {
            /* Axis */
            case 0:
                h->report++;
                if (g0->sThumbLX != g1->sThumbLX) {
                    double f = -normalize(g1->sThumbLX,
                                          INT16_MIN, INT16_MAX,
                                          XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE);
                    return vecf(env, h, v, f);
                }
                /* Fall through! */
            case 1:
                h->report++;
                if (g0->sThumbLY != g1->sThumbLY) {
                    double f = -normalize(g1->sThumbLY,
                                          INT16_MIN, INT16_MAX,
                                          XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE);
                    return vecf(env, h, v, f);
                }
                /* Fall through! */
            case 2:
                h->report++;
                if (g0->bLeftTrigger != g1->bLeftTrigger) {
                    double f = g1->bLeftTrigger * 2.0 / UINT8_MAX - 1;
                    return vecf(env, h, v, f);
                }
                /* Fall through! */
            case 3:
                h->report++;
                if (g0->sThumbRX != g1->sThumbRX) {
                    double f = -normalize(g1->sThumbRX,
                                          INT16_MIN, INT16_MAX,
                                          XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE);
                    return vecf(env, h, v, f);
                }
                /* Fall through! */
            case 4:
                h->report++;
                if (g0->sThumbRY != g1->sThumbRY) {
                    double f = -normalize(g1->sThumbRY,
                                          INT16_MIN, INT16_MAX,
                                          XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE);
                    return vecf(env, h, v, f);
                }
                /* Fall through! */
            case 5:
                h->report++;
                if (g0->bRightTrigger != g1->bRightTrigger) {
                    double f = g1->bRightTrigger * 2.0 / UINT8_MAX - 1;
                    return vecf(env, h, v, f);
                }
                /* Fall through! */
            case 6: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_DPAD_LEFT ||
                    (b0 ^ b1) & XINPUT_GAMEPAD_DPAD_RIGHT) {
                    double f = 0.0;
                    if (b1 & XINPUT_GAMEPAD_DPAD_LEFT)
                        f = -1.0;
                    else if (b1 & XINPUT_GAMEPAD_DPAD_RIGHT)
                        f = 1.0;
                    return vecf(env, h, v, f);
                }
            } /* Fall through! */
            case 7:
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_DPAD_UP ||
                    (b0 ^ b1) & XINPUT_GAMEPAD_DPAD_DOWN) {
                    double f = 0.0;
                    if (b1 & XINPUT_GAMEPAD_DPAD_UP)
                        f = -1.0;
                    else if (b1 & XINPUT_GAMEPAD_DPAD_DOWN)
                        f = 1.0;
                    return vecf(env, h, v, f);
                }
                /* Fall through! */

            /* Buttons */
            case 8: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_A) {
                    b = b1 & XINPUT_GAMEPAD_A ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 9: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_B) {
                    b = b1 & XINPUT_GAMEPAD_B ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 10: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_X) {
                    b = b1 & XINPUT_GAMEPAD_X ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 11: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_Y) {
                    b = b1 & XINPUT_GAMEPAD_Y ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 12: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_LEFT_THUMB) {
                    b = b1 & XINPUT_GAMEPAD_LEFT_THUMB ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 13: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_RIGHT_THUMB) {
                    b = b1 & XINPUT_GAMEPAD_RIGHT_THUMB ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 14: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_BACK) {
                    b = b1 & XINPUT_GAMEPAD_BACK ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 15: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_START) {
                    b = b1 & XINPUT_GAMEPAD_START ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 16: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_LEFT_SHOULDER) {
                    b = b1 & XINPUT_GAMEPAD_LEFT_SHOULDER ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */
            case 17: {
                h->report++;
                WORD b0 = g0->wButtons;
                WORD b1 = g1->wButtons;
                if ((b0 ^ b1) & XINPUT_GAMEPAD_RIGHT_SHOULDER) {
                    b = b1 & XINPUT_GAMEPAD_RIGHT_SHOULDER ? t : nil;
                    return vecb(env, h, v, b);
                }
            } /* Fall through! */

            case 18:
                /* No more differences detected. */
                h->prev = h->next;
                h->init = 1;
                if (XInputGetState(h->id, &h->next) != ERROR_SUCCESS) {
                    emacs_value signal = env->intern(env, "file-error");
                    const char str[] = "Failed to read joystick state";
                    size_t z = sizeof(str) - 1;
                    emacs_value message = env->make_string(env, str, z);
                    env->non_local_exit_signal(env, signal, message);
                    return nil;
                }
                if (h->prev.dwPacketNumber == h->next.dwPacketNumber)
                    return nil;
                h->report = 0;
                h->time = uepoch();
                break; // (loop)
        }
    }
    return 0;
}

static emacs_value
joymacs_close(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
    (void)n;
    (void)ptr;
    struct handle *h = env->get_user_ptr(env, args[0]);
    if (h) {
        if (!--handle_count)
            XInputEnable(FALSE);
        free(h);
        env->set_user_ptr(env, args[0], 0);
    }
    return S("t");
}

#endif

int
emacs_module_init(struct emacs_runtime *ert)
{
    emacs_env *env = ert->get_environment(ert);

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
