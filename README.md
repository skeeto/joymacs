# Joystick Dynamic Module for Emacs

If Emacs 25 is installed and a joystick is plugged in, try `make run`.

Supported platforms: Linux, Windows

Full article: [Emacs, Dynamic Modules, and
Joysticks](http://nullprogram.com/blog/2016/11/05/)

~~~el
(joymacs-open N)
;; Create a handle for the Nth joystick.

(joymacs-close JOYSTICK)
;; Immediately destroy JOYSTICK handle.

(joymacs-read JOYSTICK EVENT)
;; Fill 5-element vector EVENT with a single joystick event.
~~~

![](http://i.imgur.com/f3yzzzx.png)
