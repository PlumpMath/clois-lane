# clois-lane

**status:** stalled (please check out [Glop](https://github.com/patzy/glop)
which doesn't depend on a C++ library)


## CFFI bindings to OIS


### Introduction

clois-lane provides CFFI bindings to the
[OIS](http://en.wikipedia.org/wiki/Object_Oriented_Input_System)
library.

The Object-Oriented Input System (OIS) is the default input system for
[Ogre](http://www.ogre3d.org/) and these CFFI bindings work together
with the [Okra](http://github.com/aerique/okra) project which provides
access to Ogre from Common Lisp.


### License

This project is released under the simplified
[BSD](http://www.opensource.net/licenses/bsd-license.php) license.


### To Do

* add joystick support
* add Wii support (Windows only?)
* make OS device settings that are hardcoded in C++ configurable from
  CL


### Platforms

The code has currently been tested on Linux using SBCL and on Windows
XP using Clozure CL and MinGW.


### Version numbering

clois-lane follows the OIS version numbering with its own version
number tagged on at the end. This way you can easily see which version
of OIS these bindings are written for and what the latest release of
clois-lane itself is.
