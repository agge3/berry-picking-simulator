# -*- Autoconf -*-
#
# guile-sdl2 --- FFI bindings for SDL2
# Copyright © 2015, 2020 David Thompson <davet@gnu.org>
#
# This file is part of guile-sdl2.
#
# Guile-sdl2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# Guile-sdl2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-sdl2.  If not, see
# <http://www.gnu.org/licenses/>.

AC_INIT(guile-sdl2, 0.8.0)
AC_CONFIG_SRCDIR(sdl2)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_PATH_PROG([GUILE], [guile])
AC_CONFIG_FILES([Makefile sdl2/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

# Core SDL2
PKG_CHECK_MODULES([SDL2], [sdl2])
PKG_CHECK_VAR([SDL2_LIBDIR], [sdl2], [libdir])
AC_MSG_CHECKING([SDL2 library path])
AS_IF([test "x$SDL2_LIBDIR" = "x"], [
  AC_MSG_FAILURE([unable to find SDL2 library directory])
], [
  AC_MSG_RESULT([$SDL2_LIBDIR])
])
AC_SUBST([SDL2_LIBDIR])

# SDL2_image
PKG_CHECK_MODULES([SDL2_image], [SDL2_image], [
  PKG_CHECK_VAR([SDL2_IMAGE_LIBDIR], [SDL2_image], [libdir])
  AC_MSG_CHECKING([SDL2_image library path])
  AS_IF([test "x$SDL2_IMAGE_LIBDIR" = "x"], [
    AC_MSG_RESULT([none])
  ], [
    AC_MSG_RESULT([$SDL2_IMAGE_LIBDIR])
  ])
], [ ]) # do not throw an error if lib is not found
AM_CONDITIONAL([HAVE_SDL2_IMAGE], [test "x$SDL2_IMAGE_LIBDIR" != "x"])
AC_SUBST([SDL2_IMAGE_LIBDIR])

# SDL2_ttf
PKG_CHECK_MODULES([SDL2_ttf], [SDL2_ttf], [
  PKG_CHECK_VAR([SDL2_TTF_LIBDIR], [SDL2_ttf], [libdir])
  AC_MSG_CHECKING([SDL2_ttf library path])
  AS_IF([test "x$SDL2_TTF_LIBDIR" = "x"], [
    AC_MSG_RESULT([none])
  ], [
    AC_MSG_RESULT([$SDL2_TTF_LIBDIR])
  ])
], [ ]) # do not throw an error if lib is not found
AM_CONDITIONAL([HAVE_SDL2_TTF], [test "x$SDL2_TTF_LIBDIR" != "x"])
AC_SUBST([SDL2_TTF_LIBDIR])

# SDL2_mixer
PKG_CHECK_MODULES([SDL2_mixer], [SDL2_mixer], [
  PKG_CHECK_VAR([SDL2_MIXER_LIBDIR], [SDL2_mixer], [libdir])
  AC_MSG_CHECKING([SDL2_mixer library path])
  AS_IF([test "x$SDL2_MIXER_LIBDIR" = "x"], [
    AC_MSG_RESULT([none])
  ], [
    AC_MSG_RESULT([$SDL2_MIXER_LIBDIR])
  ])
], [ ]) # do not throw an error if lib is not found
AM_CONDITIONAL([HAVE_SDL2_MIXER], [test "x$SDL2_MIXER_LIBDIR" != "x"])
AC_SUBST([SDL2_MIXER_LIBDIR])

AC_OUTPUT
