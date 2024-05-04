# guile-sdl2 --- FFI bindings for SDL2
# Copyright © 2015, 2016 David Thompson <davet@gnu.org>
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

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =					\
  sdl2.scm					\
  sdl2/config.scm				\
  sdl2/bindings.scm				\
  sdl2/blend-mode.scm				\
  sdl2/clipboard.scm				\
  sdl2/hints.scm				\
  sdl2/rect.scm					\
  sdl2/surface.scm				\
  sdl2/render.scm				\
  sdl2/video.scm				\
  sdl2/audio.scm				\
  sdl2/events.scm				\
  sdl2/input/keyboard.scm			\
  sdl2/input/joystick.scm			\
  sdl2/input/game-controller.scm		\
  sdl2/input/mouse.scm				\
  sdl2/input/text.scm				\
  sdl2/input/sensor.scm

if HAVE_SDL2_IMAGE

SOURCES +=					\
  sdl2/bindings/image.scm			\
  sdl2/image.scm

endif

if HAVE_SDL2_TTF

SOURCES +=					\
  sdl2/bindings/ttf.scm				\
  sdl2/ttf.scm

endif

if HAVE_SDL2_MIXER

SOURCES +=					\
  sdl2/bindings/mixer.scm			\
  sdl2/mixer.scm

endif

info_TEXINFOS = doc/guile-sdl2.texi

doc_guile_sdl2_TEXINFOS =			\
  doc/fdl.texi					\
  doc/api.texi					\
  doc/guile-sdl2.texi

AM_MAKEINFOHTMLFLAGS = --css-ref=https://dthompson.us/css/dthompson.css \
  --css-include=doc/manual.css

#dvi: # Don't build dvi docs

EXTRA_DIST +=					\
  pre-inst-env.in				\
  README					\
  guix.scm					\
  run-example					\
  examples/hello.scm				\
  examples/hello.bmp				\
  examples/controller.scm

publish: dist
	gpg --sign --detach-sign --armor --yes guile-sdl2-$(VERSION).tar.gz && \
        scp guile-sdl2-$(VERSION).tar.gz guile-sdl2-$(VERSION).tar.gz.asc \
	    publish@dthompson.us:/var/www/files/guile-sdl2/

.PHONY: publish
