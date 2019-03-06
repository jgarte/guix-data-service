# guix-data-service - Mediocre, uh, mail interface
# Copyright © 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
# Copyright © 2019 Christopher Baines <mail@cbaines.net>
#
# This file is part of guix-data-service.
#
# guix-data-service is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# guix-data-service is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the guix-data-service.  If not, see
# <http://www.gnu.org/licenses/>.

include guile.am

bin_SCRIPTS =						\
  scripts/guix-data-service				\
  scripts/guix-data-service-process-jobs		\
  scripts/guix-data-service-query-build-servers

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir  = $(moddir)
assetsdir = $(datadir)/@PACKAGE@

SOURCES =						\
  guix-data-service/builds.scm				\
  guix-data-service/comparison.scm			\
  guix-data-service/config.scm				\
  guix-data-service/jobs.scm				\
  guix-data-service/jobs/load-new-guix-revision.scm	\
  guix-data-service/model/build-server.scm		\
  guix-data-service/model/build-status.scm		\
  guix-data-service/model/build.scm			\
  guix-data-service/model/derivation.scm		\
  guix-data-service/model/guix-revision-package.scm	\
  guix-data-service/model/guix-revision.scm		\
  guix-data-service/model/package-metadata.scm		\
  guix-data-service/model/package.scm			\
  guix-data-service/model/utils.scm			\
  guix-data-service/web/controller.scm			\
  guix-data-service/web/render.scm			\
  guix-data-service/web/server.scm			\
  guix-data-service/web/sxml.scm			\
  guix-data-service/web/util.scm			\
  guix-data-service/web/view/html.scm
