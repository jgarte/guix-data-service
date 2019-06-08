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

bin_SCRIPTS =							\
  scripts/guix-data-service					\
  scripts/guix-data-service-process-jobs			\
  scripts/guix-data-service-process-branch-updated-email	\
  scripts/guix-data-service-query-build-servers

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir  = $(moddir)
assetsdir = $(datadir)/@PACKAGE@

install-data-local:
	mkdir -p "$(DESTDIR)$(pkgdatadir)" || exit 1;
	cp -r assets "$(DESTDIR)$(pkgdatadir)/" || exit 1;
	cp -r sqitch "$(DESTDIR)$(pkgdatadir)/" || exit 1;

install-data-hook:
# These files shouldn't be installed, but I have no idea how to specify that
# :(
	rm -r "$(DESTDIR)$(moddir)/tests"

SOURCES =							\
  guix-data-service/branch-updated-emails.scm			\
  guix-data-service/builds.scm					\
  guix-data-service/comparison.scm				\
  guix-data-service/config.scm					\
  guix-data-service/database.scm				\
  guix-data-service/jobs.scm					\
  guix-data-service/jobs/load-new-guix-revision.scm		\
  guix-data-service/model/build-server.scm			\
  guix-data-service/model/build-status.scm			\
  guix-data-service/model/build.scm				\
  guix-data-service/model/derivation.scm			\
  guix-data-service/model/git-branch.scm			\
  guix-data-service/model/git-repository.scm			\
  guix-data-service/model/guix-revision-package-derivation.scm	\
  guix-data-service/model/guix-revision.scm			\
  guix-data-service/model/license.scm				\
  guix-data-service/model/license-set.scm			\
  guix-data-service/model/location.scm				\
  guix-data-service/model/package-derivation.scm		\
  guix-data-service/model/package-metadata.scm			\
  guix-data-service/model/package.scm				\
  guix-data-service/model/utils.scm				\
  guix-data-service/web/controller.scm				\
  guix-data-service/web/render.scm				\
  guix-data-service/web/server.scm				\
  guix-data-service/web/sxml.scm				\
  guix-data-service/web/query-parameters.scm			\
  guix-data-service/web/util.scm				\
  guix-data-service/web/view/html.scm				\
  tests/driver.scm						\
  tests/mock-inferior.scm

TEST_EXTENSIONS = .scm

TESTS =					\
  tests/branch-updated-emails.scm		\
  tests/jobs-load-new-guix-revision.scm	\
  tests/model-derivation.scm			\
  tests/model-git-branch.scm			\
  tests/model-git-repository.scm		\
  tests/model-license-set.scm			\
  tests/model-license.scm			\
  tests/model-package-metadata.scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_DRIVER = $(top_builddir)/test-env	\
  $(GUILE) --no-auto-compile -e main		\
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes
