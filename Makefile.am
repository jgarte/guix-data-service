# guix-data-service -- Information about Guix over time
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
  scripts/guix-data-service-process-job			\
  scripts/guix-data-service-process-jobs			\
  scripts/guix-data-service-process-branch-updated-email	\
  scripts/guix-data-service-process-branch-updated-mbox	\
  scripts/guix-data-service-manage-build-servers		\
  scripts/guix-data-service-query-build-servers		\
  scripts/guix-data-service-query-substitute-servers		\
  scripts/guix-data-service-backup-database			\
  scripts/guix-data-service-create-small-backup

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir  = $(prefix)/lib/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
assetsdir = $(datadir)/@PACKAGE@

README.html: README
	emacs -q --no-splash -batch -visit README \
		-eval "(progn (require 'org) (let ((org-export-htmlize-output-type 'css)) (org-html-export-to-html nil nil nil t nil)))"

install-data-local: README.html
	mkdir -p "$(DESTDIR)$(pkgdatadir)" || exit 1;
	cp -r assets "$(DESTDIR)$(pkgdatadir)/" || exit 1;
	cp -r sqitch "$(DESTDIR)$(pkgdatadir)/" || exit 1;
	mkdir -p "$(DESTDIR)$(docdir)";
	cp README "$(DESTDIR)$(docdir)/README"
	cp README.html "$(DESTDIR)$(docdir)/README.html"

install-data-hook:
# These files shouldn't be installed, but I have no idea how to specify that
# :(
	rm -r "$(DESTDIR)$(moddir)/tests"

check-with-tmp-database:
	$(eval TMP_DATABASE := $(shell mktemp -d -t guix-data-service-database-XXXX))
# -w 0 means that the database won't be automatically shutdown, and needs to
# -be stopped by running pg_tmp stop
	$(eval URI := $(shell pg_tmp -w 0 -d "$(TMP_DATABASE)"))
	$(eval GUIX_DATA_SERVICE_DATABASE_URI := $(patsubst postgresql:///test%,postgresql://guix_data_service@localhost/guix_data_service_test%,$(URI)))
	psql --no-psqlrc "$(URI)" --command="CREATE USER guix_data_service"
	psql --no-psqlrc "$(URI)" --command="CREATE DATABASE guix_data_service_test WITH OWNER guix_data_service"
# Set TZ and SQITCH_USER_CONFIG to avoid Sqitch tripping up in a minimal
# environment, like a Guix package
	TZ=UTC SQITCH_USER_CONFIG=/dev/null sqitch deploy --target="$(GUIX_DATA_SERVICE_DATABASE_URI)"
	GUIX_DATA_SERVICE_DATABASE_URI="$(GUIX_DATA_SERVICE_DATABASE_URI)" make check
	pg_tmp stop -w 1 -d "$(TMP_DATABASE)"

SOURCES =									\
  guix-data-service/branch-updated-emails.scm					\
  guix-data-service/builds.scm							\
  guix-data-service/comparison.scm						\
  guix-data-service/config.scm							\
  guix-data-service/database.scm						\
  guix-data-service/metrics.scm						\
  guix-data-service/substitutes.scm						\
  guix-data-service/utils.scm							\
  guix-data-service/data-deletion.scm						\
  guix-data-service/jobs.scm							\
  guix-data-service/jobs/load-new-guix-revision.scm				\
  guix-data-service/model/build-server.scm					\
  guix-data-service/model/build-server-token-seed.scm				\
  guix-data-service/model/build-status.scm					\
  guix-data-service/model/build.scm						\
  guix-data-service/model/channel-instance.scm					\
  guix-data-service/model/channel-news.scm					\
  guix-data-service/model/derivation.scm					\
  guix-data-service/model/git-branch.scm					\
  guix-data-service/model/git-repository.scm					\
  guix-data-service/model/guix-revision-package-derivation.scm			\
  guix-data-service/model/guix-revision.scm					\
  guix-data-service/model/license-set.scm					\
  guix-data-service/model/license.scm						\
  guix-data-service/model/lint-checker.scm					\
  guix-data-service/model/lint-warning-message.scm				\
  guix-data-service/model/lint-warning.scm					\
  guix-data-service/model/location.scm						\
  guix-data-service/model/nar.scm						\
  guix-data-service/model/package-derivation-by-guix-revision-range.scm	\
  guix-data-service/model/package-derivation.scm				\
  guix-data-service/model/package-metadata.scm					\
  guix-data-service/model/package.scm						\
  guix-data-service/model/system-test.scm					\
  guix-data-service/model/utils.scm						\
  guix-data-service/web/build-server/controller.scm				\
  guix-data-service/web/build-server/html.scm					\
  guix-data-service/web/build/controller.scm					\
  guix-data-service/web/build/html.scm						\
  guix-data-service/web/compare/controller.scm					\
  guix-data-service/web/compare/html.scm					\
  guix-data-service/web/dumps/controller.scm					\
  guix-data-service/web/package/controller.scm					\
  guix-data-service/web/package/html.scm					\
  guix-data-service/web/dumps/html.scm						\
  guix-data-service/web/controller.scm						\
  guix-data-service/web/html-utils.scm						\
  guix-data-service/web/jobs/controller.scm					\
  guix-data-service/web/jobs/html.scm						\
  guix-data-service/web/nar/controller.scm					\
  guix-data-service/web/nar/html.scm						\
  guix-data-service/web/query-parameters.scm					\
  guix-data-service/web/render.scm						\
  guix-data-service/web/repository/controller.scm				\
  guix-data-service/web/repository/html.scm					\
  guix-data-service/web/revision/controller.scm				\
  guix-data-service/web/revision/html.scm					\
  guix-data-service/web/server.scm						\
  guix-data-service/web/sxml.scm						\
  guix-data-service/web/util.scm						\
  guix-data-service/web/view/html.scm						\
  tests/driver.scm								\
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
  tests/model-lint-checker.scm			\
  tests/model-lint-warning-message.scm		\
  tests/model-package.scm			\
  tests/model-package-metadata.scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_DRIVER = $(top_builddir)/test-env	\
  $(GUILE) --no-auto-compile -e main		\
      $(top_srcdir)/build-aux/test-driver.scm

# AM_SCM_LOG_DRIVER_FLAGS = --brief=yes
