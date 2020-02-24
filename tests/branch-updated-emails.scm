(define-module (test-branch-updated-emails)
  #:use-module (srfi srfi-64)
  #:use-module (email email)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service branch-updated-emails))

(define master-branch-updated-email
  "Return-Path: <guix-commits-bounces+patchwork=mira.cbaines.net@gnu.org>
X-Original-To: patchwork@mira.cbaines.net
Delivered-To: patchwork@mira.cbaines.net
Received: by mira.cbaines.net (Postfix, from userid 113)
	id 893C316F50; Fri, 26 Apr 2019 13:19:54 +0100 (BST)
X-Spam-Checker-Version: SpamAssassin 3.4.0 (2014-02-07) on mira.cbaines.net
X-Spam-Level: 
X-Spam-Status: No, score=-1.9 required=5.0 tests=BAYES_00,URIBL_BLOCKED
	autolearn=ham autolearn_force=no version=3.4.0
Received: from lists.gnu.org (lists.gnu.org [209.51.188.17])
	by mira.cbaines.net (Postfix) with ESMTP id 0169916F46
	for <patchwork@mira.cbaines.net>; Fri, 26 Apr 2019 13:19:51 +0100 (BST)
Received: from localhost ([127.0.0.1]:46383 helo=lists.gnu.org)
	by lists.gnu.org with esmtp (Exim 4.71)
	(envelope-from <guix-commits-bounces+patchwork=mira.cbaines.net@gnu.org>)
	id 1hJzpX-0004ZG-5K
	for patchwork@mira.cbaines.net; Fri, 26 Apr 2019 08:19:51 -0400
Received: from eggs.gnu.org ([209.51.188.92]:41385)
	by lists.gnu.org with esmtp (Exim 4.71)
	(envelope-from <ludo@gnu.org>) id 1hJzpT-0004WT-2H
	for guix-commits@gnu.org; Fri, 26 Apr 2019 08:19:47 -0400
Received: from Debian-exim by eggs.gnu.org with spam-scanned (Exim 4.71)
	(envelope-from <ludo@gnu.org>) id 1hJzpS-00037m-84
	for guix-commits@gnu.org; Fri, 26 Apr 2019 08:19:47 -0400
Received: from vcs0.savannah.gnu.org ([209.51.188.201]:48450)
	by eggs.gnu.org with esmtp (Exim 4.71) (envelope-from <ludo@gnu.org>)
	id 1hJzpS-00037O-4X
	for guix-commits@gnu.org; Fri, 26 Apr 2019 08:19:46 -0400
Received: by vcs0.savannah.gnu.org (Postfix, from userid 68006)
	id BD977209B1; Fri, 26 Apr 2019 08:19:45 -0400 (EDT)
To: guix-commits@gnu.org
Subject: branch master updated (9ca5ff8 -> 272db5b)
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8
Message-ID: <20190426121944.32203.70977@vcs0.savannah.gnu.org>
From: guix-commits@gnu.org
Mail-Followup-To: guix-devel@gnu.org
X-Git-Repo: guix
X-Git-Refname: refs/heads/master
X-Git-Reftype: branch
X-Git-Oldrev: 9ca5ff882e2ac4eaab02eb0fde545bd784af478b
X-Git-Newrev: 272db5bcf53d9d05d5c4b2df021d9e74f78866cd
Auto-Submitted: auto-generated
Date: Fri, 26 Apr 2019 08:19:45 -0400 (EDT)
Content-Transfer-Encoding: quoted-printable
X-detected-operating-system: by eggs.gnu.org: GNU/Linux 2.2.x-3.x [generic]
X-Received-From: 209.51.188.201
X-BeenThere: guix-commits@gnu.org
X-Mailman-Version: 2.1.21
Precedence: list
List-Id: <guix-commits.gnu.org>
List-Unsubscribe: <https://lists.gnu.org/mailman/options/guix-commits>,
	<mailto:guix-commits-request@gnu.org?subject=unsubscribe>
List-Archive: <http://lists.gnu.org/archive/html/guix-commits/>
List-Post: <mailto:guix-commits@gnu.org>
List-Help: <mailto:guix-commits-request@gnu.org?subject=help>
List-Subscribe: <https://lists.gnu.org/mailman/listinfo/guix-commits>,
	<mailto:guix-commits-request@gnu.org?subject=subscribe>
Errors-To: guix-commits-bounces+patchwork=mira.cbaines.net@gnu.org
Sender: \"Guix-commits\"
	<guix-commits-bounces+patchwork=mira.cbaines.net@gnu.org>

civodul pushed a change to branch master
in repository guix.

      from  9ca5ff8   bootstrap: Break automake dependency on generated f=
iles.
       new  504a0fc   accounts: Always honor the configured user account =
shell.
       new  538b99f   system: Provide a new VM image configuration.
       new  6c849cd   installer: Run wrapped program with 'execl', not 's=
ystem'.
       new  9529f78   installer: Take 'guix system init' exit code into a=
ccount.
       new  98f0354   installer: Actually reboot when the user presses \"R=
eboot.\"
       new  b57dd20   doc: Add 'BASE-URL' variable.
       new  272db5b   doc: Use ftp.gnu.org for downloads.

The 7 revisions listed above as \"new\" are entirely new to this
repository and will be described in separate emails.  The revisions
listed as \"adds\" were already present in the repository and have only
been added to this reference.


Summary of changes:
 doc/guix.texi                     | 39 ++++++++++-----
 gnu/build/accounts.scm            |  9 ++--
 gnu/installer.scm                 | 22 +++++++--
 gnu/installer/final.scm           |  5 +-
 gnu/installer/newt/final.scm      |  5 +-
 gnu/installer/utils.scm           | 26 ++++++++--
 gnu/system/examples/vm-image.tmpl | 99 +++++++++++++++++++++++++--------=
------
 7 files changed, 140 insertions(+), 65 deletions(-)

")

(test-begin "test-branch-updated-emails")

(with-postgresql-connection
 "test-branch-updated-emails"
 (lambda (conn)
   (check-test-database! conn)

   (test-assert "enqueue-job-for-email works"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (enqueue-job-for-email conn
                               (parse-email master-branch-updated-email)))
      #:always-rollback? #t))))

(test-end)
