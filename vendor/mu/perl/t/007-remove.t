#!/usr/bin/perl
##
# 007-remove.t - test removing messages
##
# Copyright (C) 2015 by attila <attila@stalphonsos.com>
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.
##
use strict;
use warnings;
use mup;
use Test::More tests => 6;

use t::lib;

my $mu = mup->new(verbose => $ENV{'TEST_VERBOSE'});
ok($mu,"constructor won");
my $f = $mu->find(query => "openbsd");
ok($f,"find returned something");
ok($f->{'found'} > 0,"found something");
my $d = $f->{'results'}->[0]->{'docid'};
my $r = $mu->remove(docid => $d);
ok($r,"remove returned something");
ok($r->{'remove'} eq $d,"and it even looks right");
ok($mu->finish(),"finish won");

##
# Local variables:
# mode: perl
# tab-width: 4
# perl-indent-level: 4
# perl-continued-statement-offset: 4
# indent-tabs-mode: nil
# comment-column: 40
# End:
##
