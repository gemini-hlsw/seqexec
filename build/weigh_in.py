#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Track changes in the size of a file using Travis-CI and GitHub statuses.

Usage:
    weigh_in.py path/to/file

This should be run inside of a Travis-CI worker.
It will post the current size of the file as a GitHub status on the commit.

If it's able to deduce the size of the file before the change, it will report
the size delta. This requires that this script was run on the base for a Pull
Request, e.g. the commit that was merged into master.
"""

import os
import re
import sys
import json
import urllib2

BUILDKITE = os.environ.get('BUILDKITE')
BUILDKITE_COMMIT = os.environ.get('BUILDKITE_COMMIT')
BUILDKITE_PULL_REQUEST = os.environ.get('BUILDKITE_PULL_REQUEST')
BUILDKITE_REPO = os.environ.get('BUILDKITE_REPO')
BUILDKITE_BRANCH = os.environ.get('BUILDKITE_BRANCH')
GITHUB_TOKEN = os.environ.get('GITHUB_TOKEN')

# Buildkite doesn't give the slug directly, get it via regex
print BUILDKITE_REPO
match = re.match( r'((git|ssh|http(s)?)|(git@[\w\.]+))(:(//)?)([\w\.@\:/\-~]+)(\.git)?(/)?', BUILDKITE_REPO)
if not match:
    sys.stderr.write('Cannot parse the repo\n')
    sys.exit(1)

splitted = match.group(7).split('/')
if splitted.length == 3:
    owner = match.group(7).split('/')[1]
    repo = match.group(7).split('/')[2]
else:
    owner = match.group(7).split('/')[0]
    repo = match.group(7).split('/')[1]

slug = '%s/%s' % (owner, repo)

if BUILDKITE_PULL_REQUEST == 'false':
    BUILDKITE_PULL_REQUEST = False

# The PR job has all the needed info to compute deltas.
# But GitHub shows statuses for the commit associated with the push job.
# For the PR job, this will be the status URL for the push job.
STATUS_URL = None


def raise_for_status(url, response):
    if response.getcode() < 200 or response.getcode() >= 300:
        sys.stderr.write(response.read())
        sys.stderr.write('\n')
        raise Exception('Request for %s failed: %s' % (url, response.getcode()))


def post_status(url, state, context, description):
    data = {
        'state': state,
        'context': context,
        'description': description
    }
    headers = {'Authorization': 'token ' + GITHUB_TOKEN}

    request = urllib2.Request(url, json.dumps(data), headers)
    r = urllib2.urlopen(request)
    raise_for_status(url, r)

    print 'Posted %s' % json.dumps(data)


def get_status(url, context):
    headers = {'Authorization': 'token ' + GITHUB_TOKEN}
    request = urllib2.Request(url, None, headers)
    r = urllib2.urlopen(request)
    raise_for_status(url, r)

    data = json.loads(r.read())
    for status in data:
        if status['context'] == context:
            return status['description']
    return None


def get_pr_info(slug, pull_number):
    url = 'https://api.github.com/repos/%s/pulls/%s' % (slug, pull_number)
    print url
    headers = {'Authorization': 'token ' + GITHUB_TOKEN}
    request = urllib2.Request(url, None, headers)
    r = urllib2.urlopen(request)
    raise_for_status(url, r)
    return json.loads(r.read())


def get_base_size(filename):
    global STATUS_URL
    if not BUILDKITE_PULL_REQUEST:
        return None
    pr = get_pr_info(slug, BUILDKITE_PULL_REQUEST)
    sha = pr['base']['sha']
    url = pr['base']['repo']['statuses_url'].replace('{sha}', sha)
    STATUS_URL = pr['statuses_url']
    print STATUS_URL
    assert sha in url, 'statuses_url %s missing "{sha}"' % url
    status = get_status(url, filename)
    if not status:
        sys.stderr.write('Unable to find status %s for base at %s\n' % (filename, url))
        return None
    return parse_description(status)[0]


def format_description(current_size, previous_size):
    if previous_size:
        delta = current_size - previous_size
        if delta == 0:
            return 'No change ({:,.0f} bytes)'.format(current_size)
        pct = 100.0 * delta / current_size
        return '{:+,.0f} bytes ({:+0.2f}%) → {:,.0f} bytes'.format(
                delta, pct, current_size)
    return '{:,d} bytes'.format(current_size)


def parse_description(description):
    m = re.search(r'([0-9,]+) bytes\)?$', description)
    assert m, 'Unable to parse "%s"' % description
    current_size = int(m.group(1).replace(',', '').replace(' bytes', ''))

    prev_size = None
    m = re.search(r'([-+][0-9,]+) bytes', description)
    if m:
        delta = int(m.group(1).replace(',', '').replace(' bytes', ''))
        prev_size = current_size - delta
    elif 'No change' in description:
        prev_size = current_size

    return current_size, prev_size


def check_environment():
    if not GITHUB_TOKEN:
        sys.stderr.write('The GITHUB_TOKEN environment variable must be set.\n')
        sys.exit(1)

    if not BUILDKITE:
        sys.stderr.write('Not Travis; exiting\n')
        sys.exit(0)

    if not BUILDKITE_COMMIT:
        sys.stderr.write('Missing BUILDKITE_COMMIT\n')
        sys.exit(1)

    if not slug:
        sys.stderr.write('Missing slug\n')
        sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write('Usage: %s path/to/file\n' % sys.argv[0])
        sys.exit(1)
    filename = sys.argv[1]

    check_environment()

    current_size = os.stat(filename).st_size
    previous_size = get_base_size(filename)

    print '%s Current:  %s' % (filename, current_size)
    print '%s Previous: %s' % (filename, previous_size)

    if STATUS_URL:
        url = STATUS_URL
    else:
        url = 'https://api.github.com/repos/%s/statuses/%s' % (slug, BUILDKITE_COMMIT)

    prev_status = get_status(url, filename)
    if prev_status:
        old_cur, old_prev = parse_description(prev_status)
        print 'Previous status was "%s" => %s / %s' % (prev_status, old_cur, old_prev)
        if old_prev and not previous_size:
            print 'Since I have no delta information, I will refrain from POSTing.'
            sys.exit(0)

    print 'POSTing to %s' % url
    post_status(url, 'success', filename, format_description(current_size, previous_size))
