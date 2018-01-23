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

TRAVIS = os.environ.get('TRAVIS')
TRAVIS_COMMIT = os.environ.get('TRAVIS_COMMIT')
TRAVIS_PULL_REQUEST = os.environ.get('TRAVIS_PULL_REQUEST')
TRAVIS_REPO_SLUG = os.environ.get('TRAVIS_REPO_SLUG')
GITHUB_TOKEN = os.environ.get('GITHUB_TOKEN')

if TRAVIS_PULL_REQUEST == 'false':
    TRAVIS_PULL_REQUEST = False

# The PR job has all the needed info to compute deltas.
# But GitHub shows statuses for the commit associated with the push job.
# For the PR job, this will be the status URL for the push job.
TRAVIS_STATUS_URL = None


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
    headers = {'Authorization': 'token ' + GITHUB_TOKEN}
    request = urllib2.Request(url, None, headers)
    r = urllib2.urlopen(request)
    raise_for_status(url, r)
    return json.loads(r.read())


def get_base_size(filename):
    global TRAVIS_STATUS_URL
    if not TRAVIS_PULL_REQUEST:
        return None
    pr = get_pr_info(TRAVIS_REPO_SLUG, TRAVIS_PULL_REQUEST)
    sha = pr['base']['sha']
    url = pr['base']['repo']['statuses_url'].replace('{sha}', sha)
    TRAVIS_STATUS_URL = pr['statuses_url']
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

    if not TRAVIS:
        sys.stderr.write('Not Travis; exiting\n')
        sys.exit(0)

    if not TRAVIS_COMMIT:
        sys.stderr.write('Missing TRAVIS_COMMIT\n')
        sys.exit(1)
        
    if not TRAVIS_REPO_SLUG:
        sys.stderr.write('Missing TRAVIS_REPO_SLUG\n')
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

    if TRAVIS_STATUS_URL:
        url = TRAVIS_STATUS_URL
    else:
        url = 'https://api.github.com/repos/%s/statuses/%s' % (TRAVIS_REPO_SLUG, TRAVIS_COMMIT)

    prev_status = get_status(url, filename)
    if prev_status:
        old_cur, old_prev = parse_description(prev_status)
        print 'Previous status was "%s" => %s / %s' % (prev_status, old_cur, old_prev)
        if old_prev and not previous_size:
            print 'Since I have no delta information, I will refrain from POSTing.'
            sys.exit(0)

    print 'POSTing to %s' % url
    post_status(url, 'success', filename, format_description(current_size, previous_size))
