#!/bin/bash

# This dynamically generates a pipeline containing a click to unblock step with
# custom fields.

set -eu

echo 'steps:'
echo '  - block: "Notify"'
echo '    fields:'
echo '      - select: "Which team should be notified?"'
echo '        key: "notify-team"'
echo '        default: "Ops"'
echo '        options:'

# This list could be populated from anywhere! The file system, the Internet,
# Slack, Basecamp… literally anywhere.
TEAMS="All None Billing Engineering Ops QA Sales Support"

for option in $TEAMS; do
echo "          - "$option""
done

echo '  - command: "notify.sh"'
echo '    label: ":pager:"'