# Notes about track maintenance

## Github actions

Referring to the GHA workflows, specifically for pull requests:

```none
- name: Run tests for changed/added exercises
  run: |
    PULL_REQUEST_URL=$(jq -r ".pull_request.url" "$GITHUB_EVENT_PATH")
    curl --silent --url $"${PULL_REQUEST_URL}/files" --header 'authorization: Bearer ${{ secrets.GITHUB_TOKEN }}' | \
      jq -c '.[] | select(.status == "added" or .status == "modified") | select(.filename | match("\\.(md|tcl|test)$")) | .filename' | \
      xargs -r bash scripts/pr
```

The `PULL_REQUST_URL` is:
<code>https://api.github.com/repos/<em><strong>user</strong></em>/<em><strong>repo</strong></em>/pulls/<em><strong>number</strong></em></code>
```none
https://api.github.com/repos/exercism/tcl/pulls/179
```

The bearer auth does not seem to be required.
