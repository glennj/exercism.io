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

The bearer auth does not seem to be required when using curl from the
cmdline: in a GHA workflow, it is required.

In order to accomodate a pull request with many many changed files, I've updated the workflow in Tcl to use the `gh` cli tool. This should also make the `PULL_REQUEST_URL` contents more transparent, although here it's not an url, it's an API endpoint.
```none
- name: Run tests for changed/added exercises
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
  run: |
    pr_endpoint=$(jq -r '"repos/\(.repository.full_name)/pulls/\(.pull_request.number)"' "$GITHUB_EVENT_PATH")
    gh api "$pr_endpoint/files" --paginate --jq '
      .[] | 
        select(.status == "added" or .status == "modified") |
        select(.filename | match("\\.(md|tcl|test)$")) |
        .filename
    ' | xargs -r bash scripts/pr
```

---

# Some useful Docker one-liners

... but there's gotta be a better way...

## Delete all containers
```bash
docker container ls -a | awk 'NR>1 {print $1}' | xargs docker container rm
```

## Delete all un-named images
```bash
docker image ls | awk '$1 == "<none>" {print $3}' | xargs docker image rm
```
