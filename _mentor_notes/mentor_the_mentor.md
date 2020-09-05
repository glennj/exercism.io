# Mentor the mentor

Tips and tricks fron the #mentor-the-mentor channel on Slack

@bitfield [2020-03-20](https://exercism-team.slack.com/archives/CM3SY1F2L/p1585402808004500?thread_ts=1585380904.001900&cid=CM3SY1F2L)
> I have a fairly standard set of prompts:
> 1. "Do we need X?" (students learn pretty quickly that this is a polite way of saying "We don't need X")
> 1. "Can we do better than X here?" (translation: "X is crap, think again")
> 1. "While X works, there's a more elegant way. Can you find it?"
> 1. "Instead of X, we could do Y, which is better because Z."
> 1. (only if really necessary and all Socratic method has failed) "Do this: [code sample]"

---
Something like "spoiler" text: use the `details` tag with a `summary`.  
Can be nested.  
No markdown in the summary.
Markdown in the details requires the `p` tag or a blank line.  
Example:
<details>
<summary>There is a more compact way to read the fields of a CSV-type line (expand for details)</summary>
  <details>
  <summary>Use <code>read</code> with a specific <code>IFS</code> value</summary>
  <p>

```bash
line='first;second;third'
IFS=';' read -r a b c <<<"$line"
```
  </p>
  </details>
</details>

---

