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

## when a student ends a session without applying suggestions

You'll get more learning from a mentoring session if you apply the suggestions and submit new iterations.
Often new questions come up in the back-and-forth between student and mentor that increase the benefits.
Plus, you have the benefit of seeing the progression of your solutions through multiple iterations.

## Ending a mentoring conversation:

Have fun with the next exercise; feel free to post here if you have more comments or questions.
<!-- -->
If you're satisfied with this solution, feel free to "End discussion" to finalize the mentoring process.

Whenever you're ready to move on, feel free to "End discussion".

<!-- -->
I'd encourage you to submit another iteration. You'll get more learning from a mentoring session if you apply the suggestions and submit new iterations. Often new questions come up in the back-and-forth between student and mentor that increase the benefits. Plus, you have the benefit of seeing the progression of your solutions through multiple iterations.

Whenever you're ready to move on, feel free to "End discussion".
<!-- -->


"
Sometimes the test runner infrastructure can be a bit flaky.
The returned error message gives no insight.
I'd suggest making a tiny cosmetic change and submit again to trigger a new test run.
"
