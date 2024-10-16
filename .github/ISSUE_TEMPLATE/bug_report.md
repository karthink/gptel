---
name: Bug report
about: Create a report to help us improve
title: ''
labels: bug
assignees: ''

---

**Describe the bug**
A short description of what the bug is.

**To Reproduce**
How I can reproduce the behavior.

**Logging and/or simulation**
Depending on the bug, it might be relevant to see the data gptel sends and receives from the LLM, or simulate a request.

To see gptel's log,
1. Turn on logging by running `(setq gptel-log-level 'info)` (or `'debug`)
2. Use gptel
3. Check the `*gptel-log*` buffer

For a gptel request dry-run,
1. Run `(setq gptel-expert-commands t)`
2. From gptel's menu (`M-x gptel-menu`), use one of the newly visible `dry run` options.

**Backtrace**
If Emacs throws an error, please generate a backtrace as follows:
1. Run `M-x toggle-debug-on-error`
2. Reproduce the error
3. Paste the contents of the backtrace here

**Additional context**
Emacs version:
Operating System:
Any other context (Curl version etc, if relevant):
