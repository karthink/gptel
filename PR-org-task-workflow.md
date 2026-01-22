# Pull Request: Org-mode AI Task Workflow Enhancements

## Overview

This PR introduces comprehensive org-mode integration for AI-assisted task workflows. All changes were developed using AI assistant collaboration within gptel itself, serving as both a feature implementation and a real-world test of the workflow.

**Stats:** +1,966 lines across 7 files (2 new modules)

**Example:** See [emacs-example-ai.org](./emacs-example-ai.org) for a real-world example of this workflow in action.

## Summary of Changes

This PR adds three major feature areas:

1. **Subtree Context Mode** - Scoped conversations under TODO headings
2. **Tag-based Message Detection & Highlighting** - Use org tags (`:assistant:`, `:user:`) instead of text patterns
3. **AI Task Workflow Module** - TODO state transitions and model profile management
4. **Archive Module** - Summarize and archive completed AI conversations

---

## Feature 1: Subtree Context Mode

**Files:** `gptel-org.el`

Enables task-oriented conversations where chat happens under a TODO heading:

```org
*** TODO Implement feature X
**** @user                                              :user:
Please help me implement...
**** @assistant                                         :assistant:
I'll help you with that...
***** Implementation details
Here's the code...
```

### New Options

| Option                           | Default                   | Description                                              |
|----------------------------------|---------------------------|----------------------------------------------------------|
| `gptel-org-subtree-context`      | `nil`                     | Include sibling `@user`/`@assistant` headings in context |
| `gptel-org-chat-heading-markers` | `'("@user" "@assistant")` | Markers identifying chat entries                         |
| `gptel-org-save-state`           | `t`                       | Control automatic property saving                        |

### Behavior

- When enabled, gptel collects sibling chat headings as conversation context
- Response headings automatically use correct depth (one level below task heading)
- Headings in AI responses are demoted to stay within the assistant subtree
- Special org syntax (`*`, `#+`) in example blocks is properly escaped with commas

### Key Functions

- `gptel-org--get-parent-heading-level` - Finds task heading level
- `gptel-org--get-chat-siblings` - Collects sibling chat headings
- `gptel-org--dynamic-prefix-string` - Adjusts prefix heading levels
- `gptel-org--adjust-response-headings` - Demotes headings in responses

---

## Feature 2: Tag-based Message Detection

**Files:** `gptel-org.el`, `gptel.el`

Use org heading tags instead of text properties to identify message roles:

```org
*** Question about X                                    :user:
*** Here's the answer                                   :assistant:
```

### New Options

| Option                             | Default                 | Description                                            |
|------------------------------------|-------------------------|--------------------------------------------------------|
| `gptel-org-infer-bounds-from-tags` | `t`                     | Detect message bounds from `:assistant:`/`:user:` tags |
| `gptel-org-assistant-tag`          | `"assistant"`           | Tag name for assistant messages                        |
| `gptel-org-user-tag`               | `"user"`                | Tag name for user messages                             |
| `gptel-org-model-from-user-tag`    | `t`                     | Detect model from tags on user headings                |
| `gptel-org-model-from-todo-tag`    | `t`                     | Detect model from tags on TODO headings                |
| `gptel-org-todo-keywords`          | `'("AI-DO" "AI-DOING")` | TODO keywords for AI task headings                     |

### Benefits

- Works naturally with org's tag infrastructure
- Tags are visible, searchable, and filterable
- No text property manipulation needed
- Integrates with `gptel-highlight-mode`

---

## Feature 3: Highlight Mode Org Integration

**Files:** `gptel.el`

`gptel-highlight-mode` now creates overlays based on heading tags in org-mode:

### Behavior

- Scans headings with `:assistant:` tag
- Creates overlays spanning entire subtree
- Adds fringe/margin markers
- Updates dynamically when tags change (via `org-after-tags-change-hook`)
- Updates on visibility changes (via `org-cycle-hook`)
- Compatible with `org-indent-mode`

---

## Feature 4: AI Task Workflow Module

**Files:** `gptel-org-tasks.el` (new), `gptel-anthropic.el`

Provides TODO state management and model profile switching:

```org
#+SEQ_TODO: AI-DO(a) AI-DOING(i!) FEEDBACK(f@) CANCELED(c) | AI-DONE(d!) HI-DONE(D!)

** AI-DO Implement feature X                            :haiku:
** AI-DO Complex analysis                               :opus:
```

### Features

- **Auto state transition:** `AI-DO` → `AI-DOING` when `gptel-send` is called
- **Model profiles:** Tag-based model selection (`:haiku:`, `:sonnet:`, `:opus:`)
- **Abort handling:** `AI-DOING` → `CANCELED` when `gptel-abort` is called
- **Built-in Anthropic aliases:** `haiku`, `sonnet`, `opus` work out of the box

### New Options

| Option | Default | Description |
|--------|---------|-------------|
| `gptel-org-tasks-todo-keyword` | `"AI-DO"` | Ready-for-AI keyword |
| `gptel-org-tasks-doing-keyword` | `"AI-DOING"` | Processing keyword |
| `gptel-org-tasks-canceled-keyword` | `"CANCELED"` | Aborted keyword |
| `gptel-org-tasks-apply-profile-on-send` | `t` | Apply model from tag |
| `gptel-org-tasks-change-state-on-send` | `t` | Auto-transition states |

### Commands

- `gptel-org-tasks-mode` - Enable the workflow integration
- `gptel-org-tasks-set-profile` - Interactively apply a profile
- `gptel-org-tasks-show-profiles` - Display all defined profiles
- `gptel-org-tasks-define-profile` - Define custom model profiles

### Anthropic Model Aliases

Added to `gptel-anthropic.el`:

```elisp
(gptel-org-tasks-define-profile 'haiku
  :backend "Claude" :model 'claude-3-5-haiku-latest)
(gptel-org-tasks-define-profile 'sonnet  
  :backend "Claude" :model 'claude-sonnet-4-20250514)
(gptel-org-tasks-define-profile 'opus
  :backend "Claude" :model 'claude-opus-4-0-20250514)
```

---

## Feature 5: Archive Module

**Files:** `gptel-org-archive.el` (new)

Archive AI conversations with automatic summarization:

### Workflow

1. Mark task as `AI-DONE` or similar done state
2. `M-x gptel-org-prepare-archive` generates a summary
3. Review summary (use `gptel-org-restore-original` to undo)
4. `C-c C-x C-s` (`org-archive-subtree`) to archive

### Features

- **Auto-archive:** Optionally trigger on TODO state change to DONE
- **Git metadata extraction:** Captures repo, branch, and commits from conversation
- **Smart archive location:** `*-ai.org` → `*-ai-archive.org`
- **Metadata preservation:** Archive date, models used, git info

### New Options

| Option | Default | Description |
|--------|---------|-------------|
| `gptel-org-archive-auto-on-done` | `nil` | Auto-prepare on DONE transition |
| `gptel-org-archive-include-metadata` | `t` | Include metadata in summary |
| `gptel-org-archive-summary-max-tokens` | `500` | Max tokens for summary |
| `gptel-org-archive-location-function` | `#'gptel-org-archive--default-location` | Archive path function |

### Commands

- `gptel-org-prepare-archive` - Summarize current DONE task
- `gptel-org-restore-original` - Undo summarization
- `gptel-org-archive-done-tasks` - Batch process all DONE tasks

### Archive Properties

```org
:PROPERTIES:
:ARCHIVE_DATE: [2025-01-22 Wed 14:30]
:ORIGINAL_BACKEND: Claude
:ORIGINAL_MODEL: claude-sonnet-4-20250514
:SUMMARY_BACKEND: Claude
:SUMMARY_MODEL: claude-sonnet-4-20250514
:GIT_REPO: git@github.com:user/repo.git
:GIT_COMMIT: abc1234
:GIT_BRANCH: main
:GIT_PATH: quelpa/build/gptel
:GIT_COMMITS: abc1234, def5678
:END:
```

---

## Bug Fixes & Improvements

### Response Handling

- Run post-response hooks and insert prompt on abort/cancel
- Fix `@user` prefix insertion timing (after hooks run)
- Change prefix-string functions from `defsubst` to `defun` (for advice compatibility)

### Org Integration

- Fix sibling overlap detection and ordering in subtree context
- Properly escape headings inside literal example blocks with comma prefix
- Don't double-escape already-escaped lines
- Fix `org-archive-subtree` point position in async callback
- Refresh assistant tag bounds before query construction
- Unify org-mode assistant/user tag checking

### Highlight Mode

- Fix `org-indent-mode` conflict
- Fix dynamic updates for Org tag changes

### Task Workflow

- Don't transition to `AI-DOING` on dry-run (`I` transient arg)
- Don't transition when opening transient menu (prefix arg)
- Add recursive guards for advice functions to prevent re-entry

---

## Commit Log (Chronological)

<details>
<summary>Click to expand full commit list (56 commits)</summary>

```
1c3a70b feat(org): Add subtree context mode for task-oriented conversations
cc21ac7 fix(org): Handle non-starred prefixes in subtree context mode
fc28ad2 fix(org): Demote response headings in subtree context mode
4d3da1f fix(org): Correctly identify parent heading for new conversations
6436641 fix: Change prefix-string functions from defsubst to defun
7fb13bd fix: Insert @user prefix after post-response hooks run
52d6e46 Add gptel-org-archive.el for AI task conversation archival
9135ab5 Fix gptel-org-archive: use gptel-max-tokens instead of :max-tokens
f1a1eb4 Fix gptel-org-archive: preserve heading level in summaries
df9f1d9 Add git repository metadata to archive summaries
422ac50 Extract git commits from AI summary into archive metadata
e130ef9 Add gptel-org-save-state to control automatic property saving
7dd7ecc Add auto-archive on DONE for AI task documents
d7cda3f feat(org-tasks): add AI task workflow with model profiles
996a207 feat(anthropic): add simple model aliases (haiku, sonnet, opus)
1eb2fbd feat(org-tasks): auto-detect model aliases as profiles
dbc6aaf fix(org-tasks): advise gptel--suffix-send for transient menu support
2655eec fix(org-tasks): don't transition to AI-DOING when opening transient menu
f28000e Fix gptel-org-archive-done-tasks to match custom TODO keywords
b525ce0 gptel-org-tasks: transition to CANCELED state on gptel-abort
ed9941b feat(org): infer message bounds from heading tags
fdeb1ff Support tag-based chat heading detection in gptel-org--chat-heading-p
909f0bf Add recursive guards for advice functions
08d1cd6 Fix sibling overlap detection and ordering in subtree context
ad63630 Prefix headings inside literal examples with comma
8d8a609 Don't double-escape already-escaped lines in example blocks
54bb5b7 gptel: Run post-response hooks and insert prompt on abort/cancel
0a75787 gptel-highlight-mode: Add Org tag-based highlighting
6a85a58 gptel-highlight-mode: Fix dynamic updates for Org tag changes
b20704e Unify org-mode assistant/user tag checking
50663d9 Refresh assistant tag bounds before query construction
c02e49e Fix org-archive-subtree point position in async callback
de175ae Fix org-indent-mode conflict in gptel-highlight-mode
f58453c Fix chat-heading-p to recognize prefix-based headings
0b5ccb3 Remove prefix-based detection, use tags only for org chat headings
49827a2 fix(org-tasks): don't transition to AI-DOING on dry-run
fe300c1 gptel-org: Add model selection from user heading tags
0f8ee1e Add model tag detection for TODO headings
```

</details>

---

## Testing

Tests are included in the `test` submodule covering:

- Subtree context mode behavior
- Git metadata extraction in archive
- Prefix-based and tag-based chat heading detection
- Conversation boundary detection

Run tests with:
```bash
cd test && make test
```

---

## Breaking Changes

None. All new features are opt-in via customization options.

---

## Migration Guide

### Recommended Setup for AI Task Workflow

1. Add to your org file:
   ```org
   #+SEQ_TODO: AI-DO(a) AI-DOING(i!) FEEDBACK(f@) CANCELED(c) | AI-DONE(d!) HI-DONE(D!)
   ```

2. Enable the modes:
   ```elisp
   (add-hook 'gptel-mode-hook #'gptel-org-tasks-mode)
   (setq gptel-org-subtree-context t)
   (setq gptel-org-infer-bounds-from-tags t)
   ```

3. Use tags for model selection:
   ```org
   ** AI-DO Complex task                                  :opus:
   ```

---

## Notes

- All commits in this PR were created with AI assistant collaboration using gptel
- The workflow itself was used to develop and test these features
- This serves as validation that the task workflow is practical for real development
