# hephaestux

Track coding-agent sessions in tmux.

> From _Hephaestus_, ancient Greek god of artisans, which forged _kourai khryseai_, concious golden servants.

## Commands

- `hephaestux update --agent AGENT [--symbol SYMBOL] --state STATE [--title TITLE]` — record the current session. `STATE` is one of `stale`, `working`, `waiting-input`, `done`. Resolves the agent process via the `/proc` ancestor chain; requires a tmux context.
- `hephaestux end` — remove this session's record (idempotent; used by shutdown hooks).
- `hephaestux list [--tmux] [--state STATE] [--color-* STYLE]` — list recorded sessions; `--tmux` renders status segments.
- `hephaestux status --window W [--state STATE] [--no-padding] [--left-margin | --right-margin | --no-margin] [--color-* STYLE]` — render the tmux segments for one window.
- `hephaestux setup [--dry-run] [--agent AGENT]...` — install agent hooks. Without `--agent`, all supported agents are set up.

### Per-state colors

`--color-stale`, `--color-working`, `--color-waiting-input`, `--color-done` accept a tmux style fragment, brackets optional: `--color-working fg=#ffaaff` or `--color-working "[bg=#0011ff]"`.

Defaults: `stale` `fg=black`, `working` `fg=#ff8c00`, `waiting-input` `fg=#8b0000`, `done` `fg=#006400`.

## tmux integration

Per window (there is no all-sessions mode):

```tmux
set -ag window-status-format '#(hephaestux status --window #{window_index})'
set -ag window-status-current-format '#(hephaestux status --window #{window_index})'
```

Note: `set -ag` appends — re-sourcing tmux.conf duplicates the `#()` call, and a later plain `set window-status-format` drops it.

## JSON schema

`/tmp/hephaestux/<agent-pid>.json`:

```json
{"agent":{"name":"codex","symbol":"Cx"},"session":{"state":"waiting-input","title":"Refactor API"}}
```

`symbol` and `title` are optional and omitted when absent. Agent turn end reports `waiting-input`; `done` is only reachable via an explicit `update --state done`. Dead agent pids are filtered out of `list`/`status` output (liveness is checked via `kill(pid, 0)`; a recycled pid would keep a stale record visible — accepted trade-off).

## Agent support

| Agent | Hook mechanism | Events |
|---|---|---|
| omp | `~/.omp/agent/hooks/post/hephaestux.ts` | session_start→stale, agent_start→working, agent_end→waiting-input, session_shutdown→end |
| pi | `~/.pi/agent/extensions/hephaestux.ts` | same as omp |
| claude | `~/.claude/settings.json` (merged) | SessionStart→stale, UserPromptSubmit→working, Stop→waiting-input, SessionEnd→end |
| codex | `~/.codex/hooks.json` (merged) | same as claude (SessionEnd timeout 3 s) |
| opencode | `~/.config/opencode/plugins/hephaestux.ts` | session.created→stale, chat.message→working, session.idle→waiting-input, session.deleted→end |
| antigravity | `~/.gemini/config/hooks.json` | PreInvocation→working, Stop→waiting-input; no SessionStart (no stale) |
| kimi | `~/.kimi/config.toml` (appended) | SessionStart→stale, UserPromptSubmit→working, Stop→waiting-input, SessionEnd→end |
| qwen | `~/.qwen/settings.json` (merged) | same as claude |
| cline | `<cwd>/.cline/hooks/*` (scripts) | TaskStart→stale, TaskResume→working, UserPromptSubmit→working, TaskCancel→end; no idle event (no waiting-input) |
| deepseek | `./.hephaestux/dsh-hooks.json` + `./cordis-hephaestux.yml` | SessionStart→stale, UserPromptSubmit→working, Stop→waiting-input; no SessionEnd (dead-pid filtering covers cleanup) |

Setup notes:

- **codex**: run `/hooks` in codex to trust the newly installed hooks.
- **deepseek**: launch with `dsh web --patch cordis-hephaestux.yml`.
- **cline**: hooks are installed in the current working directory.
- Antigravity, opencode, cline, and deepseek have no session-end signal for cleanup, so ghost entries are hidden by pid liveness checks rather than deleted.

Generated hooks are failure-swallowing (`|| true` / `try-catch`): hephaestux can never break the agent. The `hephaestux` binary must be on PATH.
