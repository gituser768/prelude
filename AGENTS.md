# Repository Guidelines

## Project Structure & Module Organization
- `init.el` orchestrates startup, extends the `load-path`, and loads each module in the order expected by the configuration.
- `early-init.el` tunes pre-frame settings such as GC and UI suppression; keep it lightweight so GUI startup stays fast.
- `lisp/core-*.el` holds editor fundamentals, while `lisp/mod-*.el` layers language- or workflow-specific features—follow this split when adding files.
- `vendor/` contains vendored libraries that are checked in; treat them as read-only and prefer `use-package` for new dependencies.
- `elpa/` is the package cache populated by Emacs; never hand-edit files there, and minimize diffs by regenerating via package installs.
- Runtime artifacts (`savefile/`, `auto-save-list/`, `transient/`, `projectile-bookmarks.eld`, `secrets`) should stay untracked; clean them before publishing work.

## Build, Test, and Development Commands
- `emacs --batch -Q -l init.el --eval '(message "Init OK")'` is the quickest sanity check that the configuration loads without interactive prompts.
- `emacs --batch -Q --load init.el --funcall batch-byte-compile lisp/*.el` byte-compiles every module; run it from the repo root to catch syntax errors and obsolete forms.
- During interactive debugging, start with `emacs -Q -l init.el --debug-init` so backtraces surface immediately when a new module misbehaves.

## Coding Style & Naming Conventions
- Write Emacs Lisp with two-space indentation, include `-*- lexical-binding: t -*-` headers, and keep sections annotated with `;;; ---` comments as seen in `lisp/core-defaults.el`.
- Wrap third-party packages in `use-package`, grouping related settings, keymaps, and hooks to mirror existing modules.
- Prefix helper functions with `my-` and end every file with `(provide 'module-name)` to maintain discoverability.
- Bindings belong in `core-keybindings.el` or the relevant `mod-*.el`; document unusual chords in a `;;; Notes` block.

## Testing Guidelines
- Always byte-compile (`batch-byte-compile`) before opening a pull request; warnings generally hint at runtime failures.
- Run `emacs --batch -Q -l init.el --eval '(dolist (f (directory-files "lisp" t "\\\\.el$")) (with-current-buffer (find-file-noselect f) (check-parens)))'` when editing structural code to catch unbalanced forms.
- For documentation updates or UX features, record the manual verification you performed (e.g., launching `mod-python` REPL support) inside the PR description.

## Commit & Pull Request Guidelines
- Craft concise, imperative commit messages such as `Refine core completion defaults`; group unrelated changes into separate commits.
- Reference touched modules in the body (e.g., ``Affects: lisp/mod-term.el``) so reviewers can skim diffs quickly.
- Complete `.github/PULL_REQUEST_TEMPLATE.md`, link related issues, list manual checks, and call out whether docs or demos in `old/` need refreshing.

## Security & Configuration Tips
- Keep secrets in the plain-text `secrets` file using `KEY=value`; never commit the file or hard-code credentials in modules.
- When introducing new environment variables, integrate them with `load-env-vars-from-file` helpers and document the expected keys in your PR.
