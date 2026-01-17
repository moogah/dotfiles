# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a dotfiles repository focused on a modular Emacs configuration using literate programming (org-mode), along with ZSH shell configuration. The repository supports multiple machines through hostname-specific and role-based configurations.

**Key Principle**: Org source files → generated code → modular loading → machine overrides

**IMPORTANT**: When working with Emacs configuration, ALWAYS use the relevant skills (emacs-literate-programming, writing-elisp, emacs-modular-config, emacs-elisp-debugging). See "Critical Rules for Claude" section below.

## High-Level Architecture

### Emacs Configuration System

The Emacs configuration uses a custom modular loading system:

1. **Entry Point**: `emacs/init.org` tangles to `emacs/init.el`
2. **Module Loader**: `jf/load-module()` with error handling and optional debug mode
3. **Module Resolution**: `jf/resolve-module-path()` converts paths like `core/defaults` to `core/defaults.el`
4. **Module Registry**: `jf/enabled-modules` list defines what loads and in what order
5. **Machine-Specific Overrides**: `emacs/local/{machine-role}.el` auto-loads at the end

**Critical Insight**: Module load order matters. Core modules must load before features that depend on them.

### Literate Programming

All configuration is written in org-mode files with these patterns:

```org
#+property: header-args:emacs-lisp :tangle file.el
#+auto_tangle: y

* Section
#+begin_src emacs-lisp
;; Code here tangles to file.el
#+end_src
```

**Never edit `.el` files directly** - they are generated and will be overwritten. Always edit the `.org` source files.

### Machine Configuration Strategy

**Two-tier system**:
1. **Machine Role** (via `~/.machine-role` file): Stable identifier (`apploi-mac`, `personal-mac`, `personal-mac-air`)
2. **Hostname** (via `system-name`): Dynamic identifier used for journal directories

The role system solves the problem of hostname changes breaking configurations.

### Package Management

- **Manager**: straight.el (not package.el)
- **Configuration**: use-package with `straight-use-package-by-default t`
- **Org Version**: Explicitly managed to ensure org-roam compatibility
- **Package Source Code**: All packages are cloned to `/Users/jefffar/.emacs.d/straight/repos/`
  - When working with package configurations, you can read the actual package source code
  - Example: gptel source is at `/Users/jefffar/.emacs.d/straight/repos/gptel/`
  - Useful for understanding package APIs, available functions, and default behaviors

## Directory Structure

```
dotfiles/
├── .claude/
│   ├── hooks/                   # validate_elisp_syntax.py
│   ├── skills/                  # Modular Claude knowledge
│   ├── instructions.md          # Main instructions (decomposed)
│   └── settings.local.json
├── bin/
│   ├── tangle-org.sh           # CLI tangling script
│   └── get-machine-role.sh     # Machine role detection
├── emacs/
│   ├── core/                   # Core functionality (load first)
│   ├── language-modes/         # Programming language configs
│   ├── major-modes/            # Major modes (org, magit, gpt)
│   ├── look-and-feel/          # UI and themes
│   ├── local/                  # Machine-specific {role}.el files
│   └── init.org               # Main entry point
├── oh-my-zsh/custom/          # ZSH custom scripts and plugins
├── dotbot/                    # Dotbot submodule for symlinking
├── install.conf.yaml          # Dotbot configuration
├── zshrc.org                  # ZSH config (tangles to zshrc)
├── bootstrap.org              # ZSH setup documentation
└── [other dotfiles]
```

## Common Commands

### Tangling Org Files

```bash
# From repository root
./bin/tangle-org.sh emacs/major-modes/org.org
./bin/tangle-org.sh emacs/init.org
./bin/tangle-org.sh zshrc.org

# Batch tangle all org files in a directory
find emacs/ -name "*.org" -exec ./bin/tangle-org.sh {} \;
```

**In Emacs**: `C-c C-v t` (org-babel-tangle)

**Auto-tangle**: Files with `#+auto_tangle: y` tangle automatically on save

### Installing Dotfiles on New Machine

```bash
cd ~/src/dotfiles
./install  # Runs dotbot to create symlinks
```

### ZSH Setup

See `bootstrap.org` for complete setup documentation. Key steps:

```bash
# Install Antidote
git clone --depth=1 https://github.com/mattmc3/antidote.git ~/.antidote

# Install Oh My Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install FZF
brew install fzf

# Tangle zshrc.org
./bin/tangle-org.sh zshrc.org

# Run dotbot
./install
```

### Emacs Module Management

**In Emacs**:
- `jf/reload-module`: Reload a specific module (interactive)
- Set `jf/module-debug t` for verbose loading
- Check `*Messages*` buffer for errors

**Adding a new module**:
1. Create `{category}/{name}.org` with proper header args
2. Add entry to `jf/enabled-modules` in `emacs/init.org`
3. Tangle and restart Emacs (or reload module)

## Key Architectural Patterns

### Error Resilience

The module loader uses `condition-case-unless-debug` to continue loading even if individual modules fail. This prevents one broken module from blocking the entire configuration.

### Module Dependencies

Module load order in `jf/enabled-modules`:
1. **core/** - Foundational functionality (defaults, evil, completion)
2. **look-and-feel/** - UI configuration
3. **language-modes/** - Programming languages
4. **major-modes/** - Major mode configs (org, magit, etc.)
5. **local/{role}.el** - Machine-specific overrides (auto-loaded)

### Org-Roam & Knowledge Management

**Location**: `~/org/roam/`
**Dailies**: `~/org/roam/dailies/{hostname}/YYYY-MM-DD.org`

The hostname-based dailies directory allows multiple machines to maintain separate journals while sharing the same org-roam database.

**Agenda System**:
- Auto-refreshes on agenda entry and org-roam operations
- Scans `~/org/` and `~/src/dotfiles/emacs/` recursively
- Refresh manually: `C-c r` (jf/refresh-org-agenda-files)

### Syntax Validation Hook

`.claude/hooks/validate_elisp_syntax.py` intercepts file writes to `.el` files and validates syntax using Emacs batch mode before allowing the operation. This prevents breaking the configuration with invalid elisp.

## File Naming Conventions

| Type | Pattern | Example |
|------|---------|---------|
| Module files | `{category}/{name}.org` + `.el` | `core/completion.org` → `core/completion.el` |
| Daily journals | `YYYY-MM-DD.org` | `2025-01-05.org` |
| Org-roam notes | `YYYYMMDDHHMMSS-{slug}.org` | `20250105120000-emacs-config.org` |
| Machine configs | `{role}.el` | `apploi-mac.el` |

## Branch Strategy

- **master**: Main development branch
- **apploi_main**: Work-specific configurations

Machine-specific configs in `local/` allow the same repository to work across different environments.

## Critical Rules for Claude

### Required Skills for Emacs Work

**ALWAYS invoke these skills when working with Emacs configuration:**

1. **emacs-literate-programming**: Use this skill for ANY work involving .org or .el files in the emacs/ directory
2. **writing-elisp**: Use this skill when writing or modifying elisp code, especially complex forms
3. **emacs-modular-config**: Use this skill when adding/modifying modules or debugging module loading
4. **emacs-elisp-debugging**: Use this skill when encountering elisp errors or mysterious issues

These skills provide critical context and patterns. Invoke them proactively, not just when stuck.

### General Rules

1. **Never edit `.el` files** - Always edit the `.org` source
2. **Always tangle after editing** - Use auto-tangle or manual tangling
3. **Commit both files** - Commit both `.org` and generated `.el` files
4. **Test syntax** - The validation hook will catch elisp errors, but test in Emacs too
5. **Module order matters** - Core modules must load before dependent modules
6. **Use machine configs** - Never put machine-specific or secret data in main files
7. **Preserve lexical-binding** - All elisp files should have `; -*- lexical-binding: t; -*-`
8. **Check package source** - When configuring packages, read their source code in `/Users/jefffar/.emacs.d/straight/repos/{package-name}/` to understand available functions and options

## Common Configuration Locations

- **Keybindings**: Often in module files, major-modes for context-specific binds
- **Package config**: Each module typically configures related packages
- **UI settings**: `look-and-feel/` directory
- **Completion**: `core/completion/` directory (vertico, consult, embark, etc.)
- **Evil mode**: `core/evil.org`
- **Org-mode**: `major-modes/org.org`
- **LLM/AI**: `major-modes/gpt.org` (gptel integration)

## Debugging

1. **Startup issues**: Uncomment profiling in `init.org` and check `*Messages*`
2. **Module errors**: Set `jf/module-debug t` for verbose output
3. **Tangle issues**: Use `./bin/tangle-org.sh` with full path for clear error messages
4. **Syntax errors**: Emacs batch mode: `emacs --batch -l file.el`
5. **Package issues**: `M-x straight-rebuild-package` or delete `~/.emacs.d/straight/`
6. **Understanding packages**: Read package source code in `/Users/jefffar/.emacs.d/straight/repos/{package-name}/`

## Additional Context

### ZSH Plugin System

Uses Antidote for plugin management with static loading (generates `.zsh_plugins.zsh`). Plugins defined in `.zsh_plugins.txt` which is tangled from `zshrc.org`.

### Dotbot

Used for symlink management. Configuration in `install.conf.yaml` with conditional linking based on machine role.

### Skills System

`.claude/skills/` contains modular knowledge for specialized Claude Code assistance:
- `dotfiles-conventions`: Repository structure and naming
- `emacs-literate-programming`: Org-mode tangling workflows
- `emacs-modular-config`: Module system usage
- `machine-specific-config`: Machine override patterns
- `org-roam-knowledge-mgmt`: Knowledge management workflows
