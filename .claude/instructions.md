# Dotfiles Repository Guide

This repository contains Jeff's personal dotfiles with a focus on a modular Emacs configuration system using literate programming.

## Repository Overview

- **Location**: `/Users/jefffarr/src/dotfiles`
- **Git-controlled**: Yes, with branches including `apploi_main` and `master`
- **Main Focus**: Emacs configuration using org-mode literate programming
- **Machine Support**: Multi-machine setup with hostname-specific configurations

## Emacs Configuration Architecture

### Directory Structure
```
emacs/
├── core/                    # Core functionality modules
├── language-modes/          # Programming language configurations  
├── major-modes/            # Major mode configurations (org, magit, etc.)
├── look-and-feel/          # UI and appearance settings
├── local/                  # Machine-specific configurations
├── init.org               # Main configuration file (tangles to init.el)
└── modular_init.el        # Generated initialization file
```

### Key Concepts

**Literate Configuration**: 
- Configuration written in org-mode files with documentation
- Uses `#+auto_tangle: y` and `#+property: header-args:emacs-lisp :tangle file.el`
- Org files tangle to corresponding `.el` files

**Modular Loading System**:
- `jf/enabled-modules` list defines what gets loaded
- `jf/load-module()` function with error handling
- `jf/resolve-module-path()` converts module paths to file paths
- Machine-specific configs auto-loaded from `local/{hostname}.el`

**Package Management**:
- Uses straight.el package manager
- `straight-use-package-by-default t`

### Machine-Specific Configuration

**Current Machine**: "Mac.home" (detected via `system-name`)

**Adding Machine-Specific Settings**:
1. Create `emacs/local/{hostname}.el` 
2. Add configuration overrides (setq statements, hooks, etc.)
3. File automatically loaded during init

**Common Machine-Specific Configs**:
- `org-roam-dailies-directory` for hostname-based journal directories
- Custom keybindings or package configurations
- Local paths and credentials

## Org-roam & Knowledge Management

### Directory Structure
```
~/org/
├── roam/                   # Main org-roam directory
│   ├── dailies/           # Daily journal files
│   │   └── Mac.home/      # Machine-specific dailies
│   └── *.org             # Knowledge base files
└── agenda/                # Other org files
```

### Daily Journals
- **Location**: `~/org/roam/dailies/{hostname}/`
- **Format**: `YYYY-MM-DD.org` (date-only filenames)
- **Template**: Contains Meetings, Worklog, and Tasks sections

### Agenda System
- **Agenda Files**: Automatically scans `~/org/` and `~/src/dotfiles/emacs/` recursively
- **Refresh Function**: `jf/refresh-org-agenda-files` (bound to `C-c r`)
- **Auto-refresh**: Triggered on agenda mode entry and org-roam operations
- **Custom Views**: Priority-based views and enhanced TODO sorting

## Development Workflow

### Adding New Modules
1. Create `{category}/{name}.org` file
2. Add to `jf/enabled-modules` list in `init.org`
3. Use standard org-mode code blocks with `:tangle {name}.el`
4. Reload with `jf/reload-module` for testing

### Making Configuration Changes
1. Edit the `.org` file (not the `.el` file)
2. Tangle with one of these methods:
   - **Auto-tangle**: Save the file if `#+auto_tangle: y` is set
   - **Manual tangle in Emacs**: `C-c C-v t` 
   - **External script**: `./bin/tangle-org.sh path/to/file.org`
3. Restart Emacs or reload specific module

### Tangling with bin/tangle-org.sh

The repository includes a convenient shell script for tangling org files outside of Emacs:

**Usage**:
```bash
# From repository root
./bin/tangle-org.sh emacs/major-modes/org.org
./bin/tangle-org.sh emacs/init.org
```

**Features**:
- Automatically finds Emacs installation (macOS app or PATH)
- Validates org file exists and has .org extension
- Uses batch mode for fast, non-interactive tangling
- Provides clear success/error feedback

**When to use**:
- Tangling files when Emacs isn't running
- Batch processing multiple files
- CI/CD or automation scripts
- Quick command-line tangling

### Debugging Issues
- Set `jf/module-debug t` for verbose loading
- Use `jf/reload-module` to reload specific modules
- Check `*Messages*` buffer for error details
- Module loading has error handling that continues on failure

## Key Functions & Commands

### Configuration Management
- `jf/load-module`: Load a module with error handling
- `jf/reload-module`: Reload specific module for debugging
- `jf/resolve-module-path`: Convert module path to file path

### Agenda & Org-roam
- `jf/refresh-org-agenda-files`: Refresh agenda file list (C-c r)
- `org-roam-dailies-goto-today`: Jump to today's journal (C-c n d)
- `org-agenda`: Open agenda (C-c a)

### Key Variables
- `jf/emacs-dir`: Root emacs config directory
- `jf/machine-name`: Current machine hostname
- `jf/enabled-modules`: List of modules to load

## Common Tasks

### Adding a New Machine
1. Note the machine's hostname (`hostname` command)
2. Create `emacs/local/{hostname}.el` if needed
3. Add machine-specific overrides (e.g., daily journal directory)

### Troubleshooting Agenda
- Run `jf/refresh-org-agenda-files` manually (C-c r)
- Check that files exist in expected locations
- Restart Emacs if agenda files seem stale

### Package Issues
- Straight.el packages in `~/.emacs.d/straight/`
- Force rebuild: `M-x straight-rebuild-package`
- Clear cache: Delete straight directories

### Batch Tangling Multiple Files
```bash
# Tangle all org files in a directory
find emacs/ -name "*.org" -exec ./bin/tangle-org.sh {} \;

# Tangle specific modules
./bin/tangle-org.sh emacs/init.org
./bin/tangle-org.sh emacs/major-modes/org.org
./bin/tangle-org.sh emacs/core/defaults.org
```

## File Naming Conventions

- **Module files**: `{category}/{name}.org` and `{category}/{name}.el`
- **Daily journals**: `YYYY-MM-DD.org` 
- **Org-roam files**: `YYYYMMDDHHMMSS-{slug}.org`
- **Machine configs**: `{hostname}.el`

## Claude Code Hooks

### Elisp Syntax Validation

The repository includes a pre-tool-use hook that validates Emacs Lisp syntax:

**Location**: `.claude/hooks/validate_elisp_syntax.py`

**Purpose**: Prevents invalid elisp from being written to `.el` files in the `emacs/` directory

**How it works**:
- Intercepts Write, Edit, and MultiEdit operations on `.el` files
- Simulates the proposed changes
- Uses Emacs batch mode with `check-parens` to validate syntax
- Blocks operations that would introduce syntax errors

**Graceful degradation**:
- If Emacs isn't found, allows operations to proceed
- Only validates files in `emacs/` directory
- Allows operations on non-elisp files without validation

## Important Notes

- **Always edit .org files**, not .el files (they get overwritten)
- **Auto-tangle is enabled** for most config files
- **External tangling**: Use `bin/tangle-org.sh` for command-line tangling
- **Syntax protection**: Hook validates elisp syntax before file writes
- **Error handling** allows config to continue loading even if modules fail
- **Module order matters** - core modules load first
- **Machine detection** is automatic via `system-name`
- **Agenda refresh** happens automatically but can be triggered manually

## Repository Branches

- `master`: Main development branch
- `apploi_main`: Work-specific branch (current)
- Machine configs allow same repo to work across multiple environments