;; -*- lexical-binding: t; -*-
(require 'projectile)

(defvar gptel-projectile-result-limit 40000
  "Maximum character count for tool results to prevent context overflow.")

(defun gptel-projectile--result-limit (result)
  "Limit RESULT to gptel-projectile-result-limit characters.
If exceeded, return a warning message instead."
  (if (>= (length (format "%s" result)) gptel-projectile-result-limit)
      (format "Results over %s characters. Use filter_pattern to narrow results or increase limit."
              gptel-projectile-result-limit)
    result))

(gptel-make-tool
 :name "list_known_projects"
 :function (lambda ()
             (let ((projects (projectile-known-projects)))
               (if (null projects)
                   "No known projects found. Open a project directory in Emacs first (C-x C-f to a project file), then projectile will track it."
                 (format "Known projects (%d):\n\n%s\n\nUse these paths with other projectile tools like get_project_info or list_project_files."
                         (length projects)
                         (mapconcat #'identity projects "\n")))))
 :description "List all known projects tracked by projectile.

CRITICAL: Use this tool FIRST when you don't know which project to work with.
This is especially important when running in a gptel buffer that isn't associated
with a file - there's no 'current project' context in that case.

Returns a list of absolute paths to all projects that projectile knows about.
These paths can then be used with other projectile tools:
- get_project_info(directory) - get project details
- list_project_files(directory) - list project files
- list_project_directories(directory) - list project structure
- expand_project_path(relative_path, directory) - resolve paths

Typical workflow:
1. Call list_known_projects() to see available projects
2. Identify the project you want (e.g., 'dotfiles' project)
3. Use that project's path with other tools

No arguments needed - returns all known projects."
 :args (list)
 :category "projectile")

(gptel-make-tool
 :name "get_project_info"
 :function (lambda (directory)
             (let ((project-root (projectile-project-root directory)))
               (if (not project-root)
                   (format "No project found at %s. This directory is not part of a projectile project.\n\nTip: Use list_known_projects first to see available projects." directory)
                 (let ((name (projectile-project-name project-root))
                       (type (projectile-project-type project-root))
                       (vcs (projectile-project-vcs project-root)))
                   (format "Project Info:\n\nName: %s\nRoot: %s\nType: %s\nVCS: %s\n\nUse list_project_files to see files, or list_project_directories to see directory structure."
                           name project-root (or type "generic") (or vcs "none"))))))
 :description "Get high-level overview of a specific project.

Returns project name, root directory, project type (ruby, python, nodejs, etc.),
and VCS system (git, hg, svn, none).

IMPORTANT: You must provide a directory path. Get project paths from list_known_projects first.

Typical workflow:
1. Call list_known_projects() to discover available projects
2. Use a project path from that list with this tool
3. Example: get_project_info('/Users/username/src/dotfiles')

Use this tool when you need to:
- Get details about a specific project
- Understand what type of project you're working with
- Know what version control system is used

After using this tool, use list_project_files or list_project_directories to explore
the project contents."
 :args (list '(:name "directory"
               :type string
               :description "Project directory path. Use list_known_projects to discover available projects."))
 :category "projectile")

(gptel-make-tool
 :name "list_project_files"
 :function (lambda (directory &optional limit filter-pattern)
             (let ((project-root (projectile-project-root directory)))
               (if (not project-root)
                   (format "No project found at %s. Use list_known_projects to discover available projects." directory)
                 (let* ((file-limit (min (or limit 100) 500))
                        (all-files (projectile-project-files project-root))
                        (filtered-files (if filter-pattern
                                            (seq-filter (lambda (f) (string-match-p filter-pattern f)) all-files)
                                          all-files))
                        (total-count (length filtered-files))
                        (display-files (seq-take filtered-files (min file-limit total-count)))
                        (result (concat
                                 (format "Project: %s\nTotal files: %d%s\nShowing: %d\n\n"
                                         project-root
                                         total-count
                                         (if filter-pattern (format " (filtered by: %s)" filter-pattern) "")
                                         (length display-files))
                                 (mapconcat #'identity display-files "\n")
                                 (when (> total-count file-limit)
                                   (format "\n\n[Truncated. Use filter_pattern to narrow results or increase limit (max 500)]")))))
                   (gptel-projectile--result-limit result)))))
 :description "List all files in the project (respecting ignore rules).

Returns a list of relative file paths within the project, respecting VCS ignore rules
(.gitignore, .hgignore, etc.) and projectile configuration. Files are listed relative
to the project root.

IMPORTANT: You must provide a directory path. Get project paths from list_known_projects first.

Typical workflow:
1. Call list_known_projects() to discover available projects
2. Use a project path from that list with this tool
3. Example: list_project_files('/Users/username/src/dotfiles', 100, '\\.el$')

Use this tool when you need to:
- See all files in a project
- Find files matching a pattern
- Understand project file structure
- Locate specific file types

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- limit: Max files to return (default 100, max 500)
- filter_pattern: Regex pattern to filter results (e.g., '\\.py$' for Python files, '^src/' for src directory)

The tool will warn if results exceed 40,000 characters. Use filter_pattern to narrow
results in large projects."
 :args (list '(:name "directory"
               :type string
               :description "Project directory path. Use list_known_projects to discover available projects.")
             '(:name "limit"
               :type integer
               :optional t
               :description "Max files to return (default 100, max 500)")
             '(:name "filter_pattern"
               :type string
               :optional t
               :description "Regex pattern to filter results (e.g., '\\\\.py$', '^src/')"))
 :category "projectile")

(gptel-make-tool
 :name "list_project_directories"
 :function (lambda (directory &optional limit)
             (let ((project-root (projectile-project-root directory)))
               (if (not project-root)
                   (format "No project found at %s. Use list_known_projects to discover available projects." directory)
                 (let* ((dir-limit (min (or limit 50) 200))
                        (all-dirs (projectile-project-dirs project-root))
                        (total-count (length all-dirs))
                        (display-dirs (seq-take all-dirs (min dir-limit total-count)))
                        (result (concat
                                 (format "Project: %s\nTotal directories: %d\nShowing: %d\n\n"
                                         project-root
                                         total-count
                                         (length display-dirs))
                                 (mapconcat #'identity display-dirs "\n")
                                 (when (> total-count dir-limit)
                                   (format "\n\n[Truncated. Increase limit (max 200) to see more directories]")))))
                   (gptel-projectile--result-limit result)))))
 :description "List all directories in project structure.

Returns a list of relative directory paths within the project, respecting ignore rules.
This helps understand the overall structure and organization of the project.

IMPORTANT: You must provide a directory path. Get project paths from list_known_projects first.

Typical workflow:
1. Call list_known_projects() to discover available projects
2. Use a project path from that list with this tool
3. Example: list_project_directories('/Users/username/src/dotfiles', 100)

Use this tool when you need to:
- Understand project directory structure
- Find specific directories (src, tests, docs, etc.)
- Navigate project organization
- Locate where to place new files

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- limit: Max directories to return (default 50, max 200)

Returns directories relative to project root, sorted alphabetically."
 :args (list '(:name "directory"
               :type string
               :description "Project directory path. Use list_known_projects to discover available projects.")
             '(:name "limit"
               :type integer
               :optional t
               :description "Max directories to return (default 50, max 200)"))
 :category "projectile")

(gptel-make-tool
 :name "expand_project_path"
 :function (lambda (relative-path &optional directory)
             (let* ((dir (or directory default-directory))
                    (project-root (projectile-project-root dir)))
               (if (not project-root)
                   (format "Error: No project found at %s" dir)
                 (let ((expanded-path (projectile-expand-root relative-path project-root)))
                   (if (file-exists-p expanded-path)
                       (format "Expanded path: %s (exists)" expanded-path)
                     (format "Expanded path: %s (does not exist - can be created)" expanded-path))))))
 :description "Convert relative path to absolute within project context.

Takes a path relative to the project root and expands it to an absolute path.
This is useful for resolving paths before file operations.

Use this tool when you need to:
- Convert project-relative paths to absolute paths
- Verify path resolution before file operations
- Understand where a relative path points
- Plan file creation at specific locations

Arguments:
- relative_path: Path relative to project root (required)
- directory: Optional project directory context (defaults to current)

Returns the absolute path and indicates whether it exists. If the path doesn't exist,
it can still be used for file creation operations."
 :args (list '(:name "relative_path"
               :type string
               :description "Path relative to project root")
             '(:name "directory"
               :type string
               :optional t
               :description "Project directory context (defaults to current)"))
 :category "projectile")
