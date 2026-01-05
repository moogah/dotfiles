#!/usr/bin/env python3
# pylint: disable=broad-exception-caught,duplicate-code

"""
Claude Code Hook: Elisp Syntax Validator
=========================================
This hook validates Emacs Lisp syntax for Write, Edit and MultiEdit operations.
It uses Emacs' check-parens function to ensure well-formed S-expressions.
Adapted for Jeff's dotfiles repository.
"""

import json
import os
import subprocess
import sys

def find_emacs_executable():
    """Find Emacs executable, trying macOS app bundle first, then PATH."""
    # Try macOS app bundle first
    macos_emacs = "/Applications/Emacs.app/Contents/MacOS/Emacs"
    if os.path.isfile(macos_emacs):
        return macos_emacs
    
    # Try command line emacs
    import shutil
    emacs_path = shutil.which("emacs")
    if emacs_path:
        return emacs_path
    
    return None

def validate_elisp_syntax(content):
    """Validate Elisp syntax using Emacs' check-parens function."""
    emacs_executable = find_emacs_executable()
    if not emacs_executable:
        # If no Emacs found, allow the operation (graceful degradation)
        return True, "Emacs not found - skipping syntax validation"
    
    # Create the Emacs batch command
    emacs_cmd = [
        emacs_executable,
        "-batch",
        "--eval",
        """(progn
 (insert-file-contents "/dev/stdin")
 (emacs-lisp-mode)
 (goto-char (point-min))
 (condition-case err
     (progn
       (check-parens)
       (message "SYNTAX_OK"))
   (error
    (message "SYNTAX_ERROR: %s" err)
    (kill-emacs 1))))""",
    ]

    try:
        result = subprocess.run(
            emacs_cmd,
            input=content,
            capture_output=True,
            text=True,
            timeout=5,
            check=False,
        )

        # Check if syntax is valid
        if result.returncode == 0 and "SYNTAX_OK" in result.stderr:
            return True, None
        
        # Extract error message
        error_msg = result.stderr.strip()
        if "SYNTAX_ERROR:" in error_msg:
            error_msg = error_msg.split("SYNTAX_ERROR:", 1)[1].strip()
        return False, error_msg
    
    except subprocess.TimeoutExpired:
        return False, "Syntax validation timeout (>5s)"
    except Exception as e:
        return False, f"Syntax validation error: {e}"

def apply_edit(content, old_string, new_string, replace_all=False):
    """Apply an edit operation to content."""
    if not old_string:  # File creation
        return new_string
    
    if replace_all:
        return content.replace(old_string, new_string)
    
    # Single replacement
    if old_string in content:
        return content.replace(old_string, new_string, 1)
    
    # String not found
    raise ValueError(f"Old string not found in content: {old_string[:50]}...")

def get_resulting_content(tool_input):
    """Get the content that would result from the tool operation."""
    tool_name = tool_input.get("tool", "")
    parameters = tool_input.get("parameters", {})
    
    if tool_name == "Write":
        return parameters.get("content", "")
    
    elif tool_name in ["Edit", "MultiEdit"]:
        file_path = parameters.get("file_path", "")
        
        # Read current file content
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                current_content = f.read()
        except FileNotFoundError:
            current_content = ""
        except Exception:
            # If we can't read the file, allow the operation
            return None
        
        if tool_name == "Edit":
            # Single edit
            old_string = parameters.get("old_string", "")
            new_string = parameters.get("new_string", "")
            replace_all = parameters.get("replace_all", False)
            
            try:
                return apply_edit(current_content, old_string, new_string, replace_all)
            except ValueError:
                # Edit string not found - let Claude handle the error
                return None
        
        elif tool_name == "MultiEdit":
            # Multiple edits
            edits = parameters.get("edits", [])
            content = current_content
            
            try:
                for edit in edits:
                    old_string = edit.get("old_string", "")
                    new_string = edit.get("new_string", "")
                    replace_all = edit.get("replace_all", False)
                    content = apply_edit(content, old_string, new_string, replace_all)
                return content
            except ValueError:
                # Edit string not found - let Claude handle the error
                return None
    
    return None

def is_elisp_file_in_emacs_dir(file_path):
    """Check if file is an .el file in the emacs directory."""
    if not file_path.endswith('.el'):
        return False
    
    # Normalize path and check if it's in the emacs directory
    abs_path = os.path.abspath(file_path)
    emacs_dir = os.path.abspath(os.path.expanduser('~/src/dotfiles/emacs'))
    
    return abs_path.startswith(emacs_dir)

def main():
    """Main entry point for the hook."""
    try:
        # Read input from stdin
        input_data = sys.stdin.read()
        tool_input = json.loads(input_data)
        
        # Get file path
        parameters = tool_input.get("parameters", {})
        file_path = parameters.get("file_path", "")
        
        # Only validate elisp files in the emacs directory
        if not is_elisp_file_in_emacs_dir(file_path):
            # Allow operation for non-elisp files or files outside emacs dir
            print(json.dumps({"action": "allow"}))
            return
        
        # Get the content that would result from this operation
        resulting_content = get_resulting_content(tool_input)
        
        if resulting_content is None:
            # Couldn't determine resulting content, allow the operation
            print(json.dumps({"action": "allow"}))
            return
        
        # Validate the elisp syntax
        is_valid, error_msg = validate_elisp_syntax(resulting_content)
        
        if is_valid:
            print(json.dumps({"action": "allow"}))
        else:
            print(json.dumps({
                "action": "deny",
                "reason": f"Elisp syntax error: {error_msg}"
            }))
    
    except Exception as e:
        # On any error, allow the operation (graceful degradation)
        print(json.dumps({
            "action": "allow",
            "reason": f"Hook error (allowing operation): {e}"
        }))

if __name__ == "__main__":
    main()